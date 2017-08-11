
-module(push_block_worker).
-behaviour(gen_server).
%% API
-export([start_link/2,  %%Start push process. To be called from supervisor
         status/1,
         stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-record(state, {num_left,  %%number of "known" responses left to shutdown
                blockserialized,
                peers}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link(Name, BlockSerialized) ->
    Resp = {ok, Pid} = gen_server:start_link({global, {?MODULE, Name}} ,?MODULE, [BlockSerialized], []),
    push(Pid, gossip_process_count()),
    Resp.

init([BlockSerialized]) ->
    {ok, #state{num_left=gossip_stop_count(),
                blockserialized=BlockSerialized,
                peers=shuffle(peers:all())}}.

handle_call(status, _From, X) -> {reply, X, X};
handle_call(stop, _From, X) -> {stop, normal, ok, X};
handle_call(_, _From, X) -> {reply, X, X}.

handle_cast(_, State=#state{peers=[]}) ->
    lager:info("Gossip finished reason: no peers left"),
    {stop, normal, State};
handle_cast(_, State=#state{num_left=0}) ->
    lager:error("Gossip finished, but this shouldn't happen this way"),
    {stop, normal, State};
handle_cast(known, State=#state{num_left=1}) ->
    lager:info("Gossip finished reason: known limit"),
    {stop, normal, State};
handle_cast(known, State) ->  %invoked when a peer responses with known
    {noreply, State#state{num_left = State#state.num_left - 1}};
handle_cast(unknown, State) ->  %invoked when a peer responses with unknown or any problem occures
    push(self(), gossip_stop_count() - State#state.num_left + 1),
    {noreply, State#state{num_left = gossip_stop_count()}};
handle_cast(push, State) ->  %Starts new process that sends the block to one peer
    [Peer | RemainingPeers] = State#state.peers,
    Pid = self(),
    spawn(fun() ->
        lager:debug("Gossiping to peer ~p", [Peer]),
        Resp = talker:talk({give_new_block, State#state.blockserialized}, Peer),
        case Resp of
            {known} ->
                lager:debug("got known\n"),
                gen_server:cast(Pid, known);
            {unknown} ->
                lager:debug("got unknown\n"),
                gen_server:cast(Pid, unknown);
            _ ->
                lager:debug("got unexpected: ~p\n", [Resp]),
                gen_server:cast(Pid, unknown)
            end
        end),
    {noreply, State#state{peers=RemainingPeers}}.

handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
push(_, 0) ->
    ok;
push(Pid, N) ->
    push(Pid),
    push(Pid, N-1).
push(Pid) ->
    gen_server:cast(Pid, push).

stop(Name) ->
    gen_server:call({global, {?MODULE, Name}}, stop).

gossip_stop_count() ->  %Number of consequent "known" responses to shutdown.
    {ok, N} = application:get_env(ae_core, push_block_gossip_stop_count),
    N.

gossip_process_count() ->  %Number of concurrent push requests.
    {ok, N} = application:get_env(ae_core, push_block_gossip_process_count),
    N.

status(Name) ->
    gen_server:call({global, {?MODULE, Name}}, status).

shuffle(L) ->
    [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].
