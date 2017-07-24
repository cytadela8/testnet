-module(block_absorber).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
	 handle_cast/2,handle_info/2,init/1,terminate/2]).

%% API
-export([
	 enqueue/1, %% async request
         enqueue_and_push/1,
	 save/1,    %% returs after saving
	 garbage/0,
	 do_save/1
]).

init(ok) -> 
    {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> lager:debug("terminating block_absorber gen_srv"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(garbage, X) -> 
    trees:garbage(),
    {noreply, X};
handle_cast({doit, BP}, X) ->
    absorb_internal(BP),
    {noreply, X};
handle_cast({doit_and_push, BP}, X) ->
    case absorb_internal(BP) of
        ok -> ok;
        _ ->
            push_block:push_start(BP)
    end,
    {noreply, X}.
handle_call({doit, BP}, _From, X) -> 
    absorb_internal(BP),
    {reply, ok, X}.
%handle_call(_, _From, X) -> {reply, X, X}.
garbage() ->
    gen_server:cast(?MODULE, garbage).

enqueue(InputBlocks) when is_list(InputBlocks) ->
    [enqueue(InputBlock) || InputBlock <- InputBlocks];
enqueue(InputBlock) ->
    headers:absorb([block:block_to_header(InputBlock)]),
    gen_server:cast(?MODULE, {doit, InputBlock}).

enqueue_and_push(InputBlock) ->
    headers:absorb([block:block_to_header(InputBlock)]),
    gen_server:cast(?MODULE, {doit_and_push, InputBlock}).

save(InputBlocks) when is_list(InputBlocks) ->
    [save(InputBlock) || InputBlock <- InputBlocks];
save(InputBlock) ->
    gen_server:call(?MODULE, {doit, InputBlock}).

    
absorb_internal(Block) ->
    BH = block:hash(Block),
    NextBlock = block:prev_hash(Block),
    case block_hashes:check(BH) of
	true -> 
	    io:fwrite("we have seen this block before, so block_absorber will ignore it\n"),
	    ok;%If we have seen this block before, then don't process it again.
	false ->
	    true = block_hashes:check(NextBlock), %check that the previous block is known.
	    false = empty == block:read(NextBlock), %check that previous block was valid
	    block_hashes:add(BH),%Don't waste time checking invalid blocks more than once.
	    Header = block:block_to_header(Block),
	    headers:absorb([Header]),
	    {true, Block2} = block:check(Block),
	    do_save(Block2),
	    BH = block:hash(Block2),
	    timer:sleep(100),
	    {_, _, Txs} = tx_pool:data(),
	    tx_pool:dump(),
	    tx_pool_feeder:absorb(Txs)
    end.   
save_helper(BlockPlus) ->
    Z = zlib:compress(term_to_binary(BlockPlus)),
    binary_to_term(zlib:uncompress(Z)),%sanity check, not important for long-term.
    %Hash = testnet_hasher:doit(BlockPlus),
    Hash = block:hash(BlockPlus),
    BF = block:binary_to_file(Hash),
    ok = db:save(BF, Z).
