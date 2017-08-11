
-module(push_block).
-behaviour(supervisor).
-export([start_link/0, init/1]).

%% API
-export([
         push_start/1  %%starts whole process, takes one Block
]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, ok).
init(ok) ->
    {ok, {{one_for_one, 0, 1}, []}}.

push_start(Block) ->
    lager:debug("Starting gossip"),
    BlockHash = block:hash(Block),
    supervisor:start_child(?MODULE, #{id => BlockHash,
                                    start => {push_block_worker, start_link, [Block]},
                                    restart => temporary,
                                    shutdown => 1000,
                                    type => worker,
                                    modules => [push_block_worker]}).
