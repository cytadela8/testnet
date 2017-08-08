-module(download_blocks).
-export([sync_all/2, sync/2, do_send_blocks/2]).

%% TODO: introduce ranking mechanism. It may be based on initial top request.
%%       The quickest and the longest chains are promoted
%%       There is no point of quering thousands of peers and updating our chain at once from all of them

sync_all([], _) -> ok;
sync_all([Peer|T], Height) ->
    spawn(fun() ->
                  sync(Peer, Height)
          end),
    sync_all(T, Height).

sync(Peer, MyHeight) ->
    lager:info("Syncing with peer ~p", [Peer]),
    TopResp = talker:talk({top}, Peer),
    case TopResp of
        {error, failed_connect} -> 
            lager:warning("failed connect"),
            ok;
        {ok, TopBlock, Height}  ->
            {ok, DBB} = application:get_env(ae_core, download_blocks_batch),
            HH = MyHeight + DBB,
            case HH < Height of
                true ->
                    lager:info("We are more then ~p blocks behind", [DBB]),
                    {ok, Sizecap} = application:get_env(ae_core, download_blocks_sizecap),
                    {ok, Blocks} = talker:talk({block_sizecap, HH, Sizecap}, Peer),
                    trade_blocks(Peer, Blocks);
                false ->
                    trade_blocks(Peer, [TopBlock])
            end,
            get_txs(Peer),
            trade_peers(Peer)
    end.

trade_blocks(Peer, [PrevBlock|PBT] = Blocks) ->
    %"NextHash" is from earlier in the chain than PrevBlock. we are walking backwards
    NextHash = block:prev_hash(PrevBlock),
    PrevBlockIsGenesis = block:height(PrevBlock) =< 0,
    M = block:get_by_hash(NextHash),%check if it is in our memory already.
    if
        M =/= empty; PrevBlockIsGenesis ->
            LastCommonHash = last_known_block_hash(NextHash, Blocks),
            %We send blocks before absorbing to make sure we don't send any downloaded blocks
            send_blocks(Peer, block:hash(block:top()), LastCommonHash, [], block:height(block:get_by_hash(LastCommonHash))),
            NewBlocks = remove_known_blocks(Blocks),
            block_absorber:enqueue(NewBlocks);
        M == empty ->
            false = PrevBlockIsGenesis,
            {ok, Sizecap} = application:get_env(ae_core, download_blocks_sizecap),
            {ok, NextBatch} = talker:talk({block_sizecap, NextHash, Sizecap}, Peer),
            %Heighest first, lowest last. We need to reverse
            trade_blocks(Peer, lists:append(lists:reverse(NextBatch), Blocks))
    end.

remove_known_blocks([]) ->
    [];
remove_known_blocks([PrevBlock | PBT]) ->
    PrevHash = block:hash(PrevBlock),
    case block:get_by_hash(PrevHash) of 
        empty ->
            [PrevBlock | PBT];
        _ ->
            remove_known_blocks(PBT)
    end.

last_known_block_hash(Hash,[]) ->
    Hash;
last_known_block_hash(Hash,[First | Other]) ->
    HashFirst = block:hash(First),
    case block:get_by_hash(HashFirst) of
        empty ->
            Hash;
        _ ->
            last_known_block_hash(HashFirst,Other)
    end.

send_blocks(Peer, T, T, L, _) -> 
    send_blocks_external(Peer, L);
send_blocks(Peer, TopHash, CommonHash, L, CommonHeight) ->
    Block = block:get_by_hash(TopHash),
    PrevHash = block:prev_hash(Block),
    case block:height(Block) of
        CommonHeight -> %if we realize, we are on diffrent fork then the peer
            BlockCommon = block:get_by_hash(CommonHash),
            NewCommon = block:prev_hash(BlockCommon),
            send_blocks(Peer, PrevHash, NewCommon, [Block|L], CommonHeight-1); %we send one more block till we find one common with our main chain and the fork
        _ ->
            send_blocks(Peer, PrevHash, CommonHash, [Block|L], CommonHeight)
    end.

send_blocks_external(Peer, Blocks) ->
    lager:debug("send_blocks_external: ~p" ,[packer:pack({sending_blocks, Blocks})]),
    spawn(?MODULE, do_send_blocks, [Peer, Blocks]).

do_send_blocks(_, []) -> ok;
do_send_blocks(Peer, [Block|T]) ->
    remote_peer({give_block, Block}, Peer),
    timer:sleep(20),
    do_send_blocks(Peer, T).

get_txs(Peer) ->
    Txs = remote_peer({txs}, Peer),
    tx_pool_feeder:absorb(Txs),
    {_,_,Mine} = tx_pool:data(),
    remote_peer({txs, Mine}, Peer).

trade_peers(Peer) ->
    TheirsPeers = remote_peer({peers}, Peer),
    MyPeers = ae_utils:tuples2lists(peers:all()),
    remote_peer({peers, MyPeers}, Peer),
    peers:add(TheirsPeers).

remote_peer(Transaction, Peer) ->
    case talker:talk(Transaction, Peer) of
        {ok, Return0} -> Return0;
        Return1 -> Return1
    end.
