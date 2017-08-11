-module(api).

%keys_new and new_pubkey are the same. one should be removed.

-export([height/0, off/0, balance/0, spend/2, mempool/0,
         top/0, sign/1, mine_block/0, mine_block/2,
         add_peer/2, sync/2, load_key/3, keypair/0, new_keypair/0]).

-export([create_account/2, delete_account/1, account/1,
         repo_account/1, repo_account/2, coinbase/1]).

-export([channel_balance/1,
         new_channel_with_server/3, pull_channel_state/2,
         add_secret/2, pull_channel_state/0, channel_spend/1, channel_spend/3,
         new_channel_tx/6, new_channel_tx/7, close_channel_with_server/0,
         grow_channel/3, grow_channel/4,
         channel_team_close/2, channel_team_close/3, channel_repo/2,
         channel_timeout/0, channel_timeout/2, channel_slash/4, channel_close/0,
         channel_close/2, channel_close/3, new_channel_with_server/7,
         channel_solo_close/0, channel_solo_close/1, channel_solo_close/2, channel_solo_close/4,
         lightning_spend/2, lightning_spend/5, lightning_spend/7, 
         settle_bets/0, market_match/1, trade/5, trade/7,
         dump_channels/0]).

-export([new_difficulty_oracle/2, new_question_oracle/3,
         new_governance_oracle/4, oracle_bet/3, 
         oracle_close/1, oracle_shares/1, 
         oracle_unmatched/2, oracle_unmatched/3,
         dice/1]).

-export([pubkey/0, new_pubkey/1,
         channel_keys/0, keys_status/0, keys_unlock/1, 
         keys_new/1,
         new_market/3, test_oracle_unmatched/0, test/0,
	 channel_manager_update/3]).

%% Described in the docs but not found
%% close_channel/0, new_channel/2, oracle_unmatched/1, sync/0

-define(Fee, free_constants:tx_fee()).
-define(IP, {46,101,103,165}).
-define(Port, 8080).

dump_channels() ->
    channel_manager:dump().
load_key(Pub, Priv, Brainwallet) ->
    keys:load(Pub, Priv, Brainwallet).
height() ->    
    H = headers:height(headers:top()),
    {ok, H}.
%% Q: do we want to return whole header or just hash?
top() ->
    TopHeader = headers:top(),
    Height = headers:height(TopHeader),
    {top, TopHeader, Height}.
    
sign(Tx) ->
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    keys:sign(Tx).
    
tx_maker(F) -> 
    {Trees,_,_} = tx_pool:data(),
    {Tx, _} = F(Trees),
    case keys:sign(Tx) of
	{error, locked} -> 
	    io:fwrite("your password is locked. use `keys:unlock(\"PASSWORD1234\")` to unlock it"),
	    ok;
	Stx -> tx_pool_feeder:absorb(Stx)
    end.
create_account(NewAddr, Amount) ->
    lager:info("Create account"),
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(create_acc_tx, Governance),
    create_account(NewAddr, Amount, ?Fee + Cost).
create_account(NewAddr, Amount, Fee) ->
    tx_maker(
      fun(Trees) ->
              create_account_tx:new(NewAddr, to_int(Amount), Fee, keys:pubkey(), Trees)
      end).

coinbase(ID) ->
    K = keys:pubkey(),
    {Trees, _, _} = tx_pool:data(),
    F = fun(Trees) ->
		coinbase_tx:make(K, Trees) end,
    tx_maker(F).
spend(ID, Amount) ->
    K = keys:pubkey(),
    if 
	ID == K -> io:fwrite("you can't spend money to yourself\n");
	true -> 
	    A = to_int(Amount),
	    {Trees, _, _} = tx_pool:data(),
	    Governance = trees:governance(Trees),
	    Cost = governance:get_value(spend, Governance),
	    spend(ID, A, ?Fee+Cost)
    end.
spend(ID, Amount, Fee) ->
    F = fun(Trees) ->
		spend_tx:make(ID, Amount, Fee, keys:pubkey(), Trees, []) end,
    tx_maker(F).

delete_account(ID) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(delete_acc_tx, Governance),
    delete_account(ID, ?Fee + Cost).
delete_account(ID, Fee) ->
    tx_maker(
      fun(Trees) ->
              delete_account_tx:new(ID, keys:pubkey(), Fee, Trees)
      end).

repo_account(ID) ->   
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(repo, Governance),
    repo_account(ID, ?Fee+Cost).
repo_account(ID, Fee) ->   
    F = fun(Trees) ->
		repo_tx:make(ID, Fee, keys:pubkey(), Trees) end,
    tx_maker(F).

new_channel_tx(CID, Acc2, Bal1, Bal2, Entropy, Delay) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(nc, Governance),
    new_channel_tx(CID, Acc2, Bal1, Bal2, Entropy, ?Fee+Cost, Delay).
new_channel_tx(CID, Acc2, Bal1, Bal2, Entropy, Fee, Delay) ->
    %the delay is how many blocks you have to wait to close the channel if your partner disappears.
    %delay is also how long you have to stop your partner from closing at the wrong state.
    %entropy needs to be a different number every time you make a new channel with the same partner.
    {Trees, _, _} = tx_pool:data(),
    {Tx, _} = new_channel_tx:make(CID, Trees, keys:pubkey(), Acc2, Bal1, Bal2, Entropy, Delay, Fee),
    keys:sign(Tx).
    
new_channel_with_server(Bal1, Bal2, Delay) ->
    {Trees, _, _} = tx_pool:data(),
    Channels = trees:channels(Trees),
    CID = find_id(channels, Channels),
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(nc, Governance),
    new_channel_with_server(?IP, ?Port, CID, Bal1, Bal2, ?Fee+Cost, Delay).
find_id(Name, Tree) ->
    find_id(Name, 1, Tree).
find_id(Name, N, Tree) ->
    case Name:get(N, Tree) of
	{_, empty, _} -> N;
	_ -> find_id(Name, N+1, Tree)
    end.
new_channel_with_server(IP, Port, CID, Bal1, Bal2, Fee, Delay) ->
    Acc1 = keys:pubkey(),
    {ok, Acc2} = talker:talk({pubkey}, IP, Port),
    Entropy = channel_feeder:entropy([Acc1, Acc2]) + 1,
    {Trees,_,_} = tx_pool:data(),
    {Tx, _} = new_channel_tx:make(CID, Trees, Acc1, Acc2, Bal1, Bal2, Entropy, Delay, Fee),
    {ok, ChannelDelay} = application:get_env(ae_core, channel_delay),
    SPK = new_channel_tx:spk(Tx, ChannelDelay),
    Accounts = trees:accounts(Trees),
    STx = keys:sign(Tx),
    SSPK = keys:sign(SPK),
    Msg = {new_channel, STx, SSPK},
    {ok, SSTx, S2SPK} = talker:talk(Msg, IP, Port),
    tx_pool_feeder:absorb(SSTx),
    channel_feeder:new_channel(Tx, S2SPK, Accounts),
    ok.
pull_channel_state() ->
    pull_channel_state(?IP, ?Port).
pull_channel_state(IP, Port) ->
    {ok, ServerID} = talker:talk({pubkey}, IP, Port),
    {ok, CD0} = channel_manager:read(ServerID),
    true = channel_feeder:live(CD0),
    SPKME = channel_feeder:me(CD0),
    {ok, CD, ThemSPK} = talker:talk({spk, keys:pubkey()}, IP, Port),
    Return = channel_feeder:they_simplify(ServerID, ThemSPK, CD),%here
    talker:talk({channel_sync, keys:pubkey(), Return}, IP, Port),
    decrypt_msgs(channel_feeder:emsg(CD)),
    bet_unlock(IP, Port),
    ok.
decrypt_msgs([]) ->
    [];
decrypt_msgs([{msg, _, Msg, _}|T]) ->
    [Msg|decrypt_msgs(T)];
decrypt_msgs([Emsg|T]) ->
    [Secret, Code] = keys:decrypt(Emsg),
    learn_secret(Secret, Code),
    decrypt_msgs(T).

learn_secret(Secret, Code) ->
    secrets:add(Code, Secret).

add_secret(Code, Secret) ->
    ok = pull_channel_state(?IP, ?Port),
    secrets:add(Code, Secret),
    ok = bet_unlock(?IP, ?Port).

bet_unlock(IP, Port) ->
    {ok, ServerID} = talker:talk({pubkey}, IP, Port),
    [{Secrets, _SPK}] = channel_feeder:bets_unlock([ServerID]),
    lager:info("Teach secrets"),
    teach_secrets(keys:pubkey(), Secrets, IP, Port),
    {ok, _CD, ThemSPK} = talker:talk({spk, keys:pubkey()}, IP, Port),
    channel_feeder:update_to_me(ThemSPK, ServerID),
    ok.
teach_secrets(_, [], _, _) -> ok;
teach_secrets(ID, [{secret, Secret, Code}|Secrets], IP, Port) ->
    talker:talk({learn_secret, ID, Secret, Code}, IP, Port),
    teach_secrets(ID, Secrets, IP, Port).
channel_spend(Amount) ->
    channel_spend(?IP, ?Port, Amount).
channel_spend(IP, Port, Amount) ->
    {ok, PeerId} = talker:talk({pubkey}, IP, Port),
    {ok, CD} = channel_manager:read(PeerId),
    OldSPK = testnet_sign:data(channel_feeder:them(CD)),
    ID = keys:pubkey(),
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    SPK = spk:get_paid(OldSPK, ID, -Amount), 
    Payment = keys:sign(SPK),
    M = {channel_payment, Payment, Amount},
    {ok, Response} = talker:talk(M, IP, Port),
    channel_feeder:spend(Response, -Amount),
    ok.
lightning_spend(Pubkey, Amount) ->
    {ok, LFee} = application:get_env(ae_core, lightning_fee),
    lightning_spend(?IP, ?Port, Pubkey, Amount, LFee).
lightning_spend(IP, Port, Pubkey, Amount, Fee) ->
    {Code, SS} = secrets:new_lightning(),
    lightning_spend(IP, Port, Pubkey, Amount, Fee, Code, SS).
lightning_spend(IP, Port, Pubkey, Amount, Fee, Code, SS) ->
    {ok, ServerID} = talker:talk({pubkey}, IP, Port),
    %ChannelID,
    ESS = keys:encrypt([SS, Code], Pubkey),
    SSPK = channel_feeder:make_locked_payment(ServerID, Amount+Fee, Code, []),
    {ok, SSPK2} = talker:talk({locked_payment, SSPK, Amount, Fee, Code, keys:pubkey(), Pubkey, ESS}, IP, Port),
    {Trees, _, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    true = testnet_sign:verify(keys:sign(SSPK2)),
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(SSPK2),
    channel_manager_update(ServerID, SSPK2, <<>>),
    ok.
channel_manager_update(ServerID, SSPK2, DefaultSS) ->
    %store SSPK2 in channel manager, it is their most recent signature.
    {ok, CD} = channel_manager:read(ServerID),
    CID = channel_feeder:cid(CD),
    Entropy = channel_feeder:entropy(CD),
    ThemSS = channel_feeder:script_sig_them(CD),
    MeSS = channel_feeder:script_sig_me(CD),
    SPK = testnet_sign:data(SSPK2),
    NewCD = channel_feeder:new_cd(SPK, SSPK2, [DefaultSS|MeSS], [DefaultSS|ThemSS], Entropy, CID),
    channel_manager:write(ServerID, NewCD),
    ok.

channel_balance(OtherPubKey) ->
    {ok, CD} = channel_manager:read(OtherPubKey),
    SSPK = channel_feeder:them(CD),
    SPK = testnet_sign:data(SSPK),
    SS = channel_feeder:script_sig_them(CD),
    {Trees, NewHeight, _Txs} = tx_pool:data(),
    Channels = trees:channels(Trees),
    {Amount, _, _, _} = spk:run(fast, SS, SPK, NewHeight, 0, Trees),
    CID = spk:cid(SPK),
    {_, Channel, _} = channels:get(CID, Channels),
    channels:bal1(Channel)-Amount.

pretty_display(I) ->
    {ok, TokenDecimals} = application:get_env(ae_core, token_decimals),
    F = I / TokenDecimals,
    [Formatted] = io_lib:format("~.8f", [F]),
    Formatted.

dice(Amount) ->
    unlocked = keys:status(),
    A = to_int(Amount),
    internal_handler:doit({dice, A, constants:server_ip(), constants:server_port()}).
close_channel_with_server() ->
    internal_handler:doit({close_channel, constants:server_ip(), constants:server_port()}).

to_int(X) ->
    {ok, TokenDecimals} = application:get_env(ae_core, token_decimals),
    round(X * TokenDecimals).

grow_channel(CID, Bal1, Bal2) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(gc, Governance),
    grow_channel(CID, Bal1, Bal2, ?Fee+Cost).
grow_channel(CID, Bal1, Bal2, Fee) ->
    {Trees, _, _} = tx_pool:data(),
    {Tx, _} = grow_channel_tx:make(CID, Trees, to_int(Bal1), to_int(Bal2), Fee),
    keys:sign(Tx).

channel_team_close(CID, Amount) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(ctc, Governance),
    channel_team_close(CID, Amount, ?Fee+Cost).
channel_team_close(CID, Amount, Fee) ->
    {Trees, _, _} = tx_pool:data(),
    keys:sign(channel_team_close_tx:make(CID, Trees, Amount, [], Fee)).

channel_repo(CID, Fee) ->
    F = fun(Trees) ->
		channel_repo_tx:make(keys:pubkey(), CID, Fee, Trees) end,
    tx_maker(F).

channel_timeout() ->
    %% Why prod address?
    channel_timeout(constants:server_ip(), constants:server_port()).

channel_timeout(Ip, Port) ->
    {ok, Other} = talker:talk({pubkey}, Ip, Port),
    Fee = free_constants:tx_fee(),
    {Trees,_,_} = tx_pool:data(),
    {ok, CD} = channel_manager:read(Other),
    CID = channel_feeder:cid(CD),
    {Tx, _} = channel_timeout_tx:make(keys:pubkey(), Trees, CID, [], Fee),
    case keys:sign(Tx) of
        {error, locked} ->
            lager:error("Your password is locked");
        Stx ->
            tx_pool_feeder:absorb(Stx)
    end.

channel_slash(_CID, Fee, SPK, SS) ->
    F = fun(Trees) ->
		channel_slash_tx:make(keys:pubkey(), Fee, SPK, SS, Trees) end,
    tx_maker(F).
new_question_oracle(Start, Question, DiffOracleID)->
    {Trees, _, _} = tx_pool:data(),
    Oracles = trees:oracles(Trees),
    ID = find_id(oracles, Oracles),
    new_question_oracle(Start, Question, DiffOracleID, ID).
new_question_oracle(Start, Question, DiffOracleID, ID)->
    {Trees, _, _} = tx_pool:data(),
    Oracles = trees:oracles(Trees),
    {_, Recent, _} = oracles:get(DiffOracleID, Oracles),
    Difficulty = oracles:difficulty(Recent) div 2,
    Governance = trees:governance(Trees),
    Cost = governance:get_value(oracle_new, Governance),
    F = fun(Trs) ->
		oracle_new_tx:make(keys:pubkey(), ?Fee+Cost, Question, Start, ID, Difficulty, DiffOracleID, 0, 0, Trs) end,
    tx_maker(F).
new_difficulty_oracle(Start, Difficulty) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(oracle_new, Governance),
    Oracles = trees:oracles(Trees),
    ID = find_id(oracles, Oracles),
    new_difficulty_oracle(?Fee+Cost, Start, ID, Difficulty).
new_difficulty_oracle(Fee, Start, ID, Difficulty) ->
    %used to measure the difficulty at which negative and positive shares are worth the same
    F = fun(Trees) ->
		oracle_new_tx:make(keys:pubkey(), Fee, <<"">>, Start, ID, Difficulty, 0, 0, 0, Trees) end,
    tx_maker(F).
new_governance_oracle(Start, GovName, GovAmount, DiffOracleID) ->
    GovNumber = governance:name2number(GovName),
    F = fun(Trs) ->
		Oracles = trees:oracles(Trs),
		ID = find_id(oracles, Oracles),
		{_, Recent, _} = oracles:get(DiffOracleID, Oracles),
		Difficulty = oracles:difficulty(Recent) div 2,
		Governance = trees:governance(Trs),
		Cost = governance:get_value(oracle_new, Governance),
		oracle_new_tx:make(keys:pubkey(), ?Fee + Cost, <<>>, Start, ID, Difficulty, DiffOracleID, GovNumber, GovAmount, Trs) end,
    tx_maker(F).
    
oracle_bet(OID, Type, Amount) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(oracle_bet, Governance),
    oracle_bet(?Fee+Cost, OID, Type, Amount).
oracle_bet(Fee, OID, Type, Amount) ->
    F = fun(Trees) ->
		oracle_bet_tx:make(keys:pubkey(), Fee, OID, Type, to_int(Amount), Trees)
	end,
    tx_maker(F).
oracle_close(OID) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(oracle_close, Governance),
    oracle_close(?Fee+Cost, OID).
oracle_close(Fee, OID) ->
    F = fun(Trees) ->
		oracle_close_tx:make(keys:pubkey(), Fee, OID, Trees)
	end,
    tx_maker(F).
oracle_shares(OID) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(oracle_shares, Governance),
    oracle_shares(?Fee+Cost, OID).
oracle_shares(Fee, OID) ->
    F = fun(Trees) ->
		oracle_shares_tx:make(keys:pubkey(), Fee, OID, Trees)
	end,
    tx_maker(F).
oracle_unmatched(OracleID, OrderID) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(unmatched, Governance),
    oracle_unmatched(?Fee+Cost, OracleID, OrderID).
oracle_unmatched(Fee, OracleID, OrderID) ->
    F = fun(Trees) ->
		oracle_unmatched_tx:make(keys:pubkey(), Fee, OracleID, OrderID, Trees)
	end,
    tx_maker(F).

account(Pubkey) ->
    {Trees,_,_} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    case accounts:get(Pubkey, Accounts) of
        {_,empty,_} -> empty;
        {_, A, _} -> A
    end.
account() -> account(keys:pubkey()).
integer_balance() -> accounts:balance(account()).
balance() ->
    I = case keys:pubkey() of
	    -1 -> 0;
	    _ -> integer_balance()
	end,
    I.

mempool() ->
    {_, _, Txs} = tx_pool:data(),
    Txs.
off() ->
    ok = application:stop(ae_core).
mine_block() ->
    block:mine(1, 100000).
mine_block(0, Times) -> ok;
mine_block(Periods, Times) ->
    PB = block:top(),
    Top = block:block_to_header(PB),
    {_, _, Txs} = tx_pool:data(),
    Block = block:make(Top, Txs, block:trees(PB), keys:pubkey()),
    block:mine(Block, Times),
    timer:sleep(100),
    mine_block(Periods-1, Times).
channel_close() ->
    channel_close(?IP, ?Port).
channel_close(IP, Port) ->
    {Trees, _, _} = tx_pool:data(),
    Governance = trees:governance(Trees),
    Cost = governance:get_value(ctc, Governance),
    channel_close(IP, Port, ?Fee+Cost).
channel_close(IP, Port, Fee) ->
    {ok, PeerId} = talker:talk({pubkey}, IP, Port),
    {ok, CD} = channel_manager:read(PeerId),
    SPK = testnet_sign:data(channel_feeder:them(CD)),
    {Trees,_,_} = tx_pool:data(),
    Height = block:height(block:read(headers:top())),
    SS = channel_feeder:script_sig_them(CD),
    {Amount, _, _, _} = spk:run(fast, SS, SPK, Height, 0, Trees),
    CID = spk:cid(SPK),
    {Tx, _} = channel_team_close_tx:make(CID, Trees, Amount, [], Fee),
    STx = keys:sign(Tx),
    {ok, SSTx} = talker:talk({close_channel, CID, keys:pubkey(), SS, STx}, IP, Port),
    tx_pool_feeder:absorb(SSTx),
    0.

channel_solo_close() ->
    %% Why prod address?
    channel_solo_close({127,0,0,1}, 3010).

channel_solo_close(IP, Port) ->
    {ok, Other} = talker:talk({pubkey}, IP, Port),
    channel_solo_close(Other).

channel_solo_close(Other) ->
    Fee = free_constants:tx_fee(),
    {Trees,_,_} = tx_pool:data(),
    {ok, CD} = channel_manager:read(Other),
    SSPK = channel_feeder:them(CD),
    SS = channel_feeder:script_sig_them(CD),
    {Tx, _} = channel_solo_close:make(keys:pubkey(), Fee, keys:sign(SSPK), SS, Trees),
    STx = keys:sign(Tx),
    tx_pool_feeder:absorb(STx),
    ok.

channel_solo_close(_CID, Fee, SPK, ScriptSig) ->
    F = fun(Trees) ->
		channel_solo_close:make(keys:pubkey(), Fee, SPK, ScriptSig, Trees) end,
    tx_maker(F).

add_peer(IP, Port) ->
    peers:add({IP, Port}),
    0.
sync(IP, Port) ->
    lager:info("Sync with ~p ~p ~n", [IP, Port]),
    MyHeight = headers:height(headers:top()),
    ok = download_blocks:sync_all([{IP, Port}], MyHeight).

keypair() ->
    keys:keypair().
pubkey() ->
    keys:pubkey().
new_pubkey(Password) ->    
    keys:new(Password).
new_keypair() ->
    testnet_sign:new_key().
test() ->
    {test_response}.
channel_keys() ->
    channel_manager:keys().
keys_status() ->
    list_to_binary(atom_to_list(keys:status())).
keys_unlock(Password) ->
    keys:unlock(Password),
    0.
keys_new(Password) ->
    keys:new(Password),
    0.
market_match(OID) ->
    %check that we haven't matched too recently. (otherwise we lose all our money in all the channels.)
    {PriceDeclaration, _Accounts} = order_book:match(OID),
    %CodeKey = market:market_smart_contract_key(OID, Expires, keys:pubkey(), Period, OID),

    %false = Accounts == [],
    OB = order_book:data(OID),
    Expires = order_book:expires(OB),
    Period = order_book:period(OB),
    CodeKey = market:market_smart_contract_key(OID, Expires, keys:pubkey(), Period, OID),
    SS = market:settle(PriceDeclaration),
    secrets:add(CodeKey, SS),
    %channel_feeder:bets_unlock(Accounts),
    channel_feeder:bets_unlock(channel_manager:keys()),
    %io:fwrite(packer:pack({api_market_match, Accounts, SS})),
    
    %channel_feeder:market_ss_me(Accounts),
    %channel_feeder:market_ss_me(channel_manager:keys()),
    %add this to channels_manager ss_me for every bet in the channel that participated.
    {ok, ok}.
settle_bets() ->
    channel_feeder:bets_unlock(channel_manager:keys()),
    {ok, ok}.
new_market(OID, Expires, Period) -> 
    %for now lets use the oracle id as the market id. this wont work for combinatorial markets.
    order_book:new_market(OID, Expires, Period).
    %set up an order book.
    %turn on the api for betting.
trade(Price, Type, Amount, OID, Fee) ->
    trade(Price, Type, Amount, OID, Fee, ?IP, ?Port).
trade(Price, Type, A, OID, Fee, IP, Port) ->
    Amount = to_int(A),
    {ok, ServerID} = talker:talk({pubkey}, IP, Port),
    {ok, {Expires, 
	  Pubkey, %pubkey of market maker
	  Period}} = 
	talker:talk({market_data, OID}, IP, Port),
    BetLocation = constants:oracle_bet(),
    MarketID = OID,
    %type is true or false or one other thing...
    SC = market:market_smart_contract(BetLocation, MarketID, Type, Expires, Price, Pubkey, Period, Amount, OID),
    SSPK = channel_feeder:trade(Amount, SC, ServerID, OID),
    {ok, SSPK2} =
	talker:talk({trade, 
		     keys:pubkey(),
		     Price,
		     Type,
		     Amount,
		     OID,
		     SSPK, 
		     Fee}, IP, Port),
    SPK = testnet_sign:data(SSPK),
    SPK = testnet_sign:data(SSPK2),
    channel_manager_update(ServerID, SSPK2, market:unmatched()),
    ok.
    

%mine() ->
%    mine:start().
    %mine(10000000000).
%mine(N) -> 
    %sync(),
    %block:mine_blocks(N, 100000, 30). 
%second number is how many nonces we try per round.
%first number is how many rounds we do.
test_oracle_unmatched() ->
    %create_account(Address, 10, 2),
    %delete_account(2),
    {Pub,Priv} = testnet_sign:new_key(),
    {Pub2,Priv2} = testnet_sign:new_key(),
    create_account(Pub, 0.0000001),
    timer:sleep(100),
    test_txs:mine_blocks(Pub),
    repo_account(Pub2),
    timer:sleep(100),
    create_account(Pub2, 10),
    timer:sleep(100),
    spend(Pub2, Pub),
    timer:sleep(100),
    {Trees, _, _} = tx_pool:data(),
    Accounts = trees:accounts(Trees),
    Ctx = new_channel_tx(1, 2, 1, 1, 1, 1),
    Stx = testnet_sign:sign_tx(Ctx, Pub, Priv, Accounts),
    tx_pool_feeder:absorb(Stx),
    timer:sleep(100),
    Ctx2 = grow_channel(1, 0.1, 0.1),
    Stx2 = testnet_sign:sign_tx(Ctx2, Pub, Priv, Accounts),
    tx_pool_feeder:absorb(Stx2),
    test_txs:mine_blocks(1),
    timer:sleep(100),
    {_Trees, Height, _} = tx_pool:data(),
    Difficulty = constants:initial_difficulty(),
    new_difficulty_oracle(Height+1, Difficulty),
    timer:sleep(100),
    test_txs:mine_blocks(2),
    timer:sleep(100),
    oracle_bet(1, bad, 10.0),
    timer:sleep(100),
    test_txs:mine_blocks(2),
    oracle_close(1),
    timer:sleep(100),
    oracle_shares(1),
    timer:sleep(100),
    oracle_unmatched(1, 2),
    timer:sleep(100),
    tx_pool:data().
    
    
