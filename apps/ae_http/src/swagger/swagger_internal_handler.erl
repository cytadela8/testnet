%% basic handler
-module(swagger_internal_handler).

%% Cowboy REST callbacks
-export([allowed_methods/2]).
-export([init/3]).
-export([rest_init/2]).
-export([allow_missing_post/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([known_content_type/2]).
-export([malformed_request/2]).
-export([valid_content_headers/2]).
-export([valid_entity_length/2]).

%% Handlers
-export([handle_request_json/2]).

-record(state, {
    operation_id :: swagger_api:operation_id(),
    logic_handler :: atom(),
    validator_state :: jesse_state:state(),
    context=#{} :: #{}
}).

-type state() :: state().

-spec init(TransportName :: atom(), Req :: cowboy_req:req(), Opts :: swagger_router:init_opts()) ->
    {upgrade, protocol, cowboy_rest, Req :: cowboy_req:req(), Opts :: swagger_router:init_opts()}.

init(_Transport, Req, Opts) ->
    {upgrade, protocol, cowboy_rest, Req, Opts}.

-spec rest_init(Req :: cowboy_req:req(), Opts :: swagger_router:init_opts()) ->
    {ok, Req :: cowboy_req:req(), State :: state()}.

rest_init(Req0, {Operations, LogicHandler, ValidatorState}) ->
    {Method, Req} = cowboy_req:method(Req0),
    OperationID = maps:get(Method, Operations, undefined),

    error_logger:info_msg("Attempt to process operation: ~p", [OperationID]),

    State = #state{
        operation_id = OperationID,
        logic_handler = LogicHandler,
        validator_state = ValidatorState
    },
    {ok, Req, State}.

-spec allowed_methods(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: [binary()], Req :: cowboy_req:req(), State :: state()}.


allowed_methods(
    Req,
    State = #state{
        operation_id = 'AddAccount'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'AddPeer'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'AddSecret'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'ChannelBalance'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'ChannelSoloClose'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'ChannelSpend'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'ChannelTimeout'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'CreateKeyPair'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'DeleteAccount'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'FetchAccount'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'FetchKeyPair'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'FetchPubKey'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetBalance'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetHeight'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetTop'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'LightningSpend'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'MineBlock'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'NewChannelWithServer'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'PullChannelState'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'RepoAccount'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'SetKeyPair'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'Spend'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'Sync'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'TurnOff'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: true | {false, AuthHeader :: iodata()},
        Req :: cowboy_req:req(),
        State :: state()
    }.

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {true, Req, State};

is_authorized(Req, State) ->
    {{false, <<"">>}, Req, State}.

-spec content_types_accepted(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{binary(), AcceptResource :: atom()}],
        Req :: cowboy_req:req(),
        State :: state()
    }.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, handle_request_json}
    ], Req, State}.

-spec valid_content_headers(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'AddAccount'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'AddPeer'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'AddSecret'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'ChannelBalance'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'ChannelSoloClose'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'ChannelSpend'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'ChannelTimeout'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'CreateKeyPair'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'DeleteAccount'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'FetchAccount'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'FetchKeyPair'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'FetchPubKey'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetBalance'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetHeight'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetTop'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'LightningSpend'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'MineBlock'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'NewChannelWithServer'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'PullChannelState'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'RepoAccount'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'SetKeyPair'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'Spend'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'Sync'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'TurnOff'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{binary(), ProvideResource :: atom()}],
        Req :: cowboy_req:req(),
        State :: state()
    }.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_request_json}
    ], Req, State}.

-spec malformed_request(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: false, Req :: cowboy_req:req(), State :: state()}.

malformed_request(Req, State) ->
    {false, Req, State}.

-spec allow_missing_post(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: false, Req :: cowboy_req:req(), State :: state()}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

-spec delete_resource(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

delete_resource(Req, State) ->
    handle_request_json(Req, State).

-spec known_content_type(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

known_content_type(Req, State) ->
    {true, Req, State}.

-spec valid_entity_length(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

valid_entity_length(Req, State) ->
    %% @TODO check the length
    {true, Req, State}.

%%%%

-type result_ok() :: {
    ok,
    {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: iodata()}
}.

-type result_error() :: {error, Reason :: any()}.

-type processed_response() :: {halt, cowboy_req:req(), state()}.

-spec process_response(result_ok() | result_error(), cowboy_req:req(), state()) ->
    processed_response().

process_response(Response, Req0, State = #state{operation_id = OperationID}) ->
    case Response of
        {ok, {Code, Headers, Body}} ->
            {ok, Req} = cowboy_req:reply(Code, Headers, Body, Req0),
            {halt, Req, State};
        {error, Message} ->
            error_logger:error_msg("Unable to process request for ~p: ~p", [OperationID, Message]),

            {ok, Req} = cowboy_req:reply(400, Req0),
            {halt, Req, State}
    end.

-spec handle_request_json(cowboy_req:req(), state()) -> {halt, cowboy_req:req(), state()}.

handle_request_json(
    Req0,
    State = #state{
        operation_id = OperationID,
        logic_handler = LogicHandler,
        validator_state = ValidatorState,
        context = Context
    }
) ->
    case swagger_api:populate_request(OperationID, Req0, ValidatorState) of
        {ok, Populated, Req1} ->
            {Code, Headers, Body} = swagger_logic_handler:handle_request(
                LogicHandler,
                OperationID,
                Populated,
                Context
            ),
            _ = swagger_api:validate_response(
                OperationID,
                Code,
                Body,
                ValidatorState
            ),
            PreparedBody = jsx:encode(Body),
            Response = {ok, {Code, Headers, PreparedBody}},
            process_response(Response, Req1, State);
        {error, Reason, Req1} ->
            process_response({error, Reason}, Req1, State)
    end.

validate_headers(_, Req) -> {true, Req}.
