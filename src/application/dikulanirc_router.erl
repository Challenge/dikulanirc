%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc Module used for communicating and routing requests between
%%    the server and the given bot.
%% -------------------------------------------------------------------
-module(dikulanirc_router).
-behaviour(gen_server).
-include_lib("db_records.hrl").
-include_lib("irc_constants.hrl").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State definition
-record(state, {bot, processor, sock, socket_type}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Bot, IrcRouterId, BotProcessorId) ->
    error_logger:info_msg("[~s] Starting IRC router for bot ~w~n", [?MODULE, Bot#dikulanirc_bots.bot_id]),
    gen_server:start_link({global, IrcRouterId}, ?MODULE, [Bot, BotProcessorId], []).

%%-----------------------------------------------------------------------------
%% @spec init(State) -> {ok,State} | {ok,State,Timeout} | {ok,State,hibernate}
%%    | {stop,Reason} | ignore
%% @doc Whenever a gen_server is started using gen_server:start/3,4 or 
%% gen_server:start_link/3,4, this function is called by the new process to initialize.
%%-----------------------------------------------------------------------------
init([Bot, BotProcessorId]) ->
    error_logger:info_msg("[~s] Initialing IRC router~n", [?MODULE]),
    case connect(Bot#dikulanirc_bots.network) of
        {ok, {Sock, SocketType}} ->
            error_logger:info_msg("[~s] Sending connected event to bot ~w~n", [?MODULE, Bot#dikulanirc_bots.bot_id]),
            gen_fsm:send_event({global, BotProcessorId}, ?IRC_CONNECTED),
            State = #state{bot = Bot, processor = BotProcessorId, sock = Sock, socket_type = SocketType},
            {ok, State};
        _ ->
            {stop, could_not_connect}
    end.

%%-----------------------------------------------------------------------------
%% @spec handle_call(Request, From, State) ->
%%    {reply,Reply,NewState} | {reply,Reply,NewState,Timeout} | {reply,Reply,NewState,hibernate}
%%    | {noreply,NewState} | {noreply,NewState,Timeout} | {noreply,NewState,hibernate}
%%    | {stop,Reason,Reply,NewState} | {stop,Reason,NewState}
%% @doc Whenever a gen_server receives a request sent using gen_server:call/2,3 or 
%% gen_server:multi_call/2,3,4, this function is called to handle the request.
%%-----------------------------------------------------------------------------
handle_call(Request, _From, State) ->
    error_logger:info_msg("[~s] Unhandled Call Request:~n~w~n", [?MODULE, Request]),
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% @spec handle_cast(Request, State) ->
%%    {noreply,NewState} | {noreply,NewState,Timeout} | {noreply,NewState,hibernate}
%%    | {stop,Reason,NewState}
%% @doc Whenever a gen_server receives a request sent using gen_server:cast/2 or 
%% gen_server:abcast/2,3, this function is called to handle the request.
%%-----------------------------------------------------------------------------
handle_cast({?IRC_SEND_DATA, Data}, State) ->
    DataTail = string:substr(Data, string:len(Data) - 2),
    IrcData =
        case string:equal(DataTail, "\r\n") of
            true -> Data;
            false -> Data ++ "\r\n"
        end,
    error_logger:info_msg("[~s] Sending IRC command:~n~s~n", [?MODULE, IrcData]),
    SocketType = State#state.socket_type,
    SocketType:send(State#state.sock, IrcData),
    {noreply, State};
handle_cast(Request, State) ->
    error_logger:info_msg("[~s] Unhandled Cast Request:~n~w~n", [?MODULE, Request]),
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply,NewState} | {noreply,NewState,Timeout} | {noreply,NewState,hibernate} | {stop,Reason,NewState}
%% @doc This function is called by a gen_server when a timeout occurs or when it 
%% receives any other message than a synchronous or asynchronous request (or a system message).
%%-----------------------------------------------------------------------------
handle_info({tcp, Sock, Data}, State) ->
    IrcCommands = string:tokens(Data, "\r\n"),
    bot_process_request(State#state.processor, IrcCommands),
    NewState = State#state{sock=Sock},
    {noreply, NewState};
handle_info({tcp_closed, _Sock}, State) ->
    {noreply, State}; % TODO
handle_info(Info, _State) ->
    error_logger:info_msg("[~s] Unhandled Info Request:~n~w~n", [?MODULE, Info]),
    ok.

%%-----------------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to terminate.
%% It should be the opposite of Module:init/1 and do any necessary cleaning up.
%% When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%-----------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%-----------------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState} | {error, Reason}
%% @doc This function is called by a gen_server when it should update its internal
%% state during a release upgrade/downgrade, i.e. when the instruction 
%% {update,Module,Change,...} where Change={advanced,Extra} is given in the appup file.
%% See OTP Design Principles for more information.
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ===================================================================
%% Internal implementation
%% ===================================================================
%%-----------------------------------------------------------------------------
%% @spec TODO
%% @doc TODO
%%-----------------------------------------------------------------------------
connect(Network) ->
    error_logger:info_msg("[~s] Going to connect to network ~s~n", [?MODULE, Network]),
    {ok, Servers} = gen_server:call({global, dikulanirc_network_manager}, {network_get_info, Network}),
    open_connection(Servers).

%%-----------------------------------------------------------------------------
%% @spec TODO
%% @doc TODO
%%-----------------------------------------------------------------------------
open_connection([]) ->
    error_logger:info_msg("[~s] Unable to connect to any servers!~n", [?MODULE]),
    error; % TODO
open_connection([{Host, Port, Ssl} | OtherServers]) ->
    error_logger:info_msg("[~s] Trying to connect to server '~s' on port '~B'~n", [?MODULE, Host, Port]),
    TcpOpts = [{active, true}, {packet, line}, {keepalive, true}], % Don't need binary option, as IRC is plain text
    SocketType = case Ssl of
        true ->
            ok = ssl:start(),
            ssl;
        false ->
            gen_tcp
    end,
    case SocketType:connect(Host, Port, TcpOpts) of
        {ok, Sock} ->
            error_logger:info_msg("Connected to server '~s' on port '~B'~n", [Host, Port]),
            {ok, {Sock, SocketType}};
        _ ->
            open_connection(OtherServers)
    end.

%%-----------------------------------------------------------------------------
%% @spec TODO
%% @doc TODO
%%-----------------------------------------------------------------------------
bot_process_request(_, []) ->
    ok;
bot_process_request(BotProcessorId, [Data | Rest]) ->
    %gen_fsm:send_event({global, BotProcessorId}, {?IRC_RECEIVE, {calendar:universal_time(), Data}}), % Normal time stamp with date and time
    UnixTime = element(1, now()) * 10000 + element(2, now()), % Unix time :D
    gen_fsm:send_event({global, BotProcessorId}, {?IRC_RECEIVE, {UnixTime, Data}}),
    bot_process_request(BotProcessorId, Rest).


%% ===================================================================
%% Internal implementation
%% ===================================================================



