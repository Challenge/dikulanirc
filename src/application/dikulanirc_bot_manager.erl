%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc Module used to handle all the bots the application contains
%% -------------------------------------------------------------------
-module(dikulanirc_bot_manager).
-behaviour(gen_server).

-include_lib("dikulanirc.hrl").
-include_lib("db_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0]).

%% Internal API - DO NOT USE OUTSIDE!
-export([start_bot/1, stop_bot/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%% State definition
-record(state, {irc_bots=[]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    error_logger:info_msg("[~s] Starting bot manager~n", [?MODULE]),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%%-----------------------------------------------------------------------------
%% @spec init(State) -> {ok,State} | {ok,State,Timeout} | {ok,State,hibernate}
%%    | {stop,Reason} | ignore
%% @doc Whenever a gen_server is started using gen_server:start/3,4 or 
%% gen_server:start_link/3,4, this function is called by the new process to initialize.
%%-----------------------------------------------------------------------------
init(_State) ->
    error_logger:info_msg("[~s] Initializing state.~n", [?MODULE]),
    EnabledBots = case fetch_bots(true) of
        {ok, B} ->
            B;
        _ ->
            []
    end,
    Bots = EnabledBots,
    error_logger:info_msg("[~s] Fetched all active bots. Starting them all.~n", [?MODULE]),
    lists:map(fun(Bot) -> spawn(?MODULE, start_bot, [Bot]) end, Bots),
    error_logger:info_msg("[~s] All enabled bots has been started.~n", [?MODULE]),
    NewState = #state{ irc_bots = Bots },
    {ok, NewState}.

%%-----------------------------------------------------------------------------
%% @spec handle_call(Request, From, State) ->
%%    {reply,Reply,NewState} | {reply,Reply,NewState,Timeout} | {reply,Reply,NewState,hibernate}
%%    | {noreply,NewState} | {noreply,NewState,Timeout} | {noreply,NewState,hibernate}
%%    | {stop,Reason,Reply,NewState} | {stop,Reason,NewState}
%% @doc Whenever a gen_server receives a request sent using gen_server:call/2,3 or 
%% gen_server:multi_call/2,3,4, this function is called to handle the request.
%%-----------------------------------------------------------------------------
handle_call(bot_get_enabled, _From, State) ->
    Reply = fetch_bots(true),
    {reply, Reply, State};
handle_call(bot_get_disabled, _From, State) ->
    Reply = fetch_bots(false),
    {reply, Reply, State};
handle_call(bot_get_all, _From, State) ->
    Reply = fetch_bots(all),
    {reply, Reply, State};
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
handle_cast({bot_create, Network, {Nick, Channels, Module}}, State) ->
    error_logger:info_msg("[~s] bot_create - Network: ~s, Nick: ~s, Channels: ~s, Type: ~w~n", [?MODULE, Network, Nick, Channels, Module]),
    BotId = list_to_atom(Nick ++ "@" ++ Network), %THIS IS NOT THE BEST SOLUTION, BUT SHOULD WORK FOR SMALL APPLICATIONS
    Bot = #dikulanirc_bots{
            bot_id = BotId,
            network = Network,
            nick = Nick,
            channels = Channels,
            module = Module
        },
    F = fun() -> mnesia:write(Bot) end,
    {atomic, ok} = mnesia:transaction(F),
    start_bot(Bot),
    NewState = State#state{ irc_bots = [State#state.irc_bots | Bot]},
    {noreply, NewState};
handle_cast({bot_remove, BotId}, State) ->
    error_logger:info_msg("[~s] bot_remove - BotId: ~s~n", [?MODULE, atom_to_list(BotId)]),
    case get_bot_from_id(BotId) of
        {ok, _} ->
            F = fun() -> mnesia:delete({dikulanirc_bots, BotId}) end,
            {atomic, ok} = mnesia:transaction(F),
            error_logger:info_msg("[~s] bot_remove - Removed bot with id: ~s~n", [?MODULE, atom_to_list(BotId)]);
        _ ->
            error_logger:info_msg("[~s] bot_remove - No bot defined with id: ~s~n", [?MODULE, atom_to_list(BotId)]),
            no_such_bot
    end,
    {noreply, State};
handle_cast({bot_add_admin, {BotId, Name}}, State) ->
    error_logger:info_msg("[~s] bot_add_admin - BotId: ~s, Name: ~s~n", [?MODULE, atom_to_list(BotId), Name]),
    case get_bot_from_id(BotId) of
        {ok, [Bot | _]} ->
            BotProcessorId = list_to_atom(atom_to_list(Bot#dikulanirc_bots.module) ++ "_" ++ atom_to_list(Bot#dikulanirc_bots.bot_id)),
            gen_fsm:send_all_state_event({global, BotProcessorId}, {?EVENT_ADD_ADMIN, Name}),
            error_logger:info_msg("[~s] bot_add_admin - Added admin '~s' to bot with id: ~s~n", [?MODULE, Name, atom_to_list(BotId)]);
        _ ->
            error_logger:info_msg("[~s] bot_add_admin - No bot defined with id: ~s~n", [?MODULE, atom_to_list(BotId)]),
            no_such_bot
    end,
    {noreply, State};
handle_cast({bot_remove_admin, {BotId, Name}}, State) ->
    error_logger:info_msg("[~s] bot_remove_admin - BotId: ~s, Name: ~s~n", [?MODULE, atom_to_list(BotId), Name]),
    case get_bot_from_id(BotId) of
        {ok, [Bot | _]} ->
            BotProcessorId = list_to_atom(atom_to_list(Bot#dikulanirc_bots.module) ++ "_" ++ atom_to_list(Bot#dikulanirc_bots.bot_id)),
            gen_fsm:send_all_state_event({global, BotProcessorId}, {?EVENT_REMOVE_ADMIN, Name}),
            error_logger:info_msg("[~s] bot_remove_admin - Removed admin '~s' from bot with id: ~s~n", [?MODULE, Name, atom_to_list(BotId)]);
        _ ->
            error_logger:info_msg("[~s] bot_remove_admin - No bot defined with id: ~s~n", [?MODULE, atom_to_list(BotId)]),
            no_such_bot
    end,
    {noreply, State};
handle_cast({bot_enable, BotId}, State) ->
    error_logger:info_msg("[~s] bot_enable - BotId: ~s~n", [?MODULE, atom_to_list(BotId)]),
    case get_bot_from_id(BotId) of
        {ok, [Bot | _]} ->
            {ok, EnabledBots} = fetch_bots(true),
            EnabledBotIds = lists:map(fun(B) -> B#dikulanirc_bots.bot_id end, EnabledBots),
            case lists:member(BotId, EnabledBotIds) of
                true ->
                    error_logger:info_msg("[~s] bot_enable - Already enabled: ~s~n", [?MODULE, atom_to_list(BotId)]),
                    already_enabled;
                false ->
                    NewBot = Bot#dikulanirc_bots{ enabled = true },
                    F = fun() -> mnesia:write(NewBot) end,
                    {atomic, ok} = mnesia:transaction(F),
                    start_bot(NewBot),
                    error_logger:info_msg("[~s] bot_enable - Enabled bot with id: ~s~n", [?MODULE, atom_to_list(BotId)])
            end;
        _ ->
            error_logger:info_msg("[~s] bot_enable - No bot defined with id: ~s~n", [?MODULE, atom_to_list(BotId)]),
            no_such_bot
    end,
    {noreply, State};
handle_cast({bot_disable, BotId}, State) ->
    error_logger:info_msg("[~s] bot_disable - BotId: ~s~n", [?MODULE, atom_to_list(BotId)]),
    case get_bot_from_id(BotId) of
        {ok, [Bot | _]} ->
            {ok, DisabledBots} = fetch_bots(false),
            DisabledBotIds = lists:map(fun(B) -> B#dikulanirc_bots.bot_id end, DisabledBots),
            case lists:member(BotId, DisabledBotIds) of
                true ->
                    error_logger:info_msg("[~s] bot_disable - Already disabled: ~s~n", [?MODULE, atom_to_list(BotId)]),
                    already_disabled;
                false ->
                    NewBot = Bot#dikulanirc_bots{ enabled = false },
                    F = fun() -> mnesia:write(NewBot) end,
                    {atomic, ok} = mnesia:transaction(F),
                    stop_bot(NewBot),
                    error_logger:info_msg("[~s] bot_disable - Disabled bot with id: ~s~n", [?MODULE, atom_to_list(BotId)])
            end;
        _ ->
            error_logger:info_msg("[~s] bot_disable - No bot defined with id: ~s~n", [?MODULE, atom_to_list(BotId)]),
            no_such_bot
    end,
    {noreply, State};
handle_cast(Request, State) ->
    error_logger:info_msg("[~s] Unhandled Cast Request:~n~w~n", [?MODULE, Request]),
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply,NewState} | {noreply,NewState,Timeout} | {noreply,NewState,hibernate} | {stop,Reason,NewState}
%% @doc This function is called by a gen_server when a timeout occurs or when it 
%% receives any other message than a synchronous or asynchronous request (or a system message).
%%-----------------------------------------------------------------------------
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
%% @spec get_bot_from_id(BotId) -> {ok, Bot} | {error, Reason}
%%    BotId = atom()
%% @doc Fetches the given bot from the database given the id of the bot.
%%-----------------------------------------------------------------------------
get_bot_from_id(BotId) ->
    Q = qlc:q([B || B <- mnesia:table(dikulanirc_bots), B#dikulanirc_bots.bot_id =:= BotId]),
    case mnesia:transaction(fun() -> qlc:e(Q) end) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

%%-----------------------------------------------------------------------------
%% @spec fetch_bots(Active) -> {ok, Bots}
%%    Active = atom()
%%    Bots = [#dikulanirc_bots{}]
%% @doc Fetches the bots from the database. If Active is 'true' then only enabled
%% bots will be fetched, likewise if Active is 'false' then only disabled bots 
%% will be fetched. If Active is anything else, then all bots will be fetched.
%%-----------------------------------------------------------------------------
fetch_bots(Active) ->
    case Active of
        true -> 
            Q = qlc:q([B || B <- mnesia:table(dikulanirc_bots), B#dikulanirc_bots.enabled =:= true]);
        false -> 
            Q = qlc:q([B || B <- mnesia:table(dikulanirc_bots), B#dikulanirc_bots.enabled =:= false]);
        _ ->
            Q = qlc:q([B || B <- mnesia:table(dikulanirc_bots)])
    end,
    {atomic, Bots} = mnesia:transaction(fun() -> qlc:e(Q) end),
    {ok, Bots}.

%%-----------------------------------------------------------------------------
%% @spec start_bot(Bot) -> ok
%% @doc Starts the given bot with a supervisor.
%%-----------------------------------------------------------------------------
start_bot(Bot) ->
    Child = ?CHILD(dikulanirc_bot_sup, supervisor, Bot),
    supervisor:start_child({global, dikulanirc_sup}, Child),
    ok.

%%-----------------------------------------------------------------------------
%% @spec stop_bot(Bot) -> ok
%% @doc Stops the given bot by killing the supervisor.
%%-----------------------------------------------------------------------------
stop_bot(Bot) ->
    supervisor:stop_child({global, dikulanirc_sup}, Bot#dikulanirc_bots.bot_id),
    supervisor:delete_child({global, dikulanirc_sup}, Bot#dikulanirc_bots.bot_id),
    ok.




