%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc The supervisor for bots
%% -------------------------------------------------------------------
-module(dikulanirc_bot_sup).
-behaviour(supervisor).
-include_lib("db_records.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link(Bot) ->
    error_logger:info_msg("[~s] Starting bot supervisor for bot with id ~w~n", [?MODULE, Bot#dikulanirc_bots.bot_id]),
    SupervisorName = list_to_atom("dikulanirc_bot_sup_" ++ atom_to_list(Bot#dikulanirc_bots.bot_id)),
    supervisor:start_link({global, SupervisorName}, ?MODULE, Bot).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%-----------------------------------------------------------------------------
%% @spec init(State) -> {ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore
%% @doc Whenever a supervisor is started using supervisor:start_link/2,3,
%% this function is called by the new process to find out about restart strategy,
%% maximum restart frequency and child specifications.
%%-----------------------------------------------------------------------------
init(Bot) ->
    %% Order is important. The bot should be started before the router.
    BotProcessorId = list_to_atom(atom_to_list(Bot#dikulanirc_bots.module) ++ "_" ++ atom_to_list(Bot#dikulanirc_bots.bot_id)),
    IrcRouterId = list_to_atom(atom_to_list(dikulanirc_router) ++ "_" ++ atom_to_list(Bot#dikulanirc_bots.bot_id)),
    BotProcessor = ?CHILD(Bot#dikulanirc_bots.module, worker, [Bot, IrcRouterId, BotProcessorId]),
    IrcRouter = ?CHILD(dikulanirc_router, worker, [Bot, IrcRouterId, BotProcessorId]),
    {ok, { {one_for_all, 5, 10}, [BotProcessor, IrcRouter]} }.







