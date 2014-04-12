%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc The top level supervisor for the signup bot application
%% -------------------------------------------------------------------
-module(dikulanirc_sup).
-behaviour(supervisor).
-include_lib("db_records.hrl").
-include_lib("dikulanirc.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, ?SUPERVISOR_TIMEOUT, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, ?SUPERVISOR_TIMEOUT, Type, [I]}).


%% TODO:
%% Create a distributed setup of multiple nodes.
%% Create functionality to add and remove nodes while the program is running
%% (use the node manager for this functionality)


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%-----------------------------------------------------------------------------
%% @spec init(State) -> {ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore
%% @doc Whenever a supervisor is started using supervisor:start_link/2,3,
%% this function is called by the new process to find out about restart strategy,
%% maximum restart frequency and child specifications.
%%-----------------------------------------------------------------------------
init(_State) ->
    error_logger:info_msg("[~s] Starting DIKULAN IRC application, hang tight!~n", [?MODULE]),
    {ok, Nodes} = retrieve_nodes(), % For now just start locally
    init_database(Nodes),
    NodeManager = ?CHILD(dikulanirc_node_manager, worker, [Nodes]),
    NetworkManager = ?CHILD(dikulanirc_network_manager, worker),
    BotManager = ?CHILD(dikulanirc_bot_manager, worker),
    {ok, { {one_for_one, 5, 10}, [NodeManager, NetworkManager, BotManager]} }.


%% ===================================================================
%% Internal implementation
%% ===================================================================
%%-----------------------------------------------------------------------------
%% @spec init_database(Nodes) -> void()
%% @doc init_database is used to install the Mnesia Database on all nodes and create
%% the needed tables.
%%-----------------------------------------------------------------------------
init_database(Nodes) ->
    error_logger:info_msg("[~s] Initializing database on the following nodes:~n", [?MODULE]),
    lists:map(fun(X) ->  io:format(X) end, Nodes),
    mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    %application:start(mnesia),
    %
    mnesia:create_table(dikulanirc_bots,
        [{attributes, record_info(fields, dikulanirc_bots)},
        {index, [#dikulanirc_bots.network]},
        {type, set},
        {disc_copies, Nodes}]),
    mnesia:create_table(dikulanirc_networks,
        [{attributes, record_info(fields, dikulanirc_networks)},
        {index, [#dikulanirc_networks.host]},
        {type, set},
        {disc_copies, Nodes}]),
    ok = mnesia:wait_for_tables([dikulanirc_bots, dikulanirc_networks], ?MNESIA_TIMEOUT).

%%-----------------------------------------------------------------------------
%% @spec retrieve_nodes() -> {ok, Nodes}
%%    Nodes = [T], T = term()
%% @doc This function is used for retrieving all the nodes in the distributed
%% setup. We don't really know what to do yet, as there is no distributed setup.
%% So just return self().
%%-----------------------------------------------------------------------------
retrieve_nodes() ->
    {ok, [node()]}. % TODO: Fix distributed setup structure



