%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc The main API class that acts as the interface for the application
%% -------------------------------------------------------------------
-module(dikulanirc).

%% API for networks
-export([network_add/3, network_add/4, network_get_info/1]).

%% API for bots
-export([bot_create/4, bot_delete/1, bot_add_admin/2, bot_remove_admin/2, bot_enable/1, bot_disable/1, bot_get_all/0]).

%% The managers
-define(NODE_MANAGER, dikulanirc_node_manager).
-define(NETWORK_MANAGER, dikulanirc_network_manager).
-define(BOT_MANAGER, dikulanirc_bot_manager).

%% ===================================================================
%% API functions for networks
%% ===================================================================

%%-----------------------------------------------------------------------------
%% @spec network_add(Network, Host, Port) -> void()
%%    Network = string()
%%    Host = string()
%%    Port = integer()
%% @doc See Readme.md
%%-----------------------------------------------------------------------------
network_add(Network, Host, Port) ->
    network_add(Network, Host, Port, false).

%%-----------------------------------------------------------------------------
%% @spec network_add(Network, Host, Port, Ssl) -> void()
%%    Network = string()
%%    Host = string()
%%    Port = integer()
%%    Ssl = true | false
%% @doc See Readme.md
%%-----------------------------------------------------------------------------
network_add(Network, Host, Port, Ssl) ->
    ok = gen_server:cast({global, ?NETWORK_MANAGER}, {network_add, Network, {Host, Port, Ssl}}).

%%-----------------------------------------------------------------------------
%% @spec network_get_info(Network) -> ??? (NOT YET IMPLEMENTED CORRECTLY)
%%    Network = string()
%% @doc See Readme.md
%%-----------------------------------------------------------------------------
network_get_info(Network) ->
    {ok, Reply} = gen_server:call({global, ?NETWORK_MANAGER}, {network_get_info, Network}),
    Reply.

%% ===================================================================
%% API functions for bots
%% ===================================================================

%%-----------------------------------------------------------------------------
%% @spec bot_create(Network, Nick, Channels, Module) -> void()
%%    Network = string()
%%    Nick = string()
%%    Channels = [S], S = string()
%%    Module = atom()
%% @doc See Readme.md
%%-----------------------------------------------------------------------------
bot_create(Network, Nick, Channels, Module) ->
    ok = gen_server:cast({global, ?BOT_MANAGER}, {bot_create, Network, {Nick, Channels, Module}}).

%%-----------------------------------------------------------------------------
%% @spec bot_delete(BotId) -> void()
%%    BotId = atom(), (should be encapsulated in quotes)
%% @doc See Readme.md
%%-----------------------------------------------------------------------------
bot_delete(BotId) ->
    ok = gen_server:cast({global, ?BOT_MANAGER}, {bot_delete, BotId}).

%%-----------------------------------------------------------------------------
%% @spec bot_add_admin(BotId, Name) -> void()
%%    BotId = atom(), (should be encapsulated in quotes)
%%    Name = string()
%% @doc See Readme.md
%%-----------------------------------------------------------------------------
bot_add_admin(BotId, Name) ->
    ok = gen_server:cast({global, ?BOT_MANAGER}, {bot_add_admin, {BotId, Name}}).

%%-----------------------------------------------------------------------------
%% @spec bot_remove_admin(BotId, Name) -> void()
%%    BotId = atom(), (should be encapsulated in quotes)
%%    Name = string()
%% @doc See Readme.md
%%-----------------------------------------------------------------------------
bot_remove_admin(BotId, Name) ->
    ok = gen_server:cast({global, ?BOT_MANAGER}, {bot_remove_admin, {BotId, Name}}).

%%-----------------------------------------------------------------------------
%% @spec bot_enable(BotId) -> void()
%%    BotId = atom(), (should be encapsulated in quotes)
%% @doc See Readme.md
%%-----------------------------------------------------------------------------
bot_enable(BotId) ->
    ok = gen_server:cast({global, ?BOT_MANAGER}, {bot_enable, BotId}).

%%-----------------------------------------------------------------------------
%% @spec bot_disable(BotId) -> void()
%%    BotId = atom(), (should be encapsulated in quotes)
%% @doc See Readme.md
%%-----------------------------------------------------------------------------
bot_disable(BotId) ->
    ok = gen_server:cast({global, ?BOT_MANAGER}, {bot_disable, BotId}).

%%-----------------------------------------------------------------------------
%% @spec bot_get_all() -> ??? (NOT YET IMPLEMENTED CORRECTLY)
%% @doc See Readme.md
%%-----------------------------------------------------------------------------
bot_get_all() ->
    {ok, Reply} = gen_server:call({global, ?BOT_MANAGER}, bot_get_all),
    Reply.



