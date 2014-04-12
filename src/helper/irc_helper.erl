%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc Helper functions used sending IRC messages to the irc router
%% -------------------------------------------------------------------
-module(irc_helper).
-include_lib("irc_constants.hrl").

%% API
-export([join/2, kick/2, oper/2, pong/2, privmsg/2, register/2, whois/2]).
-export([get_all_nodes/0]).

%% ===================================================================
%% API functions for IRC commands
%% ===================================================================
join(IrcRouterId, Channels) ->
    ok = gen_server:cast({global, IrcRouterId}, {?IRC_SEND_DATA, lib_irc:join(Channels)}).

kick(IrcRouterId, {Nick, Channel, Reason}) ->
    ok = gen_server:cast({global, IrcRouterId}, {?IRC_SEND_DATA, lib_irc:kick(Nick, Channel, Reason)}).

oper(IrcRouterId, {UserId, Password}) ->
    ok = gen_server:cast({global, IrcRouterId}, {?IRC_SEND_DATA, lib_irc:oper(UserId, Password)}).

pong(IrcRouterId, Data) ->
    ok = gen_server:cast({global, IrcRouterId}, {?IRC_SEND_DATA, lib_irc:pong(Data)}).

privmsg(IrcRouterId, {Dest, Message}) ->
    ok = gen_server:cast({global, IrcRouterId}, {?IRC_SEND_DATA, lib_irc:privmsg(Dest, Message)}).

register(IrcRouterId, Nick) ->
    ok = gen_server:cast({global, IrcRouterId}, {?IRC_SEND_DATA, lib_irc:nick(Nick)}),
    ok = gen_server:cast({global, IrcRouterId}, {?IRC_SEND_DATA, lib_irc:user(Nick, "0", "DIKULAN IRC BOT")}).

whois(IrcRouterId, Nick) ->
    ok = gen_server:cast({global, IrcRouterId}, {?IRC_SEND_DATA, lib_irc:whois(Nick)}).


%% ===================================================================
%% API functions for fetching information
%% ===================================================================
get_all_nodes() ->
    gen_server:call({global, dikulanirc_node_manager}, node_get_all).


%% ===================================================================
%% Internal implementation
%% ===================================================================









