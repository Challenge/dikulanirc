-module(erlang_run).
-include_lib("db_records.hrl").
-include_lib("irc_records.hrl").
-include_lib("irc_constants.hrl").

-include_lib("signup_bot.hrl").

-export([do_stuff/0, do_init/0, do_try/0, do_crewlan/0]).


do_stuff() ->
    dikulanirc_app:start([], []).

do_init() ->
    dikulanirc_app:start([], []),
    dikulanirc:network_add("krageland", "krageland.dk", 6667),
    dikulanirc:network_get_info("krageland"),
    dikulanirc:bot_create("krageland", "SignupBot", ["#test"], signup_bot).

do_try() ->
    nothing_to_do.

do_crewlan() ->
    dikulanirc_app:start([], []),
    dikulanirc:network_add("Freenode", "irc.freenode.net", 6667),
    dikulanirc:bot_create("Freenode", "SignupBot", ["#dikulan"], signup_bot).

fisk4() ->
    EventId = 5,
    F = fun() -> mnesia:delete(signup_bot_event, EventId) end,
    {atomic, ok} = mnesia:transaction(F).

fisk3() ->
    dikulanirc:bot_enable('SignupBot@krageland').


fisk2() ->
    dikulanirc:bot_add_admin('SignupBot@krageland', "Ektorus").


fisk() ->
    Network = "krageland",
    Nick = "SignupBot",
    Channels = "#test",
    Module = signup_bot,
    BotId = list_to_atom(Nick ++ "@" ++ Network),
    Bot = #dikulanirc_bots{bot_id = BotId, network = Network, nick = Nick, channels = Channels, module = Module},
    dikulanirc_bot_sup:start_link(Bot).




