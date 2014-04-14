%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc The signup bot, used for signing up to events for DIKULAN
%% -------------------------------------------------------------------
-module(signup_bot).
-behaviour(gen_fsm).

%% Includes for all bot types
-include_lib("dikulanirc.hrl").
-include_lib("db_records.hrl").
-include_lib("irc_records.hrl").
-include_lib("irc_constants.hrl").

%% Specific include for this bot
-include_lib("stdlib/include/qlc.hrl").
-include_lib("signup_bot.hrl").

%% API
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% gen_fsm state exports
-export([connecting/2, registering/2, ready/2]).

%% State definition
-record(state, {bot, router}).


%% ===================================================================
%% API functions
%% ===================================================================
start_link(Bot, IrcRouterId, BotProcessorId) ->
    error_logger:info_msg("[~s] Spawning new bot ~w~n", [?MODULE, Bot#dikulanirc_bots.bot_id]),
    gen_fsm:start_link({global, BotProcessorId}, ?MODULE, [Bot, IrcRouterId], []).

%%-----------------------------------------------------------------------------
%% @spec init(State) -> {ok,StateName,StateData} | {ok,StateName,StateData,Timeout}
%%     | {ok,StateName,StateData,hibernate}
%%     | {stop,Reason} | ignore
%% @doc Whenever a gen_fsm is started using gen_fsm:start/3,4 or 
%% gen_fsm:start_link/3,4, this function is called by the new process to initialize.
%%-----------------------------------------------------------------------------
init([Bot, IrcRouterId]) ->
    error_logger:info_msg("[~s] Initializing new bot ~w~n", [?MODULE, Bot#dikulanirc_bots.bot_id]),
    {ok, Nodes} = irc_helper:get_all_nodes(),
    init_database(Nodes),
    {ok, connecting, #state{bot=Bot, router=IrcRouterId}}.

%%-----------------------------------------------------------------------------
%% @spec handle_event(Event, StateName, StateData) -> 
%%      {next_state,NextStateName,NewStateData}
%%    | {next_state,NextStateName,NewStateData,Timeout}
%%    | {next_state,NextStateName,NewStateData,hibernate}
%%    | {stop,Reason,NewStateData}
%% @doc Whenever a gen_fsm receives an event sent using 
%% gen_fsm:send_all_state_event/2, this function is called to handle the event.
%%-----------------------------------------------------------------------------
handle_event({?EVENT_ADD_ADMIN, Name}, StateName, StateData) ->
    error_logger:info_msg("[~s] Adding '~s' as super admin~n", [?MODULE, Name]),
    case is_normal_admin(Name) of
        true -> remove_admin(?NORMAL_ADMIN, Name);
        false -> pass
    end,
    add_admin(?SUPER_ADMIN, Name),
    {next_state, StateName, StateData};
handle_event({?EVENT_REMOVE_ADMIN, Name}, StateName, StateData) ->
    error_logger:info_msg("[~s] Removing '~s' as super admin~n", [?MODULE, Name]),
    remove_admin(?SUPER_ADMIN, Name),
    {next_state, StateName, StateData};
handle_event(Event, StateName, StateData) ->
    error_logger:info_msg("[~s] Unhandled event: ~s~n", [?MODULE, Event]),
    {next_state, StateName, StateData}.

%%-----------------------------------------------------------------------------
%% @spec handle_sync_event(Event, From, StateName, StateData) ->
%%      {reply,Reply,NextStateName,NewStateData}
%%    | {reply,Reply,NextStateName,NewStateData,Timeout}
%%    | {reply,Reply,NextStateName,NewStateData,hibernate}
%%    | {next_state,NextStateName,NewStateData}
%%    | {next_state,NextStateName,NewStateData,Timeout}
%%    | {next_state,NextStateName,NewStateData,hibernate}
%%    | {stop,Reason,Reply,NewStateData} | {stop,Reason,NewStateData}
%% @doc Whenever a gen_fsm receives an event sent using 
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle the event.
%%-----------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
    error_logger:info_msg("[~s] Unhandled sync event: ~s~n", [?MODULE, Event]),
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%%-----------------------------------------------------------------------------
%% @spec handle_info(Info, StateName, StateData) -> 
%%      {next_state,NextStateName,NewStateData}
%%    | {next_state,NextStateName,NewStateData,Timeout}
%%    | {next_state,NextStateName,NewStateData,hibernate}
%%    | {stop,Reason,NewStateData}
%% @doc This function is called by a gen_fsm when it receives any other message
%% than a synchronous or asynchronous event (or a system message).
%%-----------------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
    error_logger:info_msg("[~s] Unhandled info: ~s~n", [?MODULE, Info]),
    {next_state, StateName, StateData}.

%%-----------------------------------------------------------------------------
%% @spec terminate(Reason, StateName, StateData) -> void()
%% @doc This function is called by a gen_fsm when it is about to terminate.
%% It should be the opposite of Module:init/1 and do any necessary cleaning up.
%% When it returns, the gen_fsm terminates with Reason.
%% The return value is ignored.
%%-----------------------------------------------------------------------------
terminate(_Reason, _StateName, _StateData) ->
    ok.

%%-----------------------------------------------------------------------------
%% @spec code_change(OldVsn, StateName, StateData, Extra) -> {ok, NextStateName, NewStateData}
%% @doc This function is called by a gen_fsm when it should update its internal
%% state data during a release upgrade/downgrade, i.e. when the instruction
%% {update,Module,Change,...} where Change={advanced,Extra} is given in the appup file.
%% See OTP Design Principles.
%%-----------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    error_logger:info_msg("[~s] Code has been updated!"),
    {ok, StateName, StateData}.

%% ===================================================================
%% States
%% ===================================================================

%%-----------------------------------------------------------------------------
%% @spec StateName(Event, StateData) -> {next_state,NextStateName,NewStateData} 
%%    | {next_state,NextStateName,NewStateData,Timeout}
%%    | {next_state,NextStateName,NewStateData,hibernate}
%%    | {stop,Reason,NewStateData}
%% @doc There should be one instance of this function for each possible state name.
%% Whenever a gen_fsm receives an event sent using gen_fsm:send_event/2, the instance
%% of this function with the same name as the current state name StateName is called 
%% to handle the event. It is also called if a timeout occurs.
%%-----------------------------------------------------------------------------
connecting(?IRC_CONNECTED, State) ->
    Bot = State#state.bot,
    ok = irc_helper:register(State#state.router, Bot#dikulanirc_bots.nick),
    error_logger:info_msg("[~s] Switching state: connecting->registering~n", [get_bot_id(Bot)]),
    {next_state, registering, State};
connecting(Request, State) ->
    Bot = State#state.bot,
    error_logger:info_msg("[~s] In state ~s, unknown request:~n~w~n", [get_bot_id(Bot), "connecting", Request]),
    {next_state, connecting, State}.

registering({?IRC_RECEIVE, {DateTime, Packet}}, State) ->
    Bot = State#state.bot,
    Router = State#state.router,
    error_logger:info_msg("[~s] In state ~s got packet:~n~s~n", [get_bot_id(Bot), "registering", Packet]),
    IrcData = lib_irc:parse_irc(Bot, DateTime, Packet),
    error_logger:info_msg("[~s] Parsed packet information:~nCommand: ~B~nCommand Args: ~s~nSender: ~s~nDestination: ~s~n~n", [get_bot_id(Bot), IrcData#irc_data.command, IrcData#irc_data.command_args, IrcData#irc_data.sender, IrcData#irc_data.destination]),
    case IrcData#irc_data.command of
        ?PING ->
            irc_helper:pong(Router, IrcData#irc_data.command_args),
            {next_state, registering, State};
        ?ERROR_NICKNAME_IN_USE ->
            error_logger:info_msg("[~s] Nickname already in use~n", [get_bot_id(Bot)]),
            NewNick = Bot#dikulanirc_bots.nick ++ ?IRC_NICK_SUFFIX,
            NewBot = Bot#dikulanirc_bots{nick = NewNick},
            irc_helper:register(Router, NewNick),
            NewState = State#state{bot = NewBot},
            {next_state, registering, NewState};
        ?R_WELCOME ->
            error_logger:info_msg("[~s] Switching state: registering->ready~n", [get_bot_id(Bot)]),
            irc_helper:join(Router, Bot#dikulanirc_bots.channels),
            {next_state, ready, State};
        _ ->
            {next_state, registering, State}
    end;
registering(Request, State) ->
    Bot = State#state.bot,
    error_logger:info_msg("[~s] In state ~s, unknown request:~n~w~n", [get_bot_id(Bot), "registering", Request]),
    {next_state, connecting, State}.

ready({?IRC_RECEIVE, {DateTime, Packet}}, State) ->
    Bot = (State#state.bot),
    Router = State#state.router,
    error_logger:info_msg("[~s] In state ~s got packet:~n~s~n", [get_bot_id(Bot), "ready", Packet]),
    IrcData = lib_irc:parse_irc(Bot, DateTime, Packet),
    error_logger:info_msg("[~s] Parsed packet information:~nCommand: ~B~nCommand Args: ~s~nSender: ~s~nDestination: ~s~n~n", [get_bot_id(Bot), IrcData#irc_data.command, IrcData#irc_data.command_args, IrcData#irc_data.sender, IrcData#irc_data.destination]),
    case IrcData#irc_data.command of
        ?PING ->
            irc_helper:pong(Router, IrcData#irc_data.command_args),
            {next_state, ready, State};
        ?PRIVMSG ->
            %% Check if this request is a whisper or not.
            case IrcData#irc_data.sender =:= IrcData#irc_data.destination of
                true -> parse_private_request(Bot, Router, IrcData, IrcData#irc_data.command_args);
                false -> parse_public_request(Bot, Router, IrcData, IrcData#irc_data.command_args)
            end,
            {next_state, ready, State};
        _ ->
            {next_state, ready, State}
    end;
ready(Request, State) ->
    Bot = State#state.bot,
    error_logger:info_msg("[~s] In state ~s, unknown request:~n~w~n", [get_bot_id(Bot), "ready", Request]),
    {next_state, connecting, State}.

%% ===================================================================
%% Internal implementation
%% ===================================================================

%%-----------------------------------------------------------------------------
%% @spec init_database(Nodes) -> void()
%% @doc init_database is used to install the Mnesia Database on all nodes and create
%% the needed tables.
%%-----------------------------------------------------------------------------
init_database(Nodes) ->
    error_logger:info_msg("[~s] Creating Mnesia database tables on the following nodes:~n~w~n", [?MODULE, Nodes]),
    mnesia:create_table(unique_ids,
        [{attributes, record_info(fields, unique_ids)},
        {disc_copies, Nodes}]),
    mnesia:create_table(signup_bot_event,
        [{attributes, record_info(fields, signup_bot_event)},
        {index, [#signup_bot_event.name]},
        {type, set},
        {disc_copies, Nodes}]),
    mnesia:create_table(signup_bot_signups,
        [{attributes, record_info(fields, signup_bot_signups)},
        {type, bag},
        {disc_copies, Nodes}]),
    mnesia:create_table(signup_bot_admins,
        [{attributes, record_info(fields, signup_bot_admins)},
        {type, bag},
        {disc_copies, Nodes}]),
    ok = mnesia:wait_for_tables([unique_ids, signup_bot_event, signup_bot_signups, signup_bot_admins], ?MNESIA_TIMEOUT).

%%-----------------------------------------------------------------------------
%% @spec get_next_event_id() -> integer()
%% @doc This simulates the auto increment feature of most modern databases.
%% It returns the next unique id.
%%-----------------------------------------------------------------------------
get_next_event_id() ->
    mnesia:dirty_update_counter(unique_ids, event_type, 1).

%%-----------------------------------------------------------------------------
%% @spec get_bot_id(Bot) -> atom()
%%    Bot = #dikulanirc_bots{}
%% @doc Returns the id of the given bot
%%-----------------------------------------------------------------------------
get_bot_id(Bot) ->
    Bot#dikulanirc_bots.bot_id.

%%-----------------------------------------------------------------------------
%% @spec add_admin(Role, Name) -> {aborted, Reason} | {atomic, ok}
%%    Role = atom()
%%    Name = string()
%% @doc Adds an admin with the given name under role. Name should be the nick
%% name of the user.
%%-----------------------------------------------------------------------------
add_admin(Role, Name) ->
    F = fun() ->
        Admin = #signup_bot_admins{role = Role, name = Name},
        mnesia:write(Admin)
    end,
    mnesia:transaction(F).

%%-----------------------------------------------------------------------------
%% @spec remove_admin(Role, Name) -> aborted | ok
%%    Role = atom()
%%    Name = string()
%% @doc Removes an admin with the given name under role. Name should be the nick
%% name of the user.
%%-----------------------------------------------------------------------------
remove_admin(Role, Name) ->
    F = fun() ->
        Admin = #signup_bot_admins{role = Role, name = Name},
        mnesia:delete_object(Admin)
    end,
    mnesia:transaction(F).

%%-----------------------------------------------------------------------------
%% @spec get_normal_admins() -> [Admins]
%%    Admins = string()
%% @doc Fetches all normal admins from the database and returns the list of normal admins.
%%-----------------------------------------------------------------------------
get_normal_admins() ->
    QNormal = qlc:q([NA#signup_bot_admins.name ||
        NA <- mnesia:table(signup_bot_admins), NA#signup_bot_admins.role =:= ?NORMAL_ADMIN]),
    {atomic, NormalAdmins} = mnesia:transaction(fun() -> qlc:e(QNormal) end),
    NormalAdmins.

%%-----------------------------------------------------------------------------
%% @spec get_super_admins() -> [Admins]
%%    Admins = string()
%% @doc Fetches all super admins from the database and returns the list of super admins.
%%-----------------------------------------------------------------------------
get_super_admins() ->
    QSuper = qlc:q([SA#signup_bot_admins.name ||
        SA <- mnesia:table(signup_bot_admins), SA#signup_bot_admins.role =:= ?SUPER_ADMIN]),
    {atomic, SuperAdmins} = mnesia:transaction(fun() -> qlc:e(QSuper) end),
    SuperAdmins.

%%-----------------------------------------------------------------------------
%% @spec is_admin(Name) -> true | false
%% @doc Returns true if the given nick is an admin (any), false otherwise.
%%-----------------------------------------------------------------------------
is_admin(Name) ->
    Pattern = #signup_bot_admins{name = Name, _ = '_'},
    M = fun() ->
        Res = mnesia:match_object(Pattern),
        [Role || #signup_bot_admins{ role = Role } <- Res]
    end,
    {atomic, R} = mnesia:transaction(M),
    case R of
        % If R is an empty list, that means that there doesn't exist an admin with that name.
        [] ->
            false;
        _AllRoles ->
            true
    end.

%%-----------------------------------------------------------------------------
%% @spec is_super_admin(Name) -> true | false
%% @doc Returns true if the given nick is a super admin, false otherwise.
%%-----------------------------------------------------------------------------
is_super_admin(Name) ->
    Q = qlc:q([Admin || Admin <- mnesia:table(signup_bot_admins),
        Admin#signup_bot_admins.name =:= Name, Admin#signup_bot_admins.role =:= ?SUPER_ADMIN]),
    {atomic, Result} = mnesia:transaction(fun() -> qlc:e(Q) end),
    case Result of
        % If R is an empty list, that means that there doesn't exist an admin with that name.
        [] ->
            false;
        _ ->
            true
    end.

%%-----------------------------------------------------------------------------
%% @spec is_normal_admin(Name) -> true | false
%% @doc Returns true if the given nick is a normal admin, false otherwise.
%%-----------------------------------------------------------------------------
is_normal_admin(Name) ->
    Q = qlc:q([Admin || Admin <- mnesia:table(signup_bot_admins),
        Admin#signup_bot_admins.name =:= Name, Admin#signup_bot_admins.role =:= ?NORMAL_ADMIN]),
    {atomic, Result} = mnesia:transaction(fun() -> qlc:e(Q) end),
    case Result of
        % If R is an empty list, that means that there doesn't exist an admin with that name.
        [] ->
            false;
        _ ->
            true
    end.

%%-----------------------------------------------------------------------------
%% @spec parse_private_request(Bot, Router, IrcData, Request) -> atom()
%%    Bot = #dikulanirc_bots{}
%%    Router = atom()
%%    IrcData = #irc_data{}
%%    Request = string()
%% @doc Parses a private command to this bot. The command must start with the private
%% command symbol given in the irc header file. The symbol may or may not be followed
%% by our nick name. In any case the nick name is removed (if it exsits) and we 
%% call the command to parse the request.
%%-----------------------------------------------------------------------------
parse_private_request(Bot, Router, IrcData, ?IRC_PUBLIC_COMMAND_SYMBOL ++ Request) ->
    parse_private_request_helper(Bot, Router, IrcData, Request);
parse_private_request(Bot, Router, IrcData, ?IRC_PRIVATE_COMMAND_SYMBOL ++ Request) ->
    parse_private_request_helper(Bot, Router, IrcData, Request);
parse_private_request(_Bot, _Router, _IrcData, _Request) ->
    unknown_request.

parse_private_request_helper(Bot, Router, IrcData, Request) ->
    error_logger:info_msg("[~s] Parsing private request:~n~s~n", [get_bot_id(Bot), IrcData#irc_data.command_args]),
    FormatedRequest = string:to_lower(string:strip(Request, both)),
    Nick = Bot#dikulanirc_bots.nick,
    case string:str(FormatedRequest, string:to_lower(Nick)) of
        0 ->
            error_logger:info_msg("[~s] Request doesn't contain our nick. Private request so parsing anyways. New request: ~s~n", [get_bot_id(Bot), string:strip(FormatedRequest, both)]),
            parse_request(Bot, Router, IrcData, string:strip(FormatedRequest, both));
        Index ->
            NewRequest = string:substr(FormatedRequest, Index + string:len(Nick)),
            error_logger:info_msg("[~s] Request contains our nick, parse it. New request: ~s~n", [get_bot_id(Bot), string:strip(NewRequest, both)]),
            parse_request(Bot, Router, IrcData, string:strip(NewRequest, both))
    end.

%%-----------------------------------------------------------------------------
%% @spec parse_private_request(Bot, Router, IrcData, Request) -> atom()
%%    Bot = #dikulanirc_bots{}
%%    Router = atom()
%%    IrcData = #irc_data{}
%%    Request = string()
%% @doc Parses a public command given to any channel we are connected to.
%% The command must start with the public command symbol given in the irc header file.
%% The symbol must then be followed by our nick name, else we just ignore the reqest.
%% Then the nick name is removed and we call the command to parse the request.
%%-----------------------------------------------------------------------------
parse_public_request(Bot, Router, IrcData, ?IRC_PUBLIC_COMMAND_SYMBOL ++ Request) ->
    error_logger:info_msg("[~s] Parsing public request:~n~s~n", [get_bot_id(Bot), IrcData#irc_data.command_args]),
    FormatedRequest = string:to_lower(string:strip(Request, both)),
    Nick = Bot#dikulanirc_bots.nick,
    case string:str(FormatedRequest, string:to_lower(Nick)) of
        0 ->
            %% The command symbol must be followed by our name for us to care
            error_logger:info_msg("[~s] REQUEST DOES NOT CONTAIN OUR NICK!~n", [get_bot_id(Bot)]),
            unknown_request;
        Index ->
            NewRequest = string:substr(FormatedRequest, Index + string:len(Nick)),
            error_logger:info_msg("[~s] Request contains our nick, parse it. New request: ~s~n", [get_bot_id(Bot), string:strip(NewRequest, both)]),
            parse_request(Bot, Router, IrcData, string:strip(NewRequest, both))
    end;
parse_public_request(_Bot, _Router, _IrcData, _Request) ->
    unknown_request.

%%-----------------------------------------------------------------------------
%% @spec parse_request(Bot, Router, IrcData, Request) -> atom()
%%    Bot = #dikulanirc_bots{}
%%    Router = atom()
%%    IrcData = #irc_data{}
%%    Request = string()
%% @doc Parses a request given to this bot.
%%-----------------------------------------------------------------------------
parse_request(Bot, Router, IrcData, "admin " ++ Rest) ->
    error_logger:info_msg("[~s] Received a '~s' request with arguments: ~s~n", [get_bot_id(Bot), "admin", string:strip(Rest, both)]),
    Name = IrcData#irc_data.sender,
    case is_admin(Name) of
        true ->
            parse_admin_request(Bot, Router, IrcData, string:strip(Rest, both));
        _ ->
            Destination = IrcData#irc_data.destination,
            irc_helper:privmsg(Router, {Destination, "You do not have the right permissions to execute this command!"})
    end;
parse_request(Bot, Router, IrcData, "announce " ++ Rest) ->
    error_logger:info_msg("[~s] Received a '~s' request with arguments: ~s~n", [get_bot_id(Bot), "announce", string:strip(Rest, both)]),
    ArgumentParseList = string:tokens(Rest, " "),
    StrippedRest = string:strip(Rest, both),
    case length(ArgumentParseList) of
        Len when Len < 2 ->
            send_signupbot_invalid_arguments(Router, IrcData#irc_data.destination);
        _ ->
            case string:chr(StrippedRest, $:) of
                0 ->
                    % Symbol ':' not found in the string.
                    Destination = IrcData#irc_data.destination,
                    irc_helper:privmsg(Router, {Destination, "The description for the event is missing."});
                Pos ->
                    CommandArgs = IrcData#irc_data.command_args,
                    Name = string:substr(StrippedRest, 1, Pos - 1),
                    Message = string:substr(CommandArgs, string:chr(CommandArgs, $:) + 1),
                    announce_event(Bot, Router, IrcData, string:strip(Name, both), string:strip(Message, both))
            end
    end;
parse_request(Bot, Router, IrcData, "create " ++ Rest) ->
    error_logger:info_msg("[~s] Received a '~s' request with arguments: ~s~n", [get_bot_id(Bot), "create", string:strip(Rest, both)]),
    ArgumentParseList = string:tokens(Rest, " "),
    StrippedRest = string:strip(Rest, both),
    case length(ArgumentParseList) of
        Len when Len < 2 ->
            send_signupbot_invalid_arguments(Router, IrcData#irc_data.destination);
        _ ->
            case string:chr(StrippedRest, $:) of
                0 ->
                    % Symbol ':' not found in the string.
                    Destination = IrcData#irc_data.destination,
                    irc_helper:privmsg(Router, {Destination, "The message for the announcement is missing."});
                Pos ->
                    Name = string:substr(StrippedRest, 1, Pos - 1),
                    Description = string:substr(StrippedRest, Pos + 1),
                    Event = #signup_bot_event{
                        event_id = get_next_event_id(),
                        name = string:strip(Name, both),
                        description = string:strip(Description, both),
                        owner = IrcData#irc_data.sender,
                        unix_time = IrcData#irc_data.time
                    },
                    create_event(Router, IrcData, Event)
            end
    end;
parse_request(Bot, Router, IrcData, "delete " ++ Rest) ->
    error_logger:info_msg("[~s] Received a '~s' request with arguments: ~s~n", [get_bot_id(Bot), "delete", string:strip(Rest, both)]),
    delete_event(Router, IrcData, string:strip(Rest, both));
parse_request(Bot, Router, IrcData, "designup " ++ Rest) ->
    error_logger:info_msg("[~s] Received a '~s' request with arguments: ~s~n", [get_bot_id(Bot), "signup", string:strip(Rest, both)]),
    designup_event(Router, IrcData, string:strip(Rest, both));
parse_request(Bot, Router, IrcData, "help") ->
    error_logger:info_msg("[~s] Received a '~s' request~n", [get_bot_id(Bot), "help"]),
    send_signupbot_help(Bot, Router, IrcData#irc_data.sender);
parse_request(Bot, Router, IrcData, "list") ->
    error_logger:info_msg("[~s] Received a '~s' request~n", [get_bot_id(Bot), "list"]),
    send_signupbot_list(Router, IrcData#irc_data.sender);
parse_request(Bot, Router, IrcData, "signup " ++ Rest) ->
    error_logger:info_msg("[~s] Received a '~s' request with arguments: ~s~n", [get_bot_id(Bot), "signup", string:strip(Rest, both)]),
    signup_event(Router, IrcData, string:strip(Rest, both));
parse_request(_Bot, Router, IrcData, _Request) ->
    irc_helper:privmsg(Router, {IrcData#irc_data.sender, "Invalid command! Use the 'help' command to se available commands and parameters."}),
    not_a_valid_command.

%%-----------------------------------------------------------------------------
%% @spec parse_admin_request(Bot, Router, IrcData, Request) -> atom()
%%    Bot = #dikulanirc_bots{}
%%    Router = atom()
%%    IrcData = #irc_data{}
%%    Request = string()
%% @doc Parses an admin request given to this bot. The user should be admin
%% in order to use an admin request (this should be checked before running this function)
%%-----------------------------------------------------------------------------
parse_admin_request(Bot, Router, IrcData, "add " ++ Rest) ->
    error_logger:info_msg("[~s] Received admin command '~s' request with arguments: ~s~n", [get_bot_id(Bot), "add", string:strip(Rest, both)]),
    ArgumentParseList = string:tokens(Rest, " "),
    case length(ArgumentParseList) of
        1 ->
            Destination = IrcData#irc_data.destination,
            CommandArgs = IrcData#irc_data.command_args,
            Name = string:substr(CommandArgs, string:rchr(CommandArgs, $ ) + 1),
            case is_super_admin(IrcData#irc_data.sender) of
                true -> 
                    add_admin(?NORMAL_ADMIN, Name),
                    irc_helper:privmsg(Router, {Destination, "Added '" ++ Name ++ "' as normal admin."});
                _ ->
                    irc_helper:privmsg(Router, {Destination, "Only super admins may add other admins!"})
            end;
        _ ->
            send_signupbot_invalid_arguments(Router, IrcData#irc_data.destination)
    end;
parse_admin_request(Bot, Router, IrcData, "list" ++ Rest) ->
    error_logger:info_msg("[~s] Received admin command '~s' request with arguments: ~s~n", [get_bot_id(Bot), "list", string:strip(Rest, both)]),
    SuperAdmins = get_super_admins(),
    NormalAdmins = get_normal_admins(),
    Destination = IrcData#irc_data.sender,
    send_signupbot_list_to_irc_users(Router, Destination, SuperAdmins, "Super admins:", 10),
    send_signupbot_list_to_irc_users(Router, Destination, NormalAdmins, "Normal admins:", 10);
parse_admin_request(Bot, Router, IrcData, "remove " ++ Rest) ->
    error_logger:info_msg("[~s] Received admin command '~s' request with arguments: ~s~n", [get_bot_id(Bot), "remove", string:strip(Rest, both)]),
    ArgumentParseList = string:tokens(Rest, " "),
    case length(ArgumentParseList) of
        1 ->
            Destination = IrcData#irc_data.destination,
            CommandArgs = IrcData#irc_data.command_args,
            Name = string:substr(CommandArgs, string:rchr(CommandArgs, $ ) + 1),
            case is_super_admin(IrcData#irc_data.sender) of
                true ->
                    remove_admin(?NORMAL_ADMIN, Name),
                    irc_helper:privmsg(Router, {IrcData#irc_data.destination, "Removed " ++ Name ++ " as normal admin."});
                _ ->
                    irc_helper:privmsg(Router, {Destination, "Only super admins may remove other admins!"})
            end;
        _ ->
            send_signupbot_invalid_arguments(Router, IrcData#irc_data.destination)
    end;
parse_admin_request(_Bot, Router, IrcData, _Request) ->
    irc_helper:privmsg(Router, {IrcData#irc_data.sender, "Invalid administration command! Use the 'help' command to se available commands and parameters."}),
    not_a_valid_command.


%%-----------------------------------------------------------------------------
%% @spec announce_event(Bot, Router, IrcData, EventName, Message) -> void()
%%    Bot = #dikulanirc_bots{}
%%    Router = atom()
%%    IrcData = #irc_data{}
%%    EventName = Message = string()
%% @doc Announce the message for the given event.
%%-----------------------------------------------------------------------------
announce_event(Bot, Router, IrcData, EventName, Message) ->
    Q = qlc:q([E || E <- mnesia:table(signup_bot_event), E#signup_bot_event.name =:= EventName]),
    {atomic, Result} = mnesia:transaction(fun() -> qlc:e(Q) end),
    Destination = IrcData#irc_data.destination,
    case Result of
        % If R is an empty list, that means that there doesn't exist an event with that name.
        [] ->
            irc_helper:privmsg(Router, {Destination, "An event with that name doesn't exist!"});
        [Event | _] ->
            Owner = Event#signup_bot_event.owner,
            send_message_to_all_channels(Bot, Router, "Announcement for event '" ++ EventName ++ "' (owner: " ++ Owner ++ "):"),
            send_message_to_all_channels(Bot, Router, Message)
    end.

%%-----------------------------------------------------------------------------
%% @spec create_event(Router, IrcData, Event) -> void()
%%    Router = atom()
%%    IrcData = #irc_data{}
%%    Event = #signup_bot_event{}
%% @doc Creates the given event if there doesn't exist an event with the
%% same name already.
%%-----------------------------------------------------------------------------
create_event(Router, IrcData, Event) ->
    Pattern = #signup_bot_event{name = Event#signup_bot_event.name, _ = '_'},
    M = fun() ->
        Res = mnesia:match_object(Pattern),
        [{EventId} || #signup_bot_event{ event_id = EventId } <- Res]
    end,
    {atomic, R} = mnesia:transaction(M),
    case R of
        % If R is an empty list, that means that there doesn't exist an event with that name.
        % Else it already exists.
        [] ->
            F = fun() -> mnesia:write(Event) end,
            case mnesia:transaction(F) of
                {atomic, ok} -> irc_helper:privmsg(Router, {IrcData#irc_data.destination, "Event '" ++ Event#signup_bot_event.name ++ "' sucessfully created."});
                _ -> irc_helper:privmsg(Router, {IrcData#irc_data.destination, "Unable to create the desired event."})
            end;
        _ ->
            irc_helper:privmsg(Router, {IrcData#irc_data.destination, "An event with that name already exists, so cannot create it!"})
    end.

%%-----------------------------------------------------------------------------
%% @spec delete_event(Router, IrcData, EventName) -> void()
%%    Router = atom()
%%    IrcData = #irc_data{}
%%    EventName = string()
%% @doc Deletes the given event if the sender is owner or admin.
%%-----------------------------------------------------------------------------
delete_event(Router, IrcData, EventName) ->
    Q = qlc:q([E || E <- mnesia:table(signup_bot_event), E#signup_bot_event.name =:= EventName]),
    {atomic, Result} = mnesia:transaction(fun() -> qlc:e(Q) end),
    Destination = IrcData#irc_data.destination,
    Sender = IrcData#irc_data.sender,
    case Result of
        % If R is an empty list, that means that there doesn't exist an event with that name.
        [] ->
            irc_helper:privmsg(Router, {Destination, "An event with that name doesn't exist!"});
        [Event | _] ->
            Owner = Event#signup_bot_event.owner,
            case Owner =:= Sender orelse is_admin(Sender) of
                true ->
                    F = fun() -> mnesia:delete({signup_bot_event, Event#signup_bot_event.event_id}) end,
                    {atomic, ok} = mnesia:transaction(F),
                    irc_helper:privmsg(Router, {Destination, "Sucessfully deleted event '" ++ EventName ++ "'"});
                false ->
                    irc_helper:privmsg(Router, {Destination, "Cannot delete the given event, because you are not the owner or an admin!"})
            end
    end.

%%-----------------------------------------------------------------------------
%% @spec designup_event(Router, IrcData, EventName) -> void()
%%    Router = atom()
%%    IrcData = #irc_data{}
%%    EventName = string()
%% @doc Removes the senders (based on IrcData) signup for the given event
%%-----------------------------------------------------------------------------
designup_event(Router, IrcData, EventName) ->
    Pattern = #signup_bot_event{name = EventName, _ = '_'},
    M = fun() ->
        Res = mnesia:match_object(Pattern),
        [EventId || #signup_bot_event{ event_id = EventId } <- Res]
    end,
    {atomic, R} = mnesia:transaction(M),
    case R of
        % If R is an empty list, that means that there doesn't exist an event with that name.
        [] ->
            irc_helper:privmsg(Router, {IrcData#irc_data.destination, "No such event '" ++ EventName ++ "'!"});
        [EventId | _] ->
            F = fun() ->
                Signup = #signup_bot_signups{event_id = EventId, user = IrcData#irc_data.sender},
                mnesia:delete_object(Signup)
            end,
            mnesia:transaction(F),
            irc_helper:privmsg(Router, {IrcData#irc_data.destination, "User '" ++ IrcData#irc_data.sender ++ "' has removed his/her sign up for event '" ++ EventName ++ "'."})
    end.

%%-----------------------------------------------------------------------------
%% @spec signup_event(Router, IrcData, EventName) -> void()
%%    Router = atom()
%%    IrcData = #irc_data{}
%%    EventName = string()
%% @doc Signup the sender (based on IrcData) for the event with the given name.
%%-----------------------------------------------------------------------------
signup_event(Router, IrcData, EventName) ->
    Pattern = #signup_bot_event{name = EventName, _ = '_'},
    M = fun() ->
        Res = mnesia:match_object(Pattern),
        [EventId || #signup_bot_event{ event_id = EventId } <- Res]
    end,
    {atomic, R} = mnesia:transaction(M),
    case R of
        % If R is an empty list, that means that there doesn't exist an event with that name.
        [] ->
            irc_helper:privmsg(Router, {IrcData#irc_data.destination, "No such event '" ++ EventName ++ "'!"});
        [EventId | _] ->
            F = fun() ->
                NewSignup = #signup_bot_signups{event_id = EventId, user = IrcData#irc_data.sender},
                mnesia:write(NewSignup)
            end,
            {atomic, ok} = mnesia:transaction(F),
            irc_helper:privmsg(Router, {IrcData#irc_data.destination, "User '" ++ IrcData#irc_data.sender ++ "' has signed up for event '" ++ EventName ++ "'."})
    end.


%%-----------------------------------------------------------------------------
%% @spec send_message_to_all_channels(Bot, Router, Message) -> void()
%%    Bot = #dikulanirc_bots{}
%%    Router = atom()
%%    Message = string()
%% @doc Send the message to all channels the bot is a member of
%%-----------------------------------------------------------------------------
send_message_to_all_channels(Bot, Router, Message) ->
    send_message_to_all_channels_helper(Bot#dikulanirc_bots.channels, Router, Message).

send_message_to_all_channels_helper([], _, _) ->
    ok;
send_message_to_all_channels_helper([Head | Tail], Router, Message) ->
    irc_helper:privmsg(Router, {Head, Message}),
    send_message_to_all_channels_helper(Tail, Router, Message).


%%-----------------------------------------------------------------------------
%% @spec send_signupbot_help(Bot, Router, Destination) -> void()
%%    Bot = #dikulanirc_bots{}
%%    Router = atom()
%%    Destination = string()
%% @doc Send the documentation for the signup bot to destination
%%-----------------------------------------------------------------------------
send_signupbot_help(Bot, Router, Destination) ->
    case ?IRC_PRIVATE_COMMAND_SYMBOL of
        "" ->
            irc_helper:privmsg(Router, {Destination, "All commands written in a channel should be prefixed with '" ++ ?IRC_PUBLIC_COMMAND_SYMBOL ++ Bot#dikulanirc_bots.nick ++ "'."});
        _ ->
            irc_helper:privmsg(Router, {Destination, "All commands written in a channel should be prefixed with '" ++ ?IRC_PUBLIC_COMMAND_SYMBOL ++ Bot#dikulanirc_bots.nick ++ "' and private commands should be prefixed with '" ++ ?IRC_PRIVATE_COMMAND_SYMBOL ++ "'."})
        end,
    irc_helper:privmsg(Router, {Destination, "The following commands are available for normal users:"}),
    irc_helper:privmsg(Router, {Destination, ?IRC_TAB ++ "announce <event name> :<message> - Sends an announcement with message for the given event to all channels."}),
    irc_helper:privmsg(Router, {Destination, ?IRC_TAB ++ "create <event name> :<description> - Creates a new event with the given name and description."}),
    irc_helper:privmsg(Router, {Destination, ?IRC_TAB ++ "delete <event name> - Deletes the given event. You must be the owner of the event or admin to delete an event."}),
    irc_helper:privmsg(Router, {Destination, ?IRC_TAB ++ "designup <event name> - Removes your sign up for the given event."}),
    irc_helper:privmsg(Router, {Destination, ?IRC_TAB ++ "help - Shows the help information (this)."}),
    irc_helper:privmsg(Router, {Destination, ?IRC_TAB ++ "list - List all events and registered users for the events."}),
    irc_helper:privmsg(Router, {Destination, ?IRC_TAB ++ "signup <event name> :<bord nummer> - Sign up for the given event."}),
    irc_helper:privmsg(Router, {Destination, "The following commands are available to administrators:"}),
    irc_helper:privmsg(Router, {Destination, ?IRC_TAB ++ "add <name> - Adds the given name (which should be the nick of the user) as normal admin."}),
    irc_helper:privmsg(Router, {Destination, ?IRC_TAB ++ "list - List all the admins, both super and normal admins."}),
    irc_helper:privmsg(Router, {Destination, ?IRC_TAB ++ "remove <name> - Removes the given name (which should be the nick of the user) as normal admin."}).

%%-----------------------------------------------------------------------------
%% @spec send_signupbot_invalid_command(Router, Destination) -> void()
%%    Router = atom()
%%    Destination = string()
%% @doc Send invalid message to the given destination
%%-----------------------------------------------------------------------------
send_signupbot_invalid_arguments(Router, Destination) ->
    irc_helper:privmsg(Router, {Destination, "Invalid number of arguments! Use the 'help' command to see available commands and parameters."}).

%%-----------------------------------------------------------------------------
%% @spec send_signupbot_list(Router, Destination) -> void()
%%    Router = atom()
%%    Destination = string()
%% @doc Send the list of events for this bot
%%-----------------------------------------------------------------------------
send_signupbot_list(Router, Destination) ->
    Q = qlc:q([{E#signup_bot_event.event_id, E#signup_bot_event.name, E#signup_bot_event.description, E#signup_bot_event.owner} ||
        E <- mnesia:table(signup_bot_event)]),
    {atomic, Result} = mnesia:transaction(fun() -> qlc:e(Q) end),
    case Result of
        [] ->
            irc_helper:privmsg(Router, {Destination, "No events are active at the moment."});
        _ ->
            send_signupbot_list_to_irc(Router, Destination, Result)
    end.

%%-----------------------------------------------------------------------------
%% @spec send_signupbot_list_to_irc(Router, Destination, List) -> void()
%%    Router = atom()
%%    Destination = string()
%%    List = [{Name, Description, Owner}], Name = Description = Owner = string()
%% @doc Send the list of events for this bot. This function makes use of 2 helper
%% functions to generate the list of users and send them to the IRC server also.
%%-----------------------------------------------------------------------------
send_signupbot_list_to_irc(_, _, []) ->
    pass;
send_signupbot_list_to_irc(Router, Destination, [ {EventId, Name, Description, Owner} | Tail ]) ->
    % Send event information
    IrcString = Name ++ " (owner: " ++ Owner ++ ") - " ++ Description,
    irc_helper:privmsg(Router, {Destination, IrcString}),
    % Fetch all signups for the given event
    U = qlc:q([S#signup_bot_signups.user || S <- mnesia:table(signup_bot_signups),
        S#signup_bot_signups.event_id =:= EventId]),
    {atomic, Users} = mnesia:transaction(fun() -> qlc:e(U) end),
    % Send signup information for the event
    error_logger:info_msg("[~s] Creating IRC string for the list of registered users~n", [?MODULE]),
    send_signupbot_list_to_irc_users(Router, Destination, Users, "Registered users:", 10),
    % Recursive call
    send_signupbot_list_to_irc(Router, Destination, Tail).

%%-----------------------------------------------------------------------------
%% @spec send_signupbot_list_to_irc_users(Router, Destination, Users, Res, Iteration) -> void()
%%    Router = atom()
%%    Destination = string()
%%    Users = [N], N = {Name}, Name = string()
%%    Res = string()
%%    Iteration = integer()
%% @doc Sends a nicely formated IRC list of users. The max amount of users on each line is
%% equal to the number given as Iteration.
%%-----------------------------------------------------------------------------
send_signupbot_list_to_irc_users(Router, Destination, Users, Res, Iteration) ->
    send_signupbot_list_to_irc_users_helper(Router, Destination, Users, Res, Iteration, Iteration).

send_signupbot_list_to_irc_users_helper(Router, Destination, [], Res, _, _) ->
    irc_helper:privmsg(Router, {Destination, ?IRC_TAB ++ Res});
send_signupbot_list_to_irc_users_helper(Router, Destination, Users, Res, 0, MaxIteration) ->
    irc_helper:privmsg(Router, {Destination, ?IRC_TAB ++ Res}),
    send_signupbot_list_to_irc_users_helper(Router, Destination, Users, ?IRC_TAB ++ "", MaxIteration, MaxIteration);
send_signupbot_list_to_irc_users_helper(Router, Destination, [Head | Tail], Res, Iteration, MaxIteration) ->
    send_signupbot_list_to_irc_users_helper(Router, Destination, Tail, Res ++ " " ++ Head, Iteration - 1, MaxIteration).



