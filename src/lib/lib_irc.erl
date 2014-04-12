%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc Library used for communicating with the IRC server.
%% This module formats the given command to match the IRC format.
%% This module only contains a certain subset of IRC commands,
%% for the full list of IRC commands consult http://en.wikipedia.org/wiki/List_of_Internet_Relay_Chat_commands
%% -------------------------------------------------------------------
-module(lib_irc).
-include_lib("db_records.hrl").
-include_lib("irc_records.hrl").
-include_lib("irc_constants.hrl").

%% API
-export([parse_irc/3]).
-export([join/1, kick/3, mode/2, nick/1, oper/2, pong/1, privmsg/2, user/3, whois/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%-----------------------------------------------------------------------------
%% @spec join(ListOfChannels) -> string()
%%    ListOfChannels = [S]
%%    S = string()
%% @doc This function takes a list of channels to join. It removes any leading
%% and tailing whitespaces in the list and constructs a IRC JOIN command string.
%%-----------------------------------------------------------------------------
join(Channels) ->
    StrippedChannels = lists:map(fun(Str) -> string:strip(Str, both) end, Channels),
    "JOIN " ++ string:join(StrippedChannels, ",").

%% -------------------------------------------------------------------
%% @spec kick(Nick, Channel, Reason) -> string()
%%    Nick = Channel = Reason = string()
%% @doc Construct the IRC KICK command.
%% Kick 'Nick' from 'Channel' because 'Reason'.
%% -------------------------------------------------------------------
kick(Nick, Channel, "") ->
    kick(Nick, Channel, Nick);
kick(Nick, Channel, Reason) ->
    "KICK " ++ Channel ++ " " ++ Nick ++ " :" ++ Reason.

%% -------------------------------------------------------------------
%% @spec mode(Channel, Mode) -> string()
%%    Channel = Mode = string()
%% @doc Construct the IRC MODE command.
%% Changes mode on the given channel.
%% -------------------------------------------------------------------
mode(Channel, Mode) ->
    "MODE " ++ Channel ++ " " ++ Mode.

%%-----------------------------------------------------------------------------
%% @spec nick(Nick) -> string()
%%    Nick = string()
%% @doc This function construct the IRC NICK command.
%% Chages the nickname of the user.
%%-----------------------------------------------------------------------------
nick(Nick) ->
    "NICK " ++ Nick.

%%-----------------------------------------------------------------------------
%% @spec nick(UserId, Password) -> string()
%%    UserId = Password = string()
%% @doc This function construct the IRC OPER command.
%% Gets operator status on the given server.
%%-----------------------------------------------------------------------------
oper(UserId, Password) ->
    "OPER " ++ UserId ++ " " ++ Password.

%%-----------------------------------------------------------------------------
%% @spec pong(Data) -> string()
%%    Data = string()
%% @doc Construct the IRC PONG command.
%% Respond to PING command.
%%-----------------------------------------------------------------------------
pong(Data) ->
    "PONG " ++ Data.

%%-----------------------------------------------------------------------------
%% @spec privmsg(Dest, Msg) -> string()
%%    Dest = Msg = string()
%% @doc Construct the IRC PRIVMSG command.
%% Send message to user or channel.
%%-----------------------------------------------------------------------------
privmsg(Dest, Msg) ->
    "PRIVMSG " ++ Dest ++ " :" ++ Msg.

%%-----------------------------------------------------------------------------
%% @spec user(Nick, Mask, RealName) -> string()
%%    Nick = Mask = RealName = string()
%% @doc This function construct the IRC USER command given the user/nick name,
%% the mask the user should connect with and the real name of the user.
%%-----------------------------------------------------------------------------
user(User, Mask, RealName) ->
    "USER " ++ User ++ " " ++ Mask ++ " * " ++ ":" ++ RealName.

%% -------------------------------------------------------------------
%% @spec whois(Nick) -> string()
%%    Nick = string()
%% @doc Construct the IRC WHOIS command.
%% Lookup user information for the given nick.
%% -------------------------------------------------------------------
whois(Nick) ->
    "WHOIS " ++ Nick.

%%-----------------------------------------------------------------------------
%% @spec parse_irc(Bot, IrcPacket) -> #irc_data{}
%%    Bot = #dikulanirc_bots{}
%%    IrcPacket = string()
%% @doc Parses a packet line from the IRC communcation and returns a record
%% structure (irc_data) with the parsed data.
%%-----------------------------------------------------------------------------
parse_irc(_Bot, _DateTime, "PING " ++ Args) ->
    #irc_data{command = ?PING, command_args = Args};
parse_irc(_Bot, _DateTime, "ERROR :" ++ Args) ->
    #irc_data{command = ?ERROR, command_args = Args};
parse_irc(Bot, DateTime, ":" ++ Msg) ->
    Args = string:substr(Msg, string:chr(Msg, $:) + 1),
    IrcParseList = string:tokens(Msg, " "),
    Sender = case string:tokens(hd(IrcParseList), "!") of
        [S] -> S;
        [S | _] -> S
    end,
    IrcData = case length(IrcParseList) of
        1 ->
            #irc_data{
                command = undefined,
                command_args = Args,
                sender = Sender,
                destination = undefined,
                time = DateTime
            };
        2 ->
            #irc_data{
                command = ?COMMAND_CODE(lists:nth(2, IrcParseList)),
                command_args = Args,
                sender = Sender,
                destination = undefined,
                time = DateTime
            };
        _ ->
            #irc_data{
                command = ?COMMAND_CODE(lists:nth(2, IrcParseList)),
                command_args = Args,
                sender = Sender,
                destination = lists:nth(3, IrcParseList),
                time = DateTime
            }
    end,
    %% If we get a private message the destination is ourself,
    %% so we need to change it to the senders nickname instead.
    case IrcData#irc_data.destination =:= Bot#dikulanirc_bots.nick of
        true -> IrcData#irc_data{destination = IrcData#irc_data.sender};
        false -> IrcData
    end;
parse_irc(_, _, Msg) ->
    error_logger:error_msg("[~s] Received unknown IRC data string '~s'!~n", [?MODULE, Msg]),
    #irc_data{}.


%% ===================================================================
%% Internal implementation
%% ===================================================================





