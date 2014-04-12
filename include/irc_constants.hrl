%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc Constants used for IRC communication and events between the
%% router and the given bot.
%% The communcation constants has been taken from https://gist.github.com/jteeuwen/5529647.
%% -------------------------------------------------------------------

%% Constants used for general IRC behavior
-define(IRC_NICK_SUFFIX, "_").
-define(IRC_PUBLIC_COMMAND_SYMBOL, "!").
-define(IRC_PRIVATE_COMMAND_SYMBOL, "").

-define(IRC_TAB, "    ").

%% Constants used by the bot to send requests to the router
-define(IRC_SEND_DATA, irc_send_data).

%% Constants used by the router to send events to the bot
-define(IRC_CONNECTED, irc_connected).
-define(IRC_RECEIVE, irc_receive).


%% IRC replies on connect
-define(R_WELCOME, 1).
-define(R_YOUR_HOST, 2).
-define(R_SERVER_CREATED, 3).
-define(R_MY_INFO, 4).
-define(R_SERVER_BOUND, 5).

%% IRC replies
-define(R_TRACE_LINK, 200).
-define(R_TRACE_CONNECTING, 201).
-define(R_TRACE_HANDSHAKE, 202).
-define(R_TRACE_UNKNOWN, 203).
-define(R_TRACE_OPERATOR, 204).
-define(R_TRACE_USER, 205).
-define(R_TRACE_SERVER, 206).
-define(R_TRACE_SERVICE, 207).
-define(R_TRACE_NEWTYPE, 208).
-define(R_TRACE_CLASS, 209).
-define(R_TRACE_RECONNECT, 210).
-define(R_STATS_LINKINFO, 211).
-define(R_STATS_COMMANDS, 212).
-define(R_STATS_CLINE, 213).
-define(R_STATS_ILINE, 215).
-define(R_STATS_QLINE, 217).
-define(R_STATS_END, 219).
-define(R_USER_MODE_IS, 221).
-define(R_SERVICE_LIST, 234).
-define(R_SERVICE_LIST_END, 235).
-define(R_STATS_VLINE, 240).
-define(R_STATS_UPTIME, 242).
-define(R_STATS_OLINE, 243).
-define(R_STATS_HLINE, 244).
-define(R_STATS_PING, 246).
-define(R_STATS_DLINE, 250).
-define(R_LIST_USER_CLIENT, 251).
-define(R_LIST_USER_OPERATOR, 252).
-define(R_LIST_USER_UNKNOWN, 253).
-define(R_LIST_USER_CHANNELS, 254).
-define(R_LIST_USER_ME, 255).
-define(R_ADMIN_ME, 256).
-define(R_ADMIN_LOC1, 257).
-define(R_ADMIN_LOC2, 258).
-define(R_ADMIN_EMAIL, 259).
-define(R_TRACE_LOG, 261).
-define(R_TRACE_END, 262).
-define(R_TRY_AGAIN, 263).
-define(R_AWAY, 301).
-define(R_USER_HOST, 302).
-define(R_IS_ON, 303).
-define(R_UN_AWAY, 305).
-define(R_NOW_AWAY, 306).
-define(R_WHO_IS_USER, 311).
-define(R_WHO_IS_SERVER, 312).
-define(R_WHO_IS_OPERATOR, 313).
-define(R_WHO_WAS_USER, 314).
-define(R_END_OF_WHO, 315).
-define(R_WHO_IS_IDLE, 317).
-define(R_END_OF_WHO_IS, 318).
-define(R_WHO_IS_CHANNELS, 319).
-define(R_LIST_START, 321).
-define(R_LIST, 322).
-define(R_LIST_END, 323).
-define(R_CHANNEL_MODE_IS, 324).
-define(R_UNIQ_OP_IS, 325).
-define(R_NO_TOPIC, 331).
-define(R_TOPIC, 332).
-define(R_INVITING, 341).
-define(R_SUMMONING, 342).
-define(R_INVITE_LIST, 346).
-define(R_END_OF_INVITE_LIST, 347).
-define(R_EXCEPT_LIST, 348).
-define(R_END_OF_EXCEPT_LIST, 349).
-define(R_VERSION, 351).
-define(R_WHO_REPLY, 352).
-define(R_NAME_REPLY, 353).
-define(R_LINKS, 364).
-define(R_END_OF_LINKS, 365).
-define(R_END_OF_NAMES, 366).
-define(R_BAN_LIST, 367).
-define(R_END_OF_BAN_LIST, 368).
-define(R_END_OF_WHO_WAS, 369).
-define(R_INFO, 371).
-define(R_MOTD, 372).
-define(R_END_OF_INFO, 374).
-define(R_MOTD_START, 375).
-define(R_END_OF_MOTD, 376).
-define(R_YOU_ARE_OPER, 381).
-define(R_REHASHING, 382).
-define(R_YOU_ARE_SERVICE, 383).
-define(R_TIME, 391).
-define(R_USER_START, 392).
-define(R_USERS, 393).
-define(R_END_OF_USERS, 394).
-define(R_NO_USERS, 395).
-define(R_HOST_HIDDEN, 396).

%% IRC errors
-define(ERROR_NO_SUCH_NICK, 401).
-define(ERROR_NO_SUCH_SERVER, 402).
-define(ERROR_NO_SUCH_CHANNEL, 403).
-define(ERROR_CANNOT_SEND_TO_CHANNEL, 404).
-define(ERROR_TOO_MANY_CHANNELS, 405).
-define(ERROR_WAS_NO_SUCH_NICK, 406).
-define(ERROR_TOO_MANY_TARGETS, 407).
-define(ERROR_NO_SUCH_SERVICE, 408).
-define(ERROR_NO_ORIGIN, 409).
-define(ERROR_NO_RECIPIENT, 411).
-define(ERROR_NO_TEXT_TO_SEND, 412).
-define(ERROR_NO_TOP_LEVEL, 413).
-define(ERROR_WILD_TOP_LEVEL, 414).
-define(ERROR_BAD_MASK, 415).
-define(ERROR_UNKNOWN_COMMAND, 421).
-define(ERROR_NO_MOTD, 422).
-define(ERROR_NO_ADMIN_INFO, 423).
-define(ERROR_FILE_ERROR, 424).
-define(ERROR_NO_NICKNAME_GIVEN, 431).
-define(ERROR_ERRONEUS_NICKNAME, 432).
-define(ERROR_NICKNAME_IN_USE, 433).
-define(ERROR_NICK_COLLISION, 436).
-define(ERROR_UNAVAILABLE_RESOURCE, 437).
-define(ERROR_USER_NOT_IN_CHANNEL, 441).
-define(ERROR_NOT_ON_CHANNEL, 442).
-define(ERROR_USER_ON_CHANNEL, 443).
-define(ERROR_NO_LOGIN, 444).
-define(ERROR_SUMMON_DISABLED, 445).
-define(ERROR_USER_DISABLED, 446).
-define(ERROR_NOT_REGISTERED, 451).
-define(ERROR_NEED_MORE_PARAMS, 461).
-define(ERROR_ALREADY_REGISTERED, 462).
-define(ERROR_NO_PERM_FOR_HOST, 463).
-define(ERROR_PASSWORD_MISMATCH, 464).
-define(ERROR_YOU_ARE_BANNED, 465).
-define(ERROR_YOU_WILL_BE_BANNED, 466).
-define(ERROR_KEY_SET, 467).
-define(ERROR_CHANNEL_IS_FULL, 471).
-define(ERROR_UNKNOWN_MODE, 472).
-define(ERROR_INVITE_ONLY_CHANNEL, 473).
-define(ERROR_BANNED_FROM_CHANNEL, 474).
-define(ERROR_BAD_CHANNEL_KEY, 475).
-define(ERROR_BAD_CHANNEL_MASK, 476).
-define(ERROR_NO_CHANNEL_MODES, 477).
-define(ERROR_BANLIST_FULL, 478).
-define(ERROR_NO_PRIVILEGES, 481).
-define(ERROR_CHANNEL_OPRIVS_NEEDED, 482).
-define(ERROR_CANNOT_KILL_SERVER, 483).
-define(ERROR_RESTRICTED, 484).
-define(ERROR_UNIQ_OPRIVS_NEEDED, 485).
-define(ERROR_NO_OPER_HOST, 491).
-define(ERROR_NO_SERVICE_HOST, 492).
-define(ERROR_USERMODE_UNKNOWN_FLAG, 501).
-define(ERROR_USERS_DO_NOT_MATCH, 502).

%% IRC command identifiers.
%% These are most often send as string (the name of the command) instead of numeric
-define(ADMIN, 801).
-define(AWAY, 802).
-define(CONNECT, 803).
-define(DIE, 804).
-define(ERROR, 805).
-define(INFO, 806).
-define(INVITE, 807).
-define(IS_ON, 808).
-define(JOIN, 809).
-define(KICK, 810).
-define(KILL, 811).
-define(LINKS, 812).
-define(LIST, 813).
-define(LUSERS, 814).
-define(MODE, 815).
-define(MOTD, 816).
-define(NAMES, 817).
-define(NICK, 818).
-define(NJOIN, 819).
-define(NOTICE, 820).
-define(OPER, 821).
-define(PART, 822).
-define(PASS, 823).
-define(PING, 824).
-define(PONG, 825).
-define(PRIVMSG, 826).
-define(QUIT, 827).
-define(REHASH, 828).
-define(RESTART, 829).
-define(SERVER, 830).
-define(SERVICE, 831).
-define(SERVLIST, 832).
-define(SQUERY, 833).
-define(SQUIRT, 834).
-define(SQUIT, 835).
-define(STATS, 836).
-define(SUMMON, 837).
-define(TIME, 838).
-define(TOPIC, 839).
-define(TRACE, 840).
-define(USER, 841).
-define(USERHOST, 842).
-define(USERS, 843).
-define(VERSION, 844).
-define(WALLOPS, 845).
-define(WHO, 846).
-define(WHOIS, 847).
-define(WHOWAS, 848).

%% COMMAND_CODE converts the name of a command into it's numeric identifier
-define(COMMAND_CODE(Name),
    case Name of
    "PRIVMSG"  -> ?PRIVMSG;
    "JOIN"     -> ?JOIN;
    "PART"     -> ?PART;
    "KICK"     -> ?KICK;
    "NOTICE"   -> ?NOTICE;
    "ADMIN"    -> ?ADMIN;
    "AWAY"     -> ?AWAY;
    "CONNECT"  -> ?CONNECT;
    "DIE"      -> ?DIE;
    "ERROR"    -> ?ERROR;
    "INFO"     -> ?INFO;
    "INVITE"   -> ?INVITE;
    "ISON"     -> ?IS_ON;
    "KILL"     -> ?KILL;
    "LINKS"    -> ?LINKS;
    "LIST"     -> ?LIST;
    "LUSERS"   -> ?LUSERS;
    "MODE"     -> ?MODE;
    "MOTD"     -> ?MOTD;
    "NAMES"    -> ?NAMES;
    "NICK"     -> ?NICK;
    "NJOIN"    -> ?NJOIN;
    "OPER"     -> ?OPER;
    "PASS"     -> ?PASS;
    "PING"     -> ?PING;
    "PONG"     -> ?PONG;
    "QUIT"     -> ?QUIT;
    "REHASH"   -> ?REHASH;
    "RESTART"  -> ?RESTART;
    "SERVER"   -> ?SERVER;
    "SERVICE"  -> ?SERVICE;
    "SERVLIST" -> ?SERVLIST;
    "SQUERY"   -> ?SQUERY;
    "SQUIRT"   -> ?SQUIRT;
    "SQUIT"    -> ?SQUIT;
    "STATS"    -> ?STATS;
    "SUMMON"   -> ?SUMMON;
    "TIME"     -> ?TIME;
    "TOPIC"    -> ?TOPIC;
    "TRACE"    -> ?TRACE;
    "USER"     -> ?USER;
    "USERHOST" -> ?USERHOST;
    "USERS"    -> ?USERS;
    "VERSION"  -> ?VERSION;
    "WALLOPS"  -> ?WALLOPS;
    "WHO"      -> ?WHO;
    "WHOIS"    -> ?WHOIS;
    "WHOWAS"   -> ?WHOWAS;
    %% If the command doesn't match any of the above,
    %% that means that the given input is already a code
    %% and not the name of a command, so just return it as is.
    _ -> list_to_integer(Name)
    end
).







