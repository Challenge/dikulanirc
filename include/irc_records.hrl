%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc This header file contains records used for IRC data.
%% -------------------------------------------------------------------

%%---------------------------------------------------------------------
%% @doc Used to store parsed irc commands in a data record
%% Data Type: Irc data
%% where:
%%    command: The parsed IRC command (default is undefined)
%%    command_args: The arguments to the IRC command (default is undefined)
%%    sender: The person (nick) that send the command (default is undefined)
%%    destination: The receiver of the IRC message (default is undefined)
%%    time: The unix time the command was sent (default is undefined)
%%----------------------------------------------------------------------
-record(irc_data, {
        command,
        command_args,
        sender,
        destination,
        time}).





