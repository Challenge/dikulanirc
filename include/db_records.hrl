%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc This header file contains the data records used in Mnesia Database
%% -------------------------------------------------------------------

%%---------------------------------------------------------------------
%% @doc Used to store information needed for individual bots
%% Data Type: Bot
%% where:
%%    bot_id: Integer determining the id of the bot (default is undefined)
%%    network: The network (identifier/name) the bot should connect to (default is undefined)
%%    nick: A string containing the nick name of the bot (default is undefined)
%%    channels: A list of channels the bot is connected to on the given network
%%                (default is undefined)
%%    module: The module used to spawn and operate the bot (default is undefined)
%%    enabled: Whether the bot is enabled or not (default is true)
%%----------------------------------------------------------------------
-record(dikulanirc_bots, {
        bot_id,
        network,
        nick,
        channels,
        module,
        enabled = true}).

%%---------------------------------------------------------------------
%% @doc Used to store information about the given IRC network
%% Data Type: IRC server
%% where:
%%    network: The network identifier (name of the network) (default is undefined)
%%    host: The hostname or IP address of the IRC network (default is undefined)
%%    port: The port number the IRC network is listening on (default is 6667)
%%    ssl: Whether or not the server specification is ssl or not (default is false)
%%    enabled: Whether the IRC network is enabled or not (default is true)
%%----------------------------------------------------------------------
-record(dikulanirc_networks, {
        network,
        host,
        port = 6667,
        ssl = false,
        enabled = true}).



