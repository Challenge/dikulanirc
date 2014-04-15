# Dependencies
The following dependencies have to be installed in order to execute the program
* make
* rebar (https://github.com/basho/rebar)
* erlang


# Compiling
To compile the program you need to have rebar installed. See https://github.com/basho/rebar for more information on how to install it for your distribution. When this program is installed the source code can be compile by running `make`.


# Running the application
To run the application you must first be able to compile the program. Also you need to have erlang installed in order to run the program. When the program has been compiled execute `make run`. The erlang shell should now be started. In order to start the application you have to type the following in the erlang shell:
```
dikulanirc_app:start([], []).
```
The first time the application is started nothing has been added yet. To add/remove/change networks, bots or nodes in the program see the section about controlling the application.


# Controlling the application
## Controlling networks
All the functions for controlling the network configuration is located in _dikulanirc_. The following functions are available to control the network configuration:
```erlang
network_add(Network, Host, Port) ->
Adds the network as Network with the given Host and Port (the connection is assumed to be unsecure).
Only one network can be added on the given identifer Network in the current implementation. Later on more Host and Ports should be added to the network with successive calls to this function.

network_add(Network, Host, Port, Ssl) ->
Adds the network as Network with the given Host and Port. If Ssl is set to be false it works just as the above funtion. If Ssl is true, then a secure connection will be made with the server (assuming it accepts it).
Only one network can be added on the given identifer Network in the current implementation. Later on more Host and Ports should be added to the network with successive calls to this function.

network_remove(Network)
Removes the network with the given identifier. This removes all Hosts and Ports for the given network.
NOT IMPLEMENTED!

network_get_info(Network) ->
Fetches information on the given network and prints it to the erlang shell. (NOT IMPLEMENT YET!)
```

## Controlling bots
All the functions for controlling the bot configuration is located in _dikulanirc_. The following functions are available to control the bot configuration:
```erlang
bot_create(Network, Nick, Channels, Module)
Creates a bot with the given Network identifer based on the Module given as input. The Module could be any bot located in src/bots. It will have the given Nick name and will join the given Channels. The Channels parameter must be passed as a list of strings, failure to do so will crash the bot (and make it unable to start at all).

bot_delete(BotId)
Deletes the bot with the given id.

bot_add_admin(BotId, Name)
Adds an admin with Name to the bot with the given id.
(The bot should be responsible for adding the admin, this function simply sends the correct event to the bot)

bot_remove_admin(BotId, Name)
Removes an admin with Name to the bot with the given id.
(The bot should be responsible for removing the admin, this function simply sends the correct event to the bot)

bot_enable(BotId)
Enables the given bot if it is disabled (otherwise nothing happens). Also starts the given bot.

bot_disable(BotId)
Disables the given bot if it is enabled (otherwise nothing happens). Also stops the given bot.

bot_restart(BotId)
Restarts the given bot if it is enabled (otherwise nothing happens).

bot_get_all()
Fetches all the configured bots and print them to the erlang shell. (NOT IMPLEMENT YET!)
```


# Developing new bots
To develop new bots the source file for the bot should be placed in `src/bots`, if a header file is need it should be placed in `include`. All database records should be prefixed with the name of the bot module.

For example if the bot is called `foo` there should at least be the following source file `src/bots/foo.erl`. If a header file for `foo` is need it should be located at `include/foo.hrl`. Furthermore if the module `foo` needed a mnesia database record it could be called `foo_bar` or `foo_baz`. The bot should have the following files included in addition to the other header files need:
```erlang
-include_lib("dikulanirc.hrl").
-include_lib("db_records.hrl").
-include_lib("irc_records.hrl").
-include_lib("irc_constants.hrl").
```

The bot should accept the following events (in the current state):
```erlang
?IRC_CONNECTED
This will be received when the bot has been connected to the IRC network sucessfully.

?IRC_RECEIVE
This will be used whenever a message is received for the given bot (this includes all messages for channels the bot is part of).
```

When the bot wants to send IRC messages the following event should be used:
```erlang
?IRC_SEND_DATA
Should be used when sending RAW IRC messages in order for them to be sent to the IRC server.
```

The bot should accept the following events (in all states state, i.e. handle_event/3 will be used):
```erlang
{?EVENT_ADD_ADMIN, Name}
This event is sent when an admin should be added to the bot. How the bot stores this information is up to the individual bot.

{?EVENT_REMOVE_ADMIN, Name}
This event is sent when an admin should be removed from the bot.
```

The `irc_helper` library can be used for many IRC interactions, for example registering to the IRC server, join channels and much more.

In addition to the above the bot should use the following global macro definitions:
```erlang
?IRC_NICK_SUFFIX
This is used when the given nick name is already taken and should be appended to the nick of the bot to form the new nick name.

?IRC_PUBLIC_COMMAND_SYMBOL
This is the symbol(s) used for giving global commands to a bot, i.e. messages typed in the channels the bot has joined.

?IRC_PRIVATE_COMMAND_SYMBOL
This is the symbol(s) used for giving private commands to a bot, i.e. direct private messages to the bot.

?IRC_TAB
This macro should be used when the bot wants to make indentations in the RAW IRC message.
```

## Debugging bots
The application can be run in debug mode in order to find bugs in newly created bots. In order to do so run the program with `make debug` instead of the normal `make run` command. Everything else (like starting the application) remains the same. The makes all exceptions appear in the erlang shell and also prints messages from the `error_logger` module.





