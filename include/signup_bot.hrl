%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc This header file contains record definitions to be used in
%% the module signup_bot.
%% -------------------------------------------------------------------

%% Admin roles
-define(SUPER_ADMIN, super_admin).
-define(NORMAL_ADMIN, normal_admin).

%% Announcement delay
-define(SIGNUPBOT_ANNOUNCEMENT_DELAY, 300000). % 5 Minutes
-define(SIGNUPBOT_ANNOUNCEMENT_DELAY_IN_SECONDS, ?SIGNUPBOT_ANNOUNCEMENT_DELAY / 1000).
-define(SIGNUPBOT_ANNOUNCEMENT_DELAY_IN_MINUTES, ?SIGNUPBOT_ANNOUNCEMENT_DELAY_IN_SECONDS / 60).

%% Login information
-define(SIGNUPBOT_LOGIN_NICKNAME, "").
-define(SIGNUPBOT_LOGIN_PASSWORD, "").

%%---------------------------------------------------------------------
%% @doc Used to store unique id's for auto increment feature
%% Data Type: Id (set)
%% where:
%%    type: An identifier/type to store the id in (default is undefined)
%%    id: The stored auto increment id (default is undefined)
%%----------------------------------------------------------------------
-record(unique_ids, {
        type,
        id}).

%%---------------------------------------------------------------------
%% @doc Used to store information about events
%% Data Type: Events (set)
%% where:
%%    event_id: The id of the event (default is undefined)
%%    name: The name of the event (default is undefined)
%%    description: User-made description of this event (default is undefined)
%%    owner: The owner of the signup (default is undefined)
%%    users: A list of all the registered users for this signup (default is [])
%%----------------------------------------------------------------------
-record(signup_bot_event, {
        event_id,
        name,
        description,
        owner,
        unix_time}).

%%---------------------------------------------------------------------
%% @doc Used to store signup information
%% Data Type: Signup (bag)
%% where:
%%    event_id: The id of the event (default is undefined)
%%    user: A list of all the registered users for this signup (default is undefined)
%%----------------------------------------------------------------------
-record(signup_bot_signups, {
        event_id,
        user}).

%%---------------------------------------------------------------------
%% @doc Used to store the administrators of the bot
%% Data Type: Admins (bag)
%% where:
%%    role: The role of the given admin, should be one of the types
%%        defined in the start of this file (default is undefined)
%%    name: The name of the administrator (default is undefined)
%%----------------------------------------------------------------------
-record(signup_bot_admins, {
        role,
        name}).

%%---------------------------------------------------------------------
%% @doc Used to store the announcements for events
%% Data Type: Announcements (Set)
%% where:
%%    event_id: The id of the event (default is undefined)
%%    announcement: The (complete) announcement text for the event (default is undefined)
%%----------------------------------------------------------------------
-record(signup_bot_announcements, {
        event_id,
        announcement}).





