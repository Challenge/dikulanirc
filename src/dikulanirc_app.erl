%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc The signup bot application for starting all other services
%% -------------------------------------------------------------------
-module(dikulanirc_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%%-----------------------------------------------------------------------------
%% @spec start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State}
%% @doc start is called when starting the application and should create the 
%% supervision tree by starting the top supervisor. It is expected to return the
%% pid of the top supervisor and an optional term State, which defaults to [].
%% This term is passed as-is to stop.
%%-----------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    dikulanirc_sup:start_link().

%%-----------------------------------------------------------------------------
%% @spec stop(State) -> void()
%% @doc stop/1 is called after the application has been stopped and should do 
%% any necessary cleaning up. Note that the actual stopping of the application,
%% that is the shutdown of the supervision tree, is handled automatically as 
%% described in Starting and Stopping Applications.
%%-----------------------------------------------------------------------------
stop(_State) ->
    exit(whereis(dikulanirc_sup), shutdown).

