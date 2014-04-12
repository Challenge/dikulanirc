%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc Module used to handle all the networks a bot can connect to
%% -------------------------------------------------------------------
-module(dikulanirc_network_manager).
-behaviour(gen_server).

-include_lib("dikulanirc.hrl").
-include_lib("db_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% TODO:
%% Create functionality to enable and disable netowrks, just like for bots.
%% Create functionality to remove netowrks completely.
%% Create functionality to change/add/remove just some of the network, like port and host


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    error_logger:info_msg("[~s] Starting network manager~n", [?MODULE]),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%%-----------------------------------------------------------------------------
%% @spec init(State) -> {ok,State} | {ok,State,Timeout} | {ok,State,hibernate}
%%    | {stop,Reason} | ignore
%% @doc Whenever a gen_server is started using gen_server:start/3,4 or 
%% gen_server:start_link/3,4, this function is called by the new process to initialize.
%%-----------------------------------------------------------------------------
init(State) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% @spec handle_call(Request, From, State) ->
%%    {reply,Reply,NewState} | {reply,Reply,NewState,Timeout} | {reply,Reply,NewState,hibernate}
%%    | {noreply,NewState} | {noreply,NewState,Timeout} | {noreply,NewState,hibernate}
%%    | {stop,Reason,Reply,NewState} | {stop,Reason,NewState}
%% @doc Whenever a gen_server receives a request sent using gen_server:call/2,3 or 
%% gen_server:multi_call/2,3,4, this function is called to handle the request.
%%-----------------------------------------------------------------------------
handle_call({network_get_info, Network}, _From, State) ->
    Q = qlc:q([{N#dikulanirc_networks.host, N#dikulanirc_networks.port, N#dikulanirc_networks.ssl} ||
        N <- mnesia:table(dikulanirc_networks), N#dikulanirc_networks.network =:= Network]),
    {atomic, Result} = mnesia:transaction(fun() -> qlc:e(Q) end),
    Reply = case Result of
        [] ->
            {error, not_found};
        _ ->
            {ok, Result}
    end,
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    error_logger:info_msg("[~s] Unhandled Call Request:~n~w~n", [?MODULE, Request]),
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% @spec handle_cast(Request, State) ->
%%    {noreply,NewState} | {noreply,NewState,Timeout} | {noreply,NewState,hibernate}
%%    | {stop,Reason,NewState}
%% @doc Whenever a gen_server receives a request sent using gen_server:cast/2 or 
%% gen_server:abcast/2,3, this function is called to handle the request.
%%-----------------------------------------------------------------------------
handle_cast({network_add, Network, {Host, Port, Ssl}}, State) ->
    error_logger:info_msg("[~s] network_add - Network: ~s, Host: ~s, Port: ~B~n", [?MODULE, Network, Host, Port]),
    F = fun() -> mnesia:write(#dikulanirc_networks{
            network = Network,
            host = Host,
            port = Port,
            ssl = Ssl
        }) end,
    {atomic, ok} = mnesia:transaction(F),
    {noreply, State};
handle_cast(Request, State) ->
    error_logger:info_msg("[~s] Unhandled Cast Request:~n~w~n", [?MODULE, Request]),
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply,NewState} | {noreply,NewState,Timeout} | {noreply,NewState,hibernate} | {stop,Reason,NewState}
%% @doc This function is called by a gen_server when a timeout occurs or when it 
%% receives any other message than a synchronous or asynchronous request (or a system message).
%%-----------------------------------------------------------------------------
handle_info(Info, _State) ->
    error_logger:info_msg("[~s] Unhandled Info Request:~n~w~n", [?MODULE, Info]),
    ok.

%%-----------------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to terminate.
%% It should be the opposite of Module:init/1 and do any necessary cleaning up.
%% When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%-----------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%-----------------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState} | {error, Reason}
%% @doc This function is called by a gen_server when it should update its internal
%% state during a release upgrade/downgrade, i.e. when the instruction 
%% {update,Module,Change,...} where Change={advanced,Extra} is given in the appup file.
%% See OTP Design Principles for more information.
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



