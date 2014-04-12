%% -------------------------------------------------------------------
%% @author Ektorus
%% @doc Module used to handle all the nodes in the distributed setup
%% -------------------------------------------------------------------
-module(dikulanirc_node_manager).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State definition
-record(state, { nodes = [] }).

%% TODO:
%% Add all nodes to database instead of just keeping them in the state
%% (which is lost on shutdown)
%% Create functionality to add and remove nodes while the program is running

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Nodes) ->
    error_logger:info_msg("[~s] Starting node manager~n", [?MODULE]),
    gen_server:start_link({global, ?MODULE}, ?MODULE, Nodes, []).

%%-----------------------------------------------------------------------------
%% @spec init(State) -> {ok,State} | {ok,State,Timeout} | {ok,State,hibernate}
%%    | {stop,Reason} | ignore
%% @doc Whenever a gen_server is started using gen_server:start/3,4 or 
%% gen_server:start_link/3,4, this function is called by the new process to initialize.
%%-----------------------------------------------------------------------------
init(Nodes) ->
    {ok, #state{ nodes = Nodes }}.

%%-----------------------------------------------------------------------------
%% @spec handle_call(Request, From, State) ->
%%    {reply,Reply,NewState} | {reply,Reply,NewState,Timeout} | {reply,Reply,NewState,hibernate}
%%    | {noreply,NewState} | {noreply,NewState,Timeout} | {noreply,NewState,hibernate}
%%    | {stop,Reason,Reply,NewState} | {stop,Reason,NewState}
%% @doc Whenever a gen_server receives a request sent using gen_server:call/2,3 or 
%% gen_server:multi_call/2,3,4, this function is called to handle the request.
%%-----------------------------------------------------------------------------
handle_call(node_get_all, _From, State) ->
    Reply = {ok, State#state.nodes},
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
handle_cast({node_add, Node}, State) ->
    % TODO: Should also add to database instead of keeping only in internal state!
    NewState = State#state{ nodes = [State#state.nodes | Node] },
    {noreply, NewState};
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



