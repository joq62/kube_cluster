%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Manage hosts and ensure that kubelet are loaded and running
%%% 
%%% Created : 
%%% -------------------------------------------------------------------
-module(cluster_server).  
 
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube_logger.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state, {cluster_status}).



%% --------------------------------------------------------------------
%% Definitions 
%-define(WantedStateInterval,60*1000).
-define(ClusterStatusInterval,1*60*1000).
%% --------------------------------------------------------------------



%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================




%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------

% To be removed

init([]) ->
    ?PrintLog(log,"Start init",[?FUNCTION_NAME,?MODULE,?LINE]),
    % Loads dbase host and cluster info
   % ?PrintLog(log,"1/8 load cluster and hosts and deployment info",[?FUNCTION_NAME,?MODULE,?LINE]),
   % ?PrintLog(log,"2/8 Starts ssh ",[ssh:start(),?FUNCTION_NAME,?MODULE,?LINE]),
    
    ssh:start(),
   % {ok,StartResult,HostWithOutKubeletResult}=cluster_lib:strive_desired_state(),
   
   % ?PrintLog(log," HostWithOutKubeletResult ",[HostWithOutKubeletResult,?FUNCTION_NAME,?MODULE,?LINE]),
   % ?PrintLog(log," StartResult",[StartResult,?FUNCTION_NAME,?MODULE,?LINE]),
  
    spawn(fun()->cluster_status_interval() end),    

    ?PrintLog(log,"STARTED SERVER",[node(),?FUNCTION_NAME,?MODULE,?LINE]),
    {ok, #state{cluster_status=undefined}}.
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------

handle_call({running_hosts},_From,State) ->
    Reply=rpc:call(node(),host,running,[],20*1000),
    {reply, Reply, State};
handle_call({missing_hosts},_From,State) ->
    Reply=rpc:call(node(),host,missing,[],20*1000),
    {reply, Reply, State};
%%------ Standard

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call({ping},_From,State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% -------------------------------------------------------------------


handle_cast({cluster_status,Status}, State) ->
    ChangedStatus=Status/=State#state.cluster_status,
    NewState=case ChangedStatus of
	       false->
		   State;
	       true->
		   ?PrintLog(log,"Changed cluster_status  = ",[Status,?FUNCTION_NAME,?MODULE,?LINE]),
		   State#state{cluster_status=Status}
	   end,
  
    spawn(fun()->cluster_status_interval() end),  
    {noreply, NewState};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
cluster_status_interval()->
    ?PrintLog(log,"START ",[?FUNCTION_NAME,?MODULE,?LINE]),
    timer:sleep(10*1000),
 %   ?PrintLog(log,"Start ",[cluster_lib,strive_desired_state,?FUNCTION_NAME,?MODULE,?LINE]),
    Result=rpc:call(node(),cluster_lib,strive_desired_state,[],3*60*1000),
 %   ?PrintLog(log,"Result desired state",[Result,?FUNCTION_NAME,?MODULE,?LINE]),
    timer:sleep(?ClusterStatusInterval),
    rpc:cast(node(),cluster,cluster_status,[Result]),
    ?PrintLog(log,"END ",[?FUNCTION_NAME,?MODULE,?LINE]).
	    
