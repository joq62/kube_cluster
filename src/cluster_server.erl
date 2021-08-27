%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Manage Computers
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
-record(state, {running_hosts,
		missing_hosts,
		running_clusters,
		missing_clusters}).



%% --------------------------------------------------------------------
%% Definitions 
%-define(WantedStateInterval,60*1000).
-define(ClusterStatusInterval,1*30*1000).
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
    ?PrintLog(log,"Start init",[?MODULE]),
    % Loads dbase host and cluster info
    ?PrintLog(log,"1/8 load cluster and hosts and deployment info",[?FUNCTION_NAME,?MODULE,?LINE]),
    ?PrintLog(log,"2/8 Starts ssh ",[ssh:start(),?FUNCTION_NAME,?MODULE,?LINE]),
    case rpc:call(node(),host,status_all_hosts,[],10*1000) of
	{ok,RH,NAH}->
	    RunningHosts=RH,
	    MissingHosts=NAH;
	_->
	    RunningHosts=[],
	    MissingHosts=[]
    end,
    ?PrintLog(log,"3/8 Running Hosts ",[RunningHosts,?FUNCTION_NAME,?MODULE,?LINE]),
    ?PrintLog(log,"4/8 Missing Hosts ",[MissingHosts,?FUNCTION_NAME,?MODULE,?LINE]),
%    rpc:call(node(),cluster,strive_desired_state,[],50*1000),
%    ClusterStatus=rpc:call(node(),cluster,status_clusters,[],50*1000),
%    case ClusterStatus of
%	{{running,RunningClusters},{missing,MissingClusters}}->
%	    ok;
%	_->
%	    RunningClusters=[],
%	    MissingClusters=[]
 %   end,
    RunningClusters=[],
    MissingClusters=[],
    ?PrintLog(log,"5/8 Running Clusters ",[RunningClusters,?FUNCTION_NAME,?MODULE,?LINE]),
    ?PrintLog(log,"6/8 Missing Clusters ",[MissingClusters,?FUNCTION_NAME,?MODULE,?LINE]),

   ?PrintLog(log,"7/8 Start cluster_status_interval() ",[?FUNCTION_NAME,?MODULE,?LINE]),   
    spawn(fun()->cluster_status_interval() end),    

    ?PrintLog(log,"8/8 Successful starting of server",[?MODULE]),
    {ok, #state{running_hosts=RunningHosts,
		missing_hosts=MissingHosts,
		running_clusters=RunningClusters,
		missing_clusters=MissingClusters
	       }
    }.
    
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

%%-------- Hosts
handle_call({update_host_status},_From,State) ->
    Reply= case rpc:call(node(),host,status_all_hosts,[],10*1000) of
	       {ok,RH,NAH}->
		   NewState=State#state{running_hosts=RH,
					missing_hosts=NAH},
		   {ok,RH,NAH};
	       Err->
		   NewState=State,
		   {error,Err}
	   end,
        {reply, Reply, NewState};

handle_call({status_all_hosts},_From,State) ->
    Reply={State#state.running_hosts,State#state.missing_hosts},
    {reply, Reply, State};

handle_call({running_hosts},_From,State) ->
    Reply=State#state.running_hosts,
    {reply, Reply, State};

handle_call({missing_hosts},_From,State) ->
    Reply=State#state.missing_hosts,
    {reply, Reply, State};

handle_call({status_host,HostId},_From,State) ->
    AllHosts=lists:append(State#state.running_hosts,State#state.missing_hosts),
    Reply=[{Status,XHostId,Ip,Port}||{Status,XHostId,Ip,Port}<-AllHosts,
			       HostId==XHostId],
    {reply, Reply, State};

%%------- Clusters

handle_call({create_cluster,ClusterId},_From,State) ->
    Reply = case rpc:call(node(),cluster,create,[ClusterId],25*1000) of
		{ok,ClusterId,RunningKubeletNodes}->
		    RunningClusters=[{ClusterId,RunningKubeletNodes}|lists:keydelete(ClusterId,1,State#state.running_clusters)],
		    NewState=State#state{running_clusters=RunningClusters},
		    ok;
		Err->
		    NewState=State,
		    {error,[Err]}
	    end,
    {reply, Reply, NewState};

handle_call({status_all_clusters},_From,State) ->
    Reply={{running,State#state.running_clusters},{missing,State#state.missing_clusters}},
    {reply, Reply, State};

handle_call({running_clusters},_From,State) ->
    Reply=State#state.running_clusters,
    {reply, Reply, State};

handle_call({not_available_clusters},_From,State) ->
    Reply=State#state.missing_clusters,
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

handle_cast({status_hosts,HostsStatus}, State) ->
   % io:format("ClusterStatus ~p~n",[{?FUNCTION_NAME,?MODULE,?LINE,ClusterStatus}]),
    case HostsStatus of
	{ok,Running,Missing}->
	    NewState=State#state{running_hosts=Running,missing_hosts=Missing};
	Err->
	    io:format("Err ~p~n",[{?FUNCTION_NAME,?MODULE,?LINE,Err}]),
	    NewState=State
    end,
    {noreply, NewState};

handle_cast({cluster_strive_desired_state,ClusterStatus}, State) ->
   % io:format("ClusterStatus ~p~n",[{?FUNCTION_NAME,?MODULE,?LINE,ClusterStatus}]),
    case ClusterStatus of
	{{running,Running},{missing,Missing}}->
	    NewState=State#state{running_clusters=Running,missing_clusters=Missing};
	Err->
	    io:format("Err ~p~n",[{?FUNCTION_NAME,?MODULE,?LINE,Err}]),
	    NewState=State
    end,
    spawn(fun()->cluster_status_interval() end),    
    {noreply, NewState};


handle_cast({wanted_state}, State) ->
    spawn(fun()->cluster:wanted_state(State#state.running_clusters) end),    
    {noreply, State};
     
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
cluster_status_interval()->
    timer:sleep(?ClusterStatusInterval),
    spawn(fun()->check_status_hosts() end),
    ok.
  %  spawn(fun()->cl_strive_desired_state() end).

check_status_hosts()->
   Status=case rpc:call(node(),host,status_all_hosts,[],10*1000) of
	      {ok,RH,NAH}->
		  PrintRunningHosts=[Alias||{running,Alias,_,_,_}<-RH],
		  ?PrintLog(log,"Running Hosts ",[PrintRunningHosts]),
		  PrintMissingHosts=[Alias||{missing,Alias,_,_,_}<-NAH],
		  ?PrintLog(ticket,"Missing Hosts ",[PrintMissingHosts]),
		  {ok,RH,NAH};
	      Err->
		  ?PrintLog(ticket,"Error ",[Err,?FUNCTION_NAME,?MODULE,?LINE]),
		  {error,Err}
	  end,
    
    rpc:cast(node(),iaas,status_hosts,[Status]).

cl_strive_desired_state()->
 %   {ok,ClusterIdAtom}=application:get_env(cluster_id),
 %   ClusterId=atom_to_list(ClusterIdAtom),
    {ok,ClusterId}=application:get_env(cluster_id),
    rpc:call(node(),cluster,strive_desired_state,[ClusterId],90*1000),
    ClusterStatus=case rpc:call(node(),cluster,status_clusters,[],60*1000) of
		      {error,["ClusterID eexists"]}->
			  ?PrintLog(ticket,"error",["ClusterID eexists",?FUNCTION_NAME,?MODULE,?LINE]),
			  {{running,[]},{missing,[]}};
		      {badrpc,Reason}->
			  ?PrintLog(ticket,"badrpc",[Reason,?FUNCTION_NAME,?MODULE,?LINE]),
			  {{running,[]},{missing,[]}};
		      {{running,RunningClusters},{missing,MissingClusters}}->
			  case MissingClusters of
			      []->
				  PrintRunningClusters=[XClusterId||{XClusterId,_}<-RunningClusters],
				  ?PrintLog(log,"Running Clusters ",[PrintRunningClusters]);
			      _->
				  PrintRunningClusters=[XClusterId||{XClusterId,_}<-RunningClusters],
				  ?PrintLog(log,"Running Clusters ",[PrintRunningClusters]),
				  PrintMissingClusters=[XClusterId||{XClusterId,_}<-MissingClusters],
				  ?PrintLog(ticket,"Missing Clusters ",[PrintMissingClusters])
			  end,
			  {{running,RunningClusters},{missing,MissingClusters}}
		  end,
    rpc:cast(node(),iaas,cluster_strive_desired_state,[ClusterStatus]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
