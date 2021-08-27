%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% ToDo 
%%% 1. New cluster
%%% 2. Check existing cluster -> restart missing node vms
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(cluster).    
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube_logger.hrl").
%%---------------------------------------------------------------------
%% Records & defintions
%%---------------------------------------------------------------------
%missing clusters
%running clusters

-define(HostNodeName(ClusterId,HostId),HostId++"_"++ClusterId).
-define(HostNode(ClusterId,HostId),list_to_atom(HostId++"_"++ClusterId++"@"++HostId)).

-define(KubeletNodeName(ClusterId),ClusterId++"_"++"kubelet").
-define(KubeletNode(ClusterId,Alias,HostId),list_to_atom(ClusterId++"_"++"kubelet"++"_"++Alias++"@"++HostId)).
%% --------------------------------------------------------------------
-export([
       	 
	 start_node/1,
	     
	 strive_desired_state/0,
       	 strive_desired_state/1,

	 status_clusters/0,
	 status_clusters/1,

	 create/1,
	 delete/1
	]).


%% ====================================================================
%% External functions
%% ====================================================================  

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
strive_desired_state(ClusterId)->
   %   io:format("~p~n",[{?FUNCTION_NAME,?MODULE,?LINE}]),
    {{running,_R},{missing,M}}=cluster:status_clusters(ClusterId),
    [{cluster:create(XClusterId),XClusterId}||{XClusterId,_}<-M].


strive_desired_state()->
   %   io:format("~p~n",[{?FUNCTION_NAME,?MODULE,?LINE}]),
    {{running,_R},{missing,M}}=cluster:status_clusters(),
    [{cluster:create(ClusterId),ClusterId}||{ClusterId,_}<-M].

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

status_clusters()->
    ClusterId=application:get_env(cluster_id),
    status_clusters(ClusterId).
status_clusters(ClusterId)->
    R=case db_cluster_info:read(ClusterId) of
	  []->
	      ?PrintLog(alert,"ClusterID eexists",[ClusterId,?FUNCTION_NAME,?MODULE,?LINE]),
	      {error,["ClusterID eexists"]};
	  ClusterInfo->
	      check(ClusterInfo,[],[])
      end,
   % io:format("R ~p~n",[{R,?FUNCTION_NAME,?MODULE,?LINE}]),
    R.
    
check([],Running,Missing)->
    {{running,Running},{missing,Missing}};

check([{ClusterId,ControllerAlias,_,WorkerAlias,_Cookie,_}|T],Running,Missing) ->
  %  ?PrintLog(debug,"WorkerAlias ",[WorkerAlias,?FUNCTION_NAME,?MODULE,?LINE]),
    H1=[Alias||{Alias,_HostId}<-WorkerAlias,
		false==lists:member(Alias,ControllerAlias)],
    AllAlias=lists:append(ControllerAlias,H1),
  % ?PrintLog(debug,"WorkerAlias ",[WorkerAlias,?FUNCTION_NAME,?MODULE,?LINE]),
    AllHostInfo=[db_host_info:read(Alias)||Alias<-AllAlias],
  %  ?PrintLog(debug,"AllHostInfo ",[AllHostInfo,?FUNCTION_NAME,?MODULE,?LINE]),

  %  NodesToCheck=[{Alias,?KubeletNode(ClusterId,Alias,HostId)}||[{Alias,HostId,_Ip,_SshPort,_UId,_Pwd}]<-AllHostInfo],
    NodesToCheck=[{Alias,?HostNode(ClusterId,HostId)}||[{Alias,HostId,_Ip,_SshPort,_UId,_Pwd}]<-AllHostInfo],
  %  ?PrintLog(debug,"NodesToCheck ",[NodesToCheck,?FUNCTION_NAME,?MODULE,?LINE]),
    {R1,M1}=do_ping(NodesToCheck,ClusterId,[],[]),
    case M1 of
	[]->
	    NewRunning=[{ClusterId,R1}|Running],
	    NewMissing=Missing;
	_ ->
	    NewMissing=[{ClusterId,M1}|Missing],
	    NewRunning=Running
    end,   
    check(T,NewRunning,NewMissing).

do_ping([],_ClusterId,Running,Missing)->
    {Running,Missing};
do_ping([{Alias,Node}|T],ClusterId,Running,Missing)->
    case net_adm:ping(Node) of
	pong->
	    NewRunning=[{Alias,Node}|Running],
	    NewMissing=Missing;
	pang ->
	    NewMissing=[{Alias,Node}|Missing],
	    NewRunning=Running
    end,    
    do_ping(T,ClusterId,NewRunning,NewMissing).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
delete(ClusterId)->
    ?PrintLog(log,"Delete ClusterId ",[ClusterId,?FUNCTION_NAME,?MODULE,?LINE]),
      R=case db_cluster_info:read(ClusterId) of
	  []->
	      {error,[eexists,ClusterId]};
	  ClusterInfo->
	%	case db_cluster:member(ClusterId) of
	%	    false->
	%		?PrintLog(ticket,"not loaded",[ClusterId,?FUNCTION_NAME,?MODULE,?LINE]),
	%		{error,["not loaded",ClusterId,?FUNCTION_NAME,?MODULE,?LINE]};
	%	    true->
			[{ClusterId,ControllerAlias,_NumWorkers,WorkerAlias,_Cookie,_ControllerNode}]=ClusterInfo,
			H1=[XAlias||XAlias<-WorkerAlias, 
				    false==lists:member(XAlias,ControllerAlias)],
			AllAlias=lists:append(ControllerAlias,H1),
			AllHostInfo=[db_host_info:read(Alias)||{Alias,_HostId}<-AllAlias],
			NodesToKill=[?HostNode(ClusterId,HostId)||[{_Alias,HostId,_Ip,_SshPort,_UId,_Pwd}]<-AllHostInfo],
			[{Node,ClusterId,delete_cluster(Node,ClusterId)}||Node<-NodesToKill]
	%	end
	end,
    R.
delete_cluster(Node,ClusterId)->
    rpc:call(Node,os,cmd,["rm -rf "++ClusterId],5*1000),
    rpc:call(Node,init,stop,[]),
    db_cluster:remove(host_nodes,Node),
    {stopped,Node,ClusterId}.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
    
create(ClusterId)->
    F1=fun start_node/2,
    F2=fun check_node/3,
    ?PrintLog(log,"Create ClusterId ",[ClusterId,?FUNCTION_NAME,?MODULE,?LINE]),
    
    R=case db_cluster_info:read(ClusterId) of
	  []->
	      ?PrintLog(ticket,"ClusterId eexists",[{error,[eexists,ClusterId]},?FUNCTION_NAME,?MODULE,?LINE]),
	      {error,[eexists,ClusterId]};
	  ClusterInfo->
	      [{ClusterId,ControllerAlias,_NumWorkers,WorkerAlias,Cookie,_ControllerNode}]=ClusterInfo,
	   %   NodeName=?KubeletNodeName(ClusterId),  %ClusterId++"_"++"kubelet",
	      % Create db_cluster:create(
	      {atomic,ok}=db_cluster:create(ClusterId,undefined,[],Cookie,[],[]),
	      H1=[XAlias||XAlias<-WorkerAlias,
			  false==lists:member(XAlias,ControllerAlias)],
	      AllAlias=lists:append(ControllerAlias,H1),
	      ?PrintLog(debug,"AllAlias",[AllAlias,?FUNCTION_NAME,?MODULE,?LINE]),
	      R1=create_list_to_reduce(AllAlias,ClusterId,Cookie),	     
	      case R1 of
		  {error,ErrAlias}->
		      ?PrintLog(ticket,"reduce list",[{error,ErrAlias},?FUNCTION_NAME,?MODULE,?LINE]),
		      {error,ErrAlias};
		  {ok,ListToReduce}->
		      ?PrintLog(debug,"ListToReduce",[ListToReduce,?FUNCTION_NAME,?MODULE,?LINE]),
		      StartResult=mapreduce:start(F1,F2,[],ListToReduce),
		      ?PrintLog(debug,"StartResult",[StartResult,?FUNCTION_NAME,?MODULE,?LINE]),
		    %  io:format("StartResult ~p~n",[{?MODULE,?LINE,StartResult}]), 
		      {ClusterId,StartResult}
	      
	      end
      end,
    
    R.

create_list_to_reduce(AllAlias,ClusterId,Cookie)->
    create_list_to_reduce(AllAlias,ClusterId,Cookie,[]).
create_list_to_reduce([],_ClusterId,_Cookie,Acc)->
    case [{error,Reason}||{error,Reason}<-Acc] of
	[]->
	   % ReduceInfo=lists:append([XInfo||{ok,XInfo}<-Acc]),
	    %ReduceInfo=lists:append([XInfo||{_,XInfo}<-Acc]),  
	    ReduceInfo=[XInfo||{_,XInfo}<-Acc],
	    
	    {ok,ReduceInfo};
	ErrList->
	    {error,ErrList}
    end;
create_list_to_reduce([{Alias,HostId}|T],ClusterId,Cookie,Acc)->
    ?PrintLog(debug,"Alias,HostId",[Alias,HostId,?FUNCTION_NAME,?MODULE,?LINE]),
    Info=case db_host_info:read(Alias) of
	     []->
		 {error,[eexists,Alias]};
	     [AliasInfo]->
		 
		 NodeName=?HostNodeName(ClusterId,HostId),
						%Check if allready started
		 case net_adm:ping(?HostNode(ClusterId,HostId)) of
		     pang->
			 {ok,[AliasInfo,NodeName,ClusterId,Cookie]};
		     pong->
			 {already_started,[AliasInfo,NodeName,ClusterId,Cookie]}
		 end
		     
    end,
    ?PrintLog(debug,"Info",[Info,?FUNCTION_NAME,?MODULE,?LINE]),
    create_list_to_reduce(T,ClusterId,Cookie,[Info|Acc]).    

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_node(Pid,[{Alias,HostId,Ip,SshPort,UId,Pwd},NodeName,ClusterId,Cookie])->
    {Result,Node}=start_node([{Alias,HostId,Ip,SshPort,UId,Pwd},NodeName,ClusterId,Cookie]),
    Pid!{check_node,{Result,Node,ClusterId,HostId,Ip,SshPort}}.
 
start_node([{Alias,HostId,Ip,SshPort,UId,Pwd},NodeName,ClusterId,Cookie])->
    RM_cluster="rm -rf "++ClusterId,
    RM_Result=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,RM_cluster,2*5000],3*5000),
    ?PrintLog(log,"ssh ",[RM_cluster,RM_Result,Alias,?FUNCTION_NAME,?MODULE,?LINE]),
    MKDIR_cluster="mkdir "++ClusterId,
    MKDIR_result=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,MKDIR_cluster,2*5000],3*5000),
    ?PrintLog(log,"ssh ",[MKDIR_cluster,MKDIR_result,Alias,?FUNCTION_NAME,?MODULE,?LINE]),
    UniqueNodeName=NodeName,
 %   erlang:set_cookie(node(),list_to_atom(Cookie)),
    timer:sleep(1000),
    Node=list_to_atom(UniqueNodeName++"@"++HostId),
    Result= case rpc:call(node(),net_adm,ping,[Node],1*1000) of
		pong->
		    ?PrintLog(ticket,"already started ",[Node,ClusterId,Alias,?FUNCTION_NAME,?MODULE,?LINE]),
		    ok;
		_->
		    NodeStop=rpc:call(Node,init,stop,[]),
		    ?PrintLog(log,"init stop ",[NodeStop,Node,?FUNCTION_NAME,?MODULE,?LINE]),
%		    ?PrintLog(debug,"Cookie ",[Cookie,Node,?FUNCTION_NAME,?MODULE,?LINE]),
		 %   ?PrintLog(debug,"Node Cookie ",[rpc:call(node(),erlang,get_cookie,[]),node(),?FUNCTION_NAME,?MODULE,?LINE]),
		    %ErlCmd="erl -detached "++"-sname "++UniqueNodeName++" "++"-setcookie "++Cookie,
		    ErlCmd="erl_call -s "++"-sname "++UniqueNodeName++" "++"-c "++Cookie,
		    SshCmd="nohup "++ErlCmd++" &",
		    ErlcCmdResult=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,SshCmd,2*5000],3*5000),
		    ?PrintLog(log,"ssh ",[ErlcCmdResult,SshCmd,Node,?FUNCTION_NAME,?MODULE,?LINE]),
		    ErlcCmdResult
	    end,
   % io:format("Result ~p~n",[{?MODULE,?LINE,Result,Node,Alias,Ip,SshPort}]),
    {Result,Node}.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
check_node(check_node,Vals,[])->
  %  io:format("Vals ~p~n",[{?MODULE,?LINE,Vals}]),
     check_node(Vals,[]).

check_node([],Result)->
    Result;

check_node([{{badrpc,timeout},Node,ClusterId,HostId,Ip,SshPort}|T],Acc)->
    ?PrintLog(ticket,"Failed to start node",[Node,HostId,{badrpc,timeout},?FUNCTION_NAME,?MODULE,?LINE]),
    check_node(T,[{error,[{badrpc,timeout},Node,ClusterId,HostId,Ip,SshPort]}|Acc]); 
check_node([{{error,Reason},Node,ClusterId,HostId,Ip,SshPort}|T],Acc)->
     ?PrintLog(ticket,"Failed to start node",[Node,HostId,ClusterId,{error,Reason},?FUNCTION_NAME,?MODULE,?LINE]),
     check_node(T,[{error,[Reason,Node,HostId,Ip,SshPort]}|Acc]);
  
check_node([{Result,Node,ClusterId,HostId,Ip,SshPort}|T],Acc)->
  %  ?PrintLog(debug,"Result",[Result,HostId,Node,ClusterId]),
 %   ?PrintLog(debug,"Cookie after created ",[rpc:call(Node,erlang,get_cookie,[]),Node,?FUNCTION_NAME,?MODULE,?LINE]),
    NewAcc=case Result of
	       ok->
		   
		   case node_started(Node) of
		       true->
			       %Remove all cluster pods
			 %  ?PrintLog(debug,"ssh ",[RM_cluster,RM_Result_cluster,Alias,?FUNCTION_NAME,?MODULE,?LINE]),
%			   {ok,FileNames}=rpc:call(Node,file,list_dir,["."],5*1000),
%			   DirsToDelete=[FileName||FileName<-FileNames,
%						   ".pod_dir"==filename:extension(FileName)],
%			   ok=del_dir(DirsToDelete,Node),
			   ClusterAddResult=db_cluster:add(host_nodes,{Node,HostId}),
			   ?PrintLog(debug,"  {atomic,ok}",[ClusterAddResult,Node,HostId,?FUNCTION_NAME,?MODULE,?LINE]),
			   [{ok,Node,HostId,ClusterId,Ip,SshPort}|Acc];
		       false->
			   ?PrintLog(ticket,"Failed to connect to node",[Node,HostId,?FUNCTION_NAME,?MODULE,?LINE]),
			   [{error,["Failed to connect to node",Node,HostId,ClusterId,Ip,SshPort,?MODULE,?FUNCTION_NAME,?LINE]}|Acc]
		   end;
	       Err->
		   ?PrintLog(ticket,"error",[Err,Node,HostId,?FUNCTION_NAME,?MODULE,?LINE]),
		   [{Result,Node,HostId,ClusterId,Ip,SshPort}|Acc]
	   end,
    check_node(T,NewAcc).
node_started(Node)->
       check_started(50,Node,10,false).
    
check_started(_N,_Vm,_SleepTime,true)->
   true;
check_started(0,_Vm,_SleepTime,Result)->
    Result;
check_started(N,Vm,SleepTime,_Result)->
 %   io:format("net_Adm ~p~n",[net_adm:ping(Vm)]),
    NewResult= case net_adm:ping(Vm) of
	%case rpc:call(node(),net_adm,ping,[Vm],1000) of
		  pong->
		     true;
		  pang->
		       timer:sleep(SleepTime),
		       false;
		   {badrpc,_}->
		       timer:sleep(SleepTime),
		       false
	      end,
    check_started(N-1,Vm,SleepTime,NewResult).
