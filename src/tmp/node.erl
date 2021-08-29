%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% ToDo 
%%% 1. New cluster
%%% 2. Check existing cluster -> restart missing node vms
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(node).    
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube_logger.hrl").
%%---------------------------------------------------------------------
%% Records & defintions
%%---------------------------------------------------------------------
%missing clusters
%running clusters

%% --------------------------------------------------------------------

-export([
	 create/2,
	 
	 create_pod/2,
	 delete_pod/2,
	 create_node/7,  
	 delete_node/1,
	 delete_node/2
	]).

%% ====================================================================
%% External functions
%% ====================================================================  
create(PodId,Alias,PodName)->
    Result=case db_host_spec:read(Alias) of
	       []->
		   {error,[eexists,Alias,?FUNCTION_NAME,?MODULE,?LINE]};
	       [{Alias,HostId,Ip,SshPort,UId,Pwd}]->
		   {ok,ClusterId_X}=application:get_env(cluster_id),
		   ClusterId=case is_atom(ClusterId_X) of
				 true->
				     atom_to_list(ClusterId_X);
				 false->
				     ClusterId_X
			     end,
		   Cookie=atom_to_list(erlang:get_cookie()),
		   NodeName=ClusterId++"_"++HostId++"_"++PodId,
		   Pod=list_to_atom(NodeName++"@"++HostId),
		   Dir=PodId++"."++ClusterId,
     
		   RM_Kubelet_dir="rm -rf "++Dir,
		   RM_Result=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,RM_Kubelet_dir,2*5000],3*5000),
		   ?PrintLog(log,"ssh ",[RM_Kubelet_dir,RM_Result,?FUNCTION_NAME,?MODULE,?LINE]),
		   MKDIR_Kubelet_dir="mkdir "++Dir,
		   MKDIR_result=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd, MKDIR_Kubelet_dir,2*5000],3*5000),
		   ?PrintLog(log,"ssh ",[ MKDIR_Kubelet_dir,MKDIR_result,?FUNCTION_NAME,?MODULE,?LINE]),
    
		   ErlCmd="erl_call -s "++"-sname "++NodeName++" "++"-c "++Cookie,
		   SshCmd="nohup "++ErlCmd++" &",
		   ErlcCmdResult=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,SshCmd,2*5000],3*5000),
		   case node_started(Pod) of
		       false->
			   {error,['failed to start', Pod,Alias,?FUNCTION_NAME,?MODULE,?LINE]};
		       true->
			   {atomic,ok}=db_kubelet:create(PodId,HostId,ClusterId,Pod,Dir,Pod,Cookie,[]),
			 %  container load _start ok
			   glurk
		   end
	   end,
    Result.


delete_pod(Id)->
    {ok,HostId}=inet:gethostname(),
    {ok,ClusterId_X}=application:get_env(cluster_id),
    ClusterId=case is_atom(ClusterId_X) of
		  true->
		      atom_to_list(ClusterId_X);
		  false->
		      ClusterId_X
	      end,
    NodeName=ClusterId++"_"++HostId++"_"++Id,
    Pod=list_to_atom(NodeName++"@"++HostId),
    Dir=Id++"."++ClusterId,
    delete_pod(Pod,Dir).

delete_pod(Pod,Dir)->
    rpc:call(Pod,os,cmd,["rm -rf "++Dir],5*1000),
    rpc:call(Pod,init,stop,[],5*1000),		   
    Result=case node_stopped(Pod) of
	       false->
		   {error,["node not stopped",Pod,?FUNCTION_NAME,?MODULE,?LINE]};
	       true->
		   db_kubelet:delete(Pod),
		   ok
	   end,
    Result.



start_containers(PodName,Pod,Dir)->
    Containers=db_pod_spec:containers(PodName),
    start_containers(Containers,PodName,Pod,Dir,[]).
start_containers([],_PodName,_Pod,_Dir,StartResult)->
    StartResult;
start_containers([{AppId,AppVsn,GitPath,AppEnv}|T],PodName,Pod,Dir,Acc)->
    NewAcc=case container:load(AppId,AppVsn,GitPath,AppEnv,Pod,Dir) of
	       {error,Reason}->
		   {Pod,Dir,HostId,PodStatus,ContainerStatus}=db_pod_spec:deployment(PodName,Pod),
		   NewContainerStatus=[{AppId,AppVsn,failure}|lists:keydelete(AppId,1,ContainerStatus)],		   
		   {atomic,ok}=db_pod_spec:update_deployment(PodName,Pod,Dir,HostId,PodStatus,NewContainerStatus),		   
		   [{error,[Reason,AppId,AppVsn,GitPath,AppEnv,Pod,Dir,?FUNCTION_NAME,?MODULE,?LINE]}|Acc];
	       ok->
		   case container:start(AppId,Pod) of
		       {error,Reason}->
			   {Pod,Dir,HostId,PodStatus,ContainerStatus}=db_pod_spec:deployment(PodName,Pod),
			   NewContainerStatus=[{AppId,AppVsn,failure}|lists:keydelete(AppId,1,ContainerStatus)],		   
			   {atomic,ok}=db_pod_spec:update_deployment(PodName,Pod,Dir,HostId,PodStatus,NewContainerStatus),
			   [{error,[Reason,AppId,AppVsn,GitPath,AppEnv,Pod,Dir,?FUNCTION_NAME,?MODULE,?LINE]}|Acc];
		       ok->
			   {Pod,Dir,HostId,PodStatus,ContainerStatus}=db_pod_spec:deployment(PodName,Pod),
			   NewContainerStatus=[{AppId,AppVsn,started}|lists:keydelete(AppId,1,ContainerStatus)],		   
			   {atomic,ok}=db_pod_spec:update_deployment(PodName,Pod,Dir,HostId,PodStatus,NewContainerStatus),
			   [{ok,AppId}|Acc]
		   end
	   end,
    start_containers(T,PodName,Pod,Dir,NewAcc).  
    

create_vm(PodName,ClusterName)->
    Result=case get_host(PodName,ClusterName) of
	       {error,Reason}->
		   {error,Reason};
	       {ok,{Alias,HostId}} ->
		   UniqueId=integer_to_list(erlang:system_time(microsecond)), 
		   NodeName=UniqueId++"_"++ClusterName,
		   Dir=UniqueId++"."++ClusterName,
		   Cookie=db_cluster_spec:cookie(ClusterName),
		   [{Alias,HostId,Ip,SshPort,UId,Pwd}]=db_host_info:read(Alias),
		   ErlCallArgs="-c "++Cookie++" "++"-sname "++NodeName,
		   Node=list_to_atom(NodeName++"@"++HostId),    
		   true=erlang:set_cookie(Node,list_to_atom(Cookie)),
		   true=erlang:set_cookie(node(),list_to_atom(Cookie)),
		   case create_node(Ip,SshPort,UId,Pwd,HostId,NodeName,ErlCallArgs) of
		       {error,Reason}->
			   {error,Reason};
		       {ok,Pod}->
			   case rpc:call(Pod,file,make_dir,[Dir],5*1000) of
			       {error,Reason} ->
				   {error,[Reason,Ip,SshPort,UId,Pwd,HostId,NodeName,ErlCallArgs,?FUNCTION_NAME,?MODULE,?LINE]};
			       ok->
				   {atomic,ok}=db_pod_spec:add_deployment(PodName,Pod,Dir,HostId,running,[]),
				   {ok,Pod,Dir}
			   end
		   end
	   end, 
    Result.

get_host(PodName,ClusterName)->
    Result=case db_cluster_spec:hosts(ClusterName) of
	       {error,Reason}->
		    {error,Reason};
	       Hosts->
		   case db_pod_spec:wanted_hosts(PodName) of
		       []->
			   %Choose Host which has not PodName deployed
			   %If all Hosts are occupied - choose the one with lowest number of applications
			   % 
			   L=lists:flatlength(Hosts),
			   {Alias,HostId}=lists:nth(rand:uniform(L),Hosts),
			   {ok,{Alias,HostId}};
		       [{WantedAlias,WantedHostId}]->
			   case lists:member({WantedAlias,WantedHostId},Hosts) of
			       false->
				   {error,["eexists",{WantedAlias,WantedHostId},Hosts,?FUNCTION_NAME,?MODULE,?LINE]};
			       true->
				   {ok,{WantedAlias,WantedHostId}}
			   end
		   end
	   end,
    Result.    


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Creates a slave node via HostNode 
%% NodeName=pod_microseconds_clusterId_HostId
%% PodDir=clusterId/pod_microseconds
%% AppDir=PodDir/AppId
%% AppPa=AppDir/ebin
%% Returns: non
%% --------------------------------------------------------------------
delete_pod(PodName,Pod)->
    Result=case db_pod_spec:deployment(PodName,Pod) of
	       []->
		   {error,["eexists",PodName,?FUNCTION_NAME,?MODULE,?LINE]};
	       {Pod,Dir,_HostId,_PodStatus,_ContainerStatus}->
		   [container:unload_stop(AppId,Pod)||{AppId,_Vsn,_GitPath,_Env}<-db_pod_spec:containers(PodName)],
		   rpc:call(Pod,os,cmd,["rm -rf "++Dir],5*1000),
		   rpc:call(Pod,init,stop,[],5*1000),		   
		   {atomic,ok}=db_pod_spec:delete_deployment(PodName,Pod),
		   case node_stopped(Pod) of
		       false->
			   {error,["node not stopped",PodName,Pod,?FUNCTION_NAME,?MODULE,?LINE]};
		       true->
			   ok
		   end
	   end,
    Result.

delete_node(Node)->
    rpc:call(Node,init,stop,[],5*1000),
    ok.
delete_node(Node,Dir)->
    rpc:call(Node,os,cmd,["rm -rf "++Dir],5*1000),
    rpc:call(Node,init,stop,[],5*1000),
    ok.

%create_node(Alias,NodeName,Cookie)->
%    [{Alias,HostId,Ip,SshPort,UId,Pwd}]=db_host_info:read(Alias),
%    ErlCallArgs="-c "++Cookie++" "++"-sname "++NodeName,
%    Node=list_to_atom(NodeName++"@"++HostId),    
%    true=erlang:set_cookie(Node,list_to_atom(Cookie)),
%    true=erlang:set_cookie(node(),list_to_atom(Cookie)),
%    Result=create_node(Ip,SshPort,UId,Pwd,HostId,NodeName,ErlCallArgs),
%    Result.

create_node(Ip,SshPort,UId,Pwd,HostId,NodeName,ErlCallArgs)->    
    ErlCmd="erl_call -s "++ErlCallArgs, 
    SshCmd="nohup "++ErlCmd++" &",
    ErlCallResult=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,SshCmd,3*5000],4*5000),
    Result=case ErlCallResult of
	       {badrpc,timeout}->
		   ?PrintLog(ticket,"Failed to start node",[Ip,SshPort,UId,Pwd,NodeName,ErlCallArgs,badrpc,timeout,?FUNCTION_NAME,?MODULE,?LINE]),
		   {error,[{badrpc,timeout},Ip,SshPort,UId,Pwd,NodeName,ErlCallArgs,?FUNCTION_NAME,?MODULE,?LINE]};
	       {error,Reason}->
		   ?PrintLog(ticket,"Failed to start node",[Ip,SshPort,UId,Pwd,NodeName,ErlCallArgs,error,Reason,?FUNCTION_NAME,?MODULE,?LINE]),
		   {error,[Reason,?FUNCTION_NAME,?MODULE,?LINE]};
	       StartResult->
		   Node=list_to_atom(NodeName++"@"++HostId),
		   case node_started(Node) of
		       true->
			  % ?PrintLog(debug,"  {atomic,ok}",[ClusterAddResult,Node,HostId,?FUNCTION_NAME,?MODULE,?LINE]),
			   {ok,Node};
		       false->
			   ?PrintLog(ticket,"Failed to connect to node",[Ip,SshPort,UId,Pwd,HostId,NodeName,ErlCallArgs,?FUNCTION_NAME,?MODULE,?LINE]),
			   {error,["Failed to connect to node",Ip,SshPort,UId,Pwd,HostId,NodeName,ErlCallArgs,?MODULE,?FUNCTION_NAME,?LINE]}
		   end
	   end,
    Result.
		   
	      
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

node_stopped(Node)->
    check_stopped(100,Node,50,false).
    
check_stopped(_N,_Vm,_SleepTime,true)->
   true;
check_stopped(0,_Vm,_SleepTime,Result)->
    Result;
check_stopped(N,Vm,SleepTime,_Result)->
 %   io:format("net_Adm ~p~n",[net_adm:ping(Vm)]),
    NewResult= case net_adm:ping(Vm) of
	%case rpc:call(node(),net_adm,ping,[Vm],1000) of
		  pang->
		     true;
		  pong->
		       timer:sleep(SleepTime),
		       false;
		   {badrpc,_}->
		       true
	       end,
    check_stopped(N-1,Vm,SleepTime,NewResult).

