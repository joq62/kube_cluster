%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% ToDo 
%%% 1. New cluster 
%%% 2. Check existing cluster -> restart missing node vms
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(cluster_lib).    
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube_logger.hrl").
%%---------------------------------------------------------------------
%% Records & defintions
%%---------------------------------------------------------------------
%missing clusters
%running clusters

%-define(HostNodeName(ClusterId,HostId),HostId++"_"++ClusterId).
%-define(HostNode(ClusterId,HostId),list_to_atom(HostId++"_"++ClusterId++"@"++HostId)).

-define(KubeletNodeName(ClusterId,HostId),ClusterId++"_"++HostId++"_kubelet").
%-define(KubeletNode(ClusterId,Alias,HostId),list_to_atom(ClusterId++"_"++"kubelet"++"_"++Alias++"@"++HostId)).
%% --------------------------------------------------------------------
-export([
	 stop_node/3,
	 load_start/3,
	 stop_unload/3,

	 strive_desired_state/0
	]).


%% ====================================================================
%% External functions
%% ====================================================================  
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
strive_desired_state()->
    ClusterId=sd:call(etcd,db_cluster_info,cluster,[],5*1000),
    Result= case find_nodes_wo_kubelet_loaded() of
		 {error,Reason}->
		    {error,Reason};
		{ok,FindNodeWOKubeletLoaded,RunningHosts,MissingHosts}->
		    ?PrintLog(debug,"",[FindNodeWOKubeletLoaded,RunningHosts,MissingHosts,?FUNCTION_NAME,?MODULE,?LINE]), 
		 %   case strive_desired_state(ClusterId,FindNodeWOKubeletLoaded) of
		    case FindNodeWOKubeletLoaded of
			{error,Reason}->
			    {error,[Reason,?FUNCTION_NAME,?MODULE,?LINE]};
			FindNodeWOKubeletLoaded->
			    DeploymentSpecName="node",
			    StartKubeletNodesInfo=start_kubelet_nodes(FindNodeWOKubeletLoaded,ClusterId,DeploymentSpecName),
			    case StartKubeletNodesInfo of
				{error,Reason}->
				    {error,Reason};
				{ok,StartInfo}->
	%			    ?PrintLog(log,"RunningHosts ",[RunningHosts ,?FUNCTION_NAME,?MODULE,?LINE]),
	%			    ?PrintLog(log,"MissingHosts ",[MissingHosts ,?FUNCTION_NAME,?MODULE,?LINE]),
				    ClusterStatus=examine_state(StartInfo,RunningHosts,MissingHosts),
			%	    ?PrintLog(log,"ClusterStatus ",[ClusterStatus ,?FUNCTION_NAME,?MODULE,?LINE]),
				    ClusterStatus			    
			    end
		    end
	    end,
    Result.

examine_state([],_RunningHosts,[])->
    {ok,{"In desired state",[]}};
examine_state(StartInfo,_RunningHosts,[])->
    {error,{"Starting found hosts without kubelet",[StartInfo]}};
examine_state([],_RunningHosts,MissingHosts)->
    {error,{"Missing hosts",[MissingHosts]}};
examine_state(StartInfo,_RunningHosts,MissingHosts)->
    {error,{"Starting found Host and missing hosts",[StartInfo,MissingHosts]}}.
%examine_state(StartInfo,RunningHosts,MissingHosts)->
%    {error,{unmatched,[StartInfo,RunningHosts,MissingHosts]}}.

		
find_nodes_wo_kubelet_loaded()->
    Result=case rpc:call(node(),host,status_all_hosts,[],20*1000) of
	       {badrpc,Reason}->
		   {error,[badrpc,Reason,?FUNCTION_NAME,?MODULE,?LINE]};
	       {error,Reason}->
		   {error,[Reason,?FUNCTION_NAME,?MODULE,?LINE]};
	       {ok,RunningHosts,MissingHosts}-> 
		   HostsWithKubelete=get_nodes_with_kubelet_runing(),
		   FindNodeWOKubeletLoaded=[{Alias,HostId}||{Alias,HostId}<-RunningHosts,
							   false==lists:keymember(HostId,2,HostsWithKubelete)], 
		   {ok,FindNodeWOKubeletLoaded,RunningHosts,MissingHosts}
	   end,
    Result.
get_nodes_with_kubelet_runing()->   
    
    KubeletePing=[rpc:call(Node,kubelet,ping,[],2*1000)||Node<-[node()|nodes()]],
    X=[{KubeletNode,rpc:call(KubeletNode,inet,gethostname,[],5*1000)}||{pong,KubeletNode,_}<-KubeletePing],
    HostsWithKubelete=[{KubeletNode,HostId}||{KubeletNode,{ok,HostId}}<-X],
    HostsWithKubelete.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_kubelet_nodes(HostWithOutKubelet,ClusterId,DeploymentSpecName)->
    F1=fun map_create_kubelet_node/2,
    F2=fun check_kubelet_node_start/3,
    StartList=[{HostInfo,ClusterId,DeploymentSpecName}||HostInfo<-HostWithOutKubelet],
    StartResult=mapreduce:start(F1,F2,[],StartList),
    Result=case [{error,Reason}||{error,Reason}<-StartResult] of
	       []->
		   {ok,StartResult};
	       _->
		   {error,StartResult}
	   end,
    Result.

map_create_kubelet_node(Parent,{HostInfo,ClusterId,DeploymentSpecName})->
    Parent!{create_kubelet_node,start_kubelet_node(HostInfo,ClusterId,DeploymentSpecName)}.


check_kubelet_node_start(create_kubelet_node,Vals,_)->
    check_kubelet_node_start(Vals,[]).
check_kubelet_node_start([],StartResult)->
    StartResult;
check_kubelet_node_start([StartResult|T],Acc) ->
    check_kubelet_node_start(T,[StartResult|Acc]).
 
start_kubelet_node({WantedAlias,HostId},ClusterId,DeploymentId)->
 %   DeploymentId="kubelet",
    DeploymentVsn=sd:call(etcd,db_deployment_spec,vsn,[DeploymentId],5*1000),
    Cookie=sd:call(etcd,db_cluster_spec,cookie,[ClusterId],5*1000),
    NodeName="kubelet"++"_"++ClusterId++"_"++HostId,
    Dir=NodeName++".deployment",  
    Result=case kubelet:create_vm_ssh(WantedAlias,NodeName,Dir,Cookie) of
	       {error,Reason}->
		   ?PrintLog(ticket,"error",[Reason,?FUNCTION_NAME,?MODULE,?LINE]),
		   {error,Reason};
	       {badrpc,Reason}->
		   ?PrintLog(ticket,"badrpc",[Reason,?FUNCTION_NAME,?MODULE,?LINE]),
		    {error,[badrpc,Reason]};
	       {ok,Node}->
		   Apps=["support","kubelet"],
		   LoadStart=[{kubelet:load_start_app(Node,AppId,Dir),AppId}||AppId<-Apps],
		   ?PrintLog(debug,"LoadStart",[LoadStart,?FUNCTION_NAME,?MODULE,?LINE]),
		   case [{error,Reason}||{error,Reason}<-LoadStart] of
		       []->
			   DbaseAction=[sd:call(etcd,db_deployment,create,[DeploymentId,DeploymentVsn,Node,Dir,XAppId,HostId,ClusterId,running],5*1000)||{{ok,_Info},XAppId}<-LoadStart],
			   ?PrintLog(debug,"DbaseAction",[DbaseAction,?FUNCTION_NAME,?MODULE,?LINE]),
			   case [R||R<-DbaseAction,
				 {atomic,ok}/=R] of
			       []->
				   {ok,LoadStart};
			       Reason->
				   ?PrintLog(ticket,"error",[Reason,?FUNCTION_NAME,?MODULE,?LINE]),
				   {error,Reason}
			   end
		   end
	   end,
    Result.
%		   case sd:call(etcd,db_kubelet,create,[PodId,HostId,ClusterId,Node,Dir,Node,Cookie,[]],5*1000) of

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
stop_node(Pod,Container,Dir)->
    Result=case container:stop_unload(od,Container,Dir) of
	       {error,Reason}->
		   ?PrintLog(ticket,"error",[Reason,?FUNCTION_NAME,?MODULE,?LINE]),
		   {error,[Reason,Pod,Container,Dir,?FUNCTION_NAME,?MODULE,?LINE]};
	       ok->
		   case pod:stop_node(Pod) of
		       {error,Reason}->
			   ?PrintLog(ticket,"error",[Reason,?FUNCTION_NAME,?MODULE,?LINE]),
			   {error,Reason};
		       ok ->
			   case sd:call(etcd,db_deployment,delete,[Pod],5*1000) of
			       {atomic,ok}->
				   ok;
			       Reason->
				   ?PrintLog(ticket,"error",[Reason,?FUNCTION_NAME,?MODULE,?LINE]),
				   {error,[Reason,Pod,Container,Dir,?FUNCTION_NAME,?MODULE,?LINE]}			       
			   end
		   end
		  
	   end,
    Result.		 
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
load_start(Pod,Container,Dir)->
    Result=case container:load_start(Pod,Container,Dir) of
	       {error,Reason}->
		   ?PrintLog(ticket,"error",[Reason,?FUNCTION_NAME,?MODULE,?LINE]),
		   {error,Reason};
	       ok->
		   case sd:call(etcd,db_kubelet,add_container,[Pod,Container],5*1000) of
		       {atomic,ok}->			   
			   ok;
		       Reason->
			   ?PrintLog(ticket,"error",[Reason,?FUNCTION_NAME,?MODULE,?LINE]),
			   {error,[Reason,Pod,Container,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Result.	    

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
stop_unload(Pod,Container,Dir)->
    Result=case container:stop_unload(Pod,Container,Dir) of
	         {error,Reason}->
		   {error,[Reason,Pod,Container,Dir,?FUNCTION_NAME,?MODULE,?LINE]};
   
	       ok->
		   case sd:call(etcd,db_kubelet,delete_container,[Pod,Container],5*1000) of
		       {atomic,ok}->
			   ok;
		       Reason->
			   {error,[Reason,Pod,Container,Dir,?FUNCTION_NAME,?MODULE,?LINE]}			       
		   end
	   end,
    Result.
