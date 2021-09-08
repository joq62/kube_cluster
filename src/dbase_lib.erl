%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_lib).   
 
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(ClusterConfigPath,"https://github.com/joq62/cluster_config.git").
-define(ClusterConfigDirName,"cluster_config").
-define(ClusterConfigFile,"cluster_config/cluster.config").
-define(ClusterConfigFileName,"cluster.config").

-define(HostConfigPath,"https://github.com/joq62/host_config.git").
-define(HostConfigDirName,"host_config").
-define(HostConfigFile,"host_config/hosts.config").
-define(HostConfigFileName,"hosts.config").

-define(PodSpecsPath,"https://github.com/joq62/pod_specs.git").
-define(PodSpecsDirName,"pod_specs").

-define(DeploymentSpecsPath,"https://github.com/joq62/deployment.git").
-define(DeploymentSpecsDirName,"deployment").

-define(CatalogInfoPath,"https://github.com/joq62/catalog.git").
-define(CatalogInfoDirName,"catalog").


-define(TempDir,"temp_dir").

-define(LockId,cluster).
%% --------------------------------------------------------------------


%% External exports
-export([
	 lock_id/0,
	 check_mnesia_status/0,
	 initial_start_mnesia/0,
	 init_tables/0,
	 create_tables/1,
	 init_distributed_mnesia/1,
	 add_nodes/1
	]).

-define(WAIT_FOR_TABLES,5000).

%% ====================================================================
%% External functions
%% ====================================================================
lock_id()->
    ?LockId.

check_mnesia_status()->
    Status=case mnesia:system_info() of
	       no->
		   mnesia_not_started;
	       yes ->
		   case mnesia:system_info(tables) of
		       []->
			   {error,[mnesia,system_info,?FUNCTION_NAME,?MODULE,?LINE]};
		       [schema]->
			  mnesia_started;
		       _Tables ->
			   mnesia_started_tables_initiated
		   end
	   end,
    Status.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
initial_start_mnesia()->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    ok.
init_tables()->
    ok=db_host_info:create_table(),
    ok=init_host_info(),

    ok=db_cluster_info:create_table(),
    ok=init_cluster_info(),

    ok=db_lock:create_table(),
    {atomic,ok}=db_lock:create(?LockId,0),
    true=db_lock:is_open(?LockId),    

  %  ok=db_app_info:create_table(),
    ok=init_app_info(),
      
    ok.

init_distributed_mnesia(Nodes)->
    StopResult=[rpc:call(Node,mnesia,stop,[],5*1000)||Node<-Nodes],
    Result=case [Error||Error<-StopResult,Error/=stopped] of
	       []->
		   case mnesia:delete_schema(Nodes) of
		       ok->
			   StartResult=[rpc:call(Node,mnesia,start,[],5*1000)||Node<-Nodes],
			   case [Error||Error<-StartResult,Error/=ok] of
			       []->
				   ok;
			       Reason->
				   {error,[Reason,?FUNCTION_NAME,?MODULE,?LINE]}
			   end;
		       Reason->
			   {error,[Reason,?FUNCTION_NAME,?MODULE,?LINE]}
		   end;
	       Reason->
		   {error,[Reason,?FUNCTION_NAME,?MODULE,?LINE]}
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
create_tables(Nodes)->
    %% 
 %   ok=db_host_info:create_table(),
    [db_host_info:add_node(Node,ram_copies)||Node<-Nodes],
  %  ok=init_host_info(),

   % ok=db_cluster_info:create_table(),
    [db_host_info:add_node(Node,ram_copies)||Node<-Nodes],
    %ok=init_cluster_info(),

  
   % ok=db_lock:create_table(),
    [db_lock:add_node(Node,ram_copies)||Node<-Nodes],
 %   {atomic,ok}=db_lock:create(?LockId,0),
 %   false=db_lock:is_open(?LockId),
    ok.

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
add_nodes(Nodes)->
    [db_host_info:add_node(Node,ram_copies)||Node<-Nodes],
    [db_cluster_info:add_node(Node,ram_copies)||Node<-Nodes],
    [db_lock:add_node(Node,ram_copies)||Node<-Nodes],
    [db_app_info:add_node(Node,ram_copies)||Node<-Nodes],
    ok.

    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
init_host_info()->
    os:cmd("rm -rf "++?HostConfigDirName),
    os:cmd("git clone "++?HostConfigPath),
    HostConfigFile=filename:join([?HostConfigDirName,?HostConfigFileName]),
    {ok,Info}=file:consult(HostConfigFile),
   % io:format("~p~n",[{Debug,?MODULE,?LINE}]),
    ok=init_host_info(Info,[]),
    os:cmd("rm -rf "++?HostConfigDirName),
    ok.
init_host_info([],Result)->
    R=[R||R<-Result,
	  R/={atomic,ok}],
    case R of
	[]->
	    ok;
	R->
	    {error,[R]}
    end;
    
init_host_info([[{host_id,HostId},{ip,Ip},{ssh_port,SshPort},{uid,UId},{pwd,Pwd}]|T],Acc)->
    R=db_host_info:create(HostId,Ip,SshPort,UId,Pwd),
    init_host_info(T,[R|Acc]).
    

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
init_cluster_info()->
    ok=db_cluster_info:create_table(), 
    {ok,ClusterId}=application:get_env(cluster_id),
    {ok,MonitorNodeName}=application:get_env(monitor_node),
    {ok,HostId}=inet:gethostname(),
    MonitorNode=list_to_atom(MonitorNodeName++"@"++HostId),
    {ok,Cookie}=application:get_env(cookie),
    {atomic,ok}=db_cluster_info:create(ClusterId,MonitorNode,Cookie),
    io:format("ClusterId,MonitorNode,Cookie ~p~n",[{ClusterId,MonitorNode,Cookie,?MODULE,?LINE}]),
    ok.

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
init_cluster_specs()->
    os:cmd("rm -rf "++?ClusterConfigDirName),
    os:cmd("git clone "++?ClusterConfigPath),
    ClusterConfigFile=filename:join([?ClusterConfigDirName,?ClusterConfigFileName]),
    {ok,Info}=file:consult(ClusterConfigFile),
    ok=db_cluster_spec:create_table(),
    ok=init_cluster_specs(Info,[]),
    os:cmd("rm -rf "++?ClusterConfigDirName),
    ok.
init_cluster_specs([],Result)->
    R=[R||R<-Result,
	  R/={atomic,ok}],
    case R of
	[]->
	    ok;
	R->
	    {error,[R]}
    end;
    
init_cluster_specs([[{cluster_name,ClusterName},{hosts,Hosts},{cookie,Cookie}]|T],Acc)->
    R=db_cluster_spec:create(ClusterName,Hosts,Cookie),
    init_cluster_specs(T,[R|Acc]).
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
init_app_info()->
    os:cmd("rm -rf "++?CatalogInfoDirName),
    os:cmd("git clone "++?CatalogInfoPath),
    {ok,FileNames}=file:list_dir(?CatalogInfoDirName),
    AppFileNames=[filename:join([?CatalogInfoDirName,FileName])||FileName<-FileNames,
								 ".app"==filename:extension(FileName)],
    
    
    AppInfo=[file:consult(AppFileName)||AppFileName<-AppFileNames],
    ok=db_app_info:create_table(),
    ok=init_app_info(AppInfo,[]),
    os:cmd("rm -rf "++?CatalogInfoDirName),
    ok.

init_app_info([],Result)->
    R=[R||R<-Result,
	  R/={atomic,ok}],
    case R of
	[]->
	    ok;
	R->
	    {error,[R]}
    end;    

init_app_info([{ok,[{application,App,Info}]}|T],Acc)->
    R=db_app_info:create(App,Info),
    init_app_info(T,[R|Acc]).
  
