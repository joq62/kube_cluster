%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(iaas_leader).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records & defintions
%%---------------------------------------------------------------------
-define(ControllerLeaderTime,30).

%%---- ConfigDir

%%---- HostFile Info 
-define(HostPath,"https://github.com/joq62/host_config.git").
-define(HostFile,"hosts.config").
-define(HostDir,"host_config").
%%---- Catalog
-define(CatalogPath,"https://github.com/joq62/catalog.git").
-define(CatalogFile,"application.catalog").
-define(CatalogDir,"catalog").
%%---- Cluster config
-define(ClusterPath,"https://github.com/joq62/cluster_config.git").
-define(ClusterFile,"cluster.config").
-define(ClusterDir,"cluster_config").
%%----

%% --------------------------------------------------------------------
-export([start/0]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start()->
    ok=etcd_lib:init(),
    %% Create tables and load inital data
    %--------------- lock
    ok=db_lock(),
    ok=db_host_info(),
    ok=db_catalog(),
    ok=db_cluster_info(),
   
    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
db_lock()->
    ok=db_lock:create_table(),
    {atomic,ok}=db_lock:create(controller_leader,?ControllerLeaderTime),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
db_host_info()->
    ok=db_host_info:create_table(),
    Dir=?HostDir,
    GitPath=?HostPath,
    FileName=?HostFile,
    FullFileName=filename:join(Dir,FileName),
    os:cmd("rm -rf "++Dir),
    os:cmd("git clone "++GitPath),
    Reply=case filelib:is_file(FullFileName) of
	      true->
		  {ok,Info}=file:consult(FullFileName),
		  [db_host_info:create(HostId,Ip,SshPort,UId,Pwd)||
		      [{host_id,HostId},
		       {ip,Ip},
		       {ssh_port,SshPort},
		       {uid,UId},
		       {pwd,Pwd}]<-Info],
		  ok;
	      false->
		  {error,[noexist,FileName]}
	  end,
    Reply.
 
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
db_catalog()->
    ok=db_catalog:create_table(),
    Dir=?CatalogDir,
    GitPath=?CatalogPath,
    FileName=?CatalogFile,
    FullFileName=filename:join(Dir,FileName),
    os:cmd("rm -rf "++Dir),
    os:cmd("git clone "++GitPath),
    Reply=case filelib:is_file(FullFileName) of
	      true->
		  {ok,Info}=file:consult(FullFileName),
		  [db_catalog:create(XApplication,XVsn,XGitPath)||
		      {XApplication,XVsn,XGitPath}<-Info],
		  ok;
	      false->
		  {error,[noexist,FileName]}
	  end,
    Reply.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
db_cluster_info()->
    ok=db_cluster_info:create_table(),
    Dir=?ClusterDir,
    GitPath=?ClusterPath,
    FileName=?ClusterFile,
    FullFileName=filename:join(Dir,FileName),
    os:cmd("rm -rf "++Dir),
    os:cmd("git clone "++GitPath),
    Deployed=false,
    ControllerVms=[],
    Reply=case filelib:is_file(FullFileName) of
	      true->
		  {ok,Info}=file:consult(FullFileName),
		  [db_cluster_info:create(ClusterName,NumControllers,Hosts,Cookie,ControllerVms,Deployed)||
		      [{cluster_name,ClusterName},
		       {num_controllers,NumControllers},
		       {hosts,Hosts},
		       {cookie,Cookie}]<-Info],
		  ok;
	      false->
		  {error,[noexist,FileName]}
	  end,
    Reply.
 
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

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
load_config(Dir,HostFile,GitCmd)->
    os:cmd("rm -rf "++Dir),
    os:cmd(GitCmd),
    Reply=case filelib:is_file(HostFile) of
	      true->
		  ok;
	      false->
		  {error,[noexist,HostFile]}
	  end,
    Reply.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
read_config(HostFile)->
    Reply=case filelib:is_file(HostFile) of
	      true->
		  file:consult(HostFile);
	      false->
		  {error,[noexist,HostFile]}
	  end,
    Reply.
