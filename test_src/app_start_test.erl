%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(app_start_test).    
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
-define(APP,iaas).
%% External exports
-export([start/0]).



%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    ?debugMsg("Start setup"),
    ?assertEqual(ok,setup()),
    ?debugMsg("stop setup"),

 %   ?debugMsg("Start testXXX"),
 %   ?assertEqual(ok,single_node()),
 %   ?debugMsg("stop single_node"),
    
      %% End application tests
    ?debugMsg("Start cleanup"),
    ?assertEqual(ok,cleanup()),
    ?debugMsg("Stop cleanup"),

    ?debugMsg("------>"++atom_to_list(?MODULE)++" ENDED SUCCESSFUL ---------"),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

setup()->

    {ok,ClusterIdAtom}=application:get_env(unit_test,cluster_id),
    ClusterId=atom_to_list(ClusterIdAtom),
    os:cmd("rm -rf "++ClusterId),
    ok=file:make_dir(ClusterId),
    {ok,MonitorNodeNameAtom}=application:get_env(unit_test,monitor_node),
    MonitorNodeName=atom_to_list(MonitorNodeNameAtom),
    {ok,HostId}=inet:gethostname(),
    MonitorNode=list_to_atom(MonitorNodeName++"@"++HostId),
    Env=[{cluster_id,ClusterIdAtom},{monitor_node,MonitorNode}],
    ok=application:set_env([{support,Env},
			    {kubelet,Env},
			    {etcd,Env},
			    {cluster,Env}]),
    ok=application:start(support),
    ok=application:start(etcd),
    ok=application:start(kubelet),
    ok=application:start(cluster),

  
    ok.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------    

cleanup()->
  
  %  init:stop(),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
