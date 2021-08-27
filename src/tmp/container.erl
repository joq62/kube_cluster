%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% ToDo 
%%% 1. New cluster
%%% 2. Check existing cluster -> restart missing node vms
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(container).    
   
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
	 unload_stop/2,
	 load/6,
	 start/2
	]).


%% ====================================================================
%% External functions
%% ====================================================================  
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Creates a slave node via HostNode 
%% NodeName=pod_microseconds_clusterId_HostId
%% PodDir=clusterId/pod_microseconds
%% AppDir=PodDir/AppId
%% AppPa=AppDir/ebin
%% Returns: non
%% --------------------------------------------------------------------
unload_stop(AppId,Pod)->
    App=list_to_atom(AppId),
    R1=rpc:call(Pod,application,unload,[App],2*1000),
    R2=rpc:call(Pod,application,stop,[App],2*1000),
    {R1,R2}.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Creates a slave node via HostNode 
%% NodeName=pod_microseconds_clusterId_HostId
%% PodDir=clusterId/pod_microseconds
%% AppDir=PodDir/AppId
%% AppPa=AppDir/ebin
%% Returns: non
%% --------------------------------------------------------------------
load(AppId,_AppVsn,GitPath,AppEnv,Pod,Dir)->
    Result = case rpc:call(Pod,application,which_applications,[],5*1000) of
		 {badrpc,Reason}->
		     {error,[badrpc,Reason,?FUNCTION_NAME,?MODULE,?LINE]};
		 LoadedApps->
		     case lists:keymember(list_to_atom(AppId),1,LoadedApps) of
			 true->
			     ?PrintLog(log,'Already loaded',[AppId,Pod,?FUNCTION_NAME,?MODULE,?LINE]),
			     {error,['Already loaded',AppId,Pod]};
			 false ->
			     AppDir=filename:join(Dir,AppId),
			     AppEbin=filename:join(AppDir,"ebin"),
			     App=list_to_atom(AppId),
			     rpc:call(Pod,os,cmd,["rm -rf "++AppId],25*1000),
			     _GitResult=rpc:call(Pod,os,cmd,["git clone "++GitPath],25*1000),
				%	   ?PrintLog(log,"GitResult",[PodNode,GitPath,GitResult,?FUNCTION_NAME,?MODULE,?LINE]),
			     _MVResult=rpc:call(Pod,os,cmd,["mv "++AppId++" "++AppDir],25*1000),
				%	   ?PrintLog(log,"MVResult",[AppId,AppDir,MVResult,?FUNCTION_NAME,?MODULE,?LINE]),
			     true=rpc:call(Pod,code,add_patha,[AppEbin],22*1000),
			     ok=rpc:call(Pod,application,set_env,[[{App,AppEnv}]]),		       
			     ok
		     end
	     end,
    Result.

start(AppId,Pod)->
    App=list_to_atom(AppId),
    ?PrintLog(debug,"App,Pod",[App,Pod,?FUNCTION_NAME,?MODULE,?LINE]),
    Result=case rpc:call(Pod,application,start,[App],2*60*1000) of
	       ok->
		   ok;
	       {error,{already_started}}->
		   ok;
	       {Error,Reason}->
		   {Error,[Reason,application,Pod,start,App,?FUNCTION_NAME,?MODULE,?LINE]}
	   end,
    Result.
