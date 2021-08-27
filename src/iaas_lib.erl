%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(iaas_lib).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(MaxSlaves,10).
%% --------------------------------------------------------------------

% New final ?

-export([slave_start/1,
	 slave_start/5,
	 slave_stop/1,
	 slave_arg/3,
	 slave_arg/4

	]).

%% External exports




%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
create_slave_dir(HostNode,ClusterId,NodeName)->
    SlaveDir=filename:join(ClusterId,NodeName),
    rpc:call(HostNode,os,cmd,["rm -rf "++SlaveDir],5*1000),
    ok=rpc:call(HostNode,file,make_dir,[SlaveDir],5*1000),
    {ok,SlaveDir,HostNode}.
    

slave_start({HostId,HostNode,ClusterId,NodeName,Arg})->
    slave_start(HostId,HostNode,ClusterId,NodeName,Arg).

slave_start(HostId,HostNode,ClusterId,NodeName,Arg)->
    {ok,_SlaveDir,HostNode}=create_slave_dir(HostNode,ClusterId,NodeName),
    {ok,SlaveNode}=rpc:call(HostNode,slave,start,[HostId,NodeName,Arg],5*1000),
    pong=net_adm:ping(SlaveNode),
    {ok,SlaveNode}.

slave_stop(SlaveNode)->
    slave:stop(SlaveNode).

slave_arg(HostId,ClusterId,Node)->
    slave_arg(?MaxSlaves,HostId,ClusterId,Node,[]).

slave_arg(N,HostId,ClusterId,Node)->
    slave_arg(N,HostId,ClusterId,Node,[]).
slave_arg(0,_HostId,_ClusterId,_Node,SlaveNames)->
    SlaveNames;
slave_arg(N,HostId,ClusterId,Node,Acc)->
    Cookie=atom_to_list(rpc:call(Node,erlang,get_cookie,[],2*1000)),
    NStr=integer_to_list(N),
    NodeName=HostId++"_"++ClusterId++"_"++"slave"++NStr,
    Arg="-setcookie "++Cookie,
   slave_arg(N-1,HostId,ClusterId,Node,[{HostId,Node,ClusterId,NodeName,Arg}|Acc]).

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
