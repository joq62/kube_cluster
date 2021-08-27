%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(host_lib).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------


%% External exports
-export([
	
%	 status_host/1,
	 status_hosts/0
%	 restart_host/0
%	 install/0,
%	 start_app/5,
%	 stop_app/4,
%	 app_status/2

	]).

%% ====================================================================
%% External functions
%% ====================================================================
status_hosts()->
     HostInfoList=etcd:host_info_all(),
    check_hosts(HostInfoList).
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
status_host(HostInfo)->
    {host_id,HostId}=lists:keyfind(host_id,1,HostInfo),
    {ip,Ip}=lists:keyfind(ip,1,HostInfo),
    {ssh_port,Port}=lists:keyfind(ssh_port,1,HostInfo),
    {uid,Uid}=lists:keyfind(uid,1,HostInfo),
    {pwd,Pwd}=lists:keyfind(pwd,1,HostInfo),
    case my_ssh:ssh_send(Ip,Port,Uid,Pwd,"hostname",5000) of
	[_HostId]->
	    running;
	Err->
	    missing
    end.
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

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
check_hosts(HostInfoList)->
    F1=fun check_host/2,
    F2=fun host_status/3,
    R1=mapreduce:start(F1,F2,[],HostInfoList),
    Running=[HostInfo||{ok,HostInfo}<-R1],
    Missing=[HostInfo||{Err,[_,HostInfo]}<-R1,
		       ok/=Err],
    [{running,Running},{missing,Missing}].

check_host(Pid,{HostId,Ip,SshPort,UId,Pwd})->
    Result=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,HostId,7000],5000),
%    io:format("Result ~p~n",[{Result, ?MODULE,?LINE}]),
    Pid!{check_host,{Result,{HostId,Ip,SshPort,UId,Pwd}}}.

host_status(Key,Vals,[])->
 %   io:format("~p~n",[{?MODULE,?LINE,Key,Vals}]),
     host_status(Vals,[]).

host_status([],Status)->
    Status;
host_status([{ok,HostInfo}|T],Acc) ->
    host_status(T, [{ok,HostInfo}|Acc]);
host_status([{{Err,Reason},HostInfo}|T],Acc) ->
    host_status(T,[{Err,[Reason,HostInfo]}|Acc]).


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
stop_app(ApplicationStr,Application,Dir,Vm)->
    rpc:call(Vm,os,cmd,["rm -rf "++Dir++"/"++ApplicationStr]),
    rpc:call(Vm,application,stop,[Application]),
    rpc:call(Vm,application,unload,[Application]).
    

start_app(ApplicationStr,Application,CloneCmd,Dir,Vm)->
    rpc:call(Vm,os,cmd,[CloneCmd++" "++Dir++"/"++ApplicationStr]),
    true=rpc:call(Vm,code,add_patha,[Dir++"/"++ApplicationStr++"/ebin"]),
    ok=rpc:call(Vm,application,start,[Application]),
    app_status(Vm,Application).

app_status(Vm,Application)->
    Status = case rpc:call(Vm,Application,ping,[]) of   
		 {pong,_,Application}->
		     running;
		 Err ->
		     {error,[Err]}
	     end,
    Status.

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
