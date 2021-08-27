%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(host).   



-export([
       	 start_node/0,
	 status_all_hosts/0,
	 status/1,
	 update_status/1,
	 read_status/1
	]).

%% ====================================================================
%% External functions
%% ============================ ========================================

%% -------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
start_node()->
    ok.

%% -------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
status_all_hosts()->
    F1=fun get_hostname/2,
    F2=fun check_host_status/3,
    
    AllHosts=db_host_info:read_all(),
   % io:format("AllHosts = ~p~n",[{?MODULE,?LINE,AllHosts}]),
    Status=mapreduce:start(F1,F2,[],AllHosts),
  %  io:format("Status = ~p~n",[{?MODULE,?LINE,Status}]),
    Running=[{running,Alias,HostId,Ip,Port}||{running,Alias,HostId,Ip,Port}<-Status],
    Missing=[{missing,Alias,HostId,Ip,Port}||{missing,Alias,HostId,Ip,Port}<-Status],
    {ok,Running,Missing}.

get_hostname(Parent,{Alias,HostId,IpAddr,Port,User,PassWd})->    
   % io:format("get_hostname= ~p~n",[{?MODULE,?LINE,HostId,User,PassWd,IpAddr,Port}]),
    Msg="hostname",
    Result=rpc:call(node(),my_ssh,ssh_send,[IpAddr,Port,User,PassWd,Msg, 5*1000],4*1000),
  %  io:format("Result, HostId= ~p~n",[{?MODULE,?LINE,Result,HostId}]),
    Parent!{machine_status,{Alias,HostId,IpAddr,Port,Result}}.

check_host_status(machine_status,Vals,_)->
    check_host_status(Vals,[]).

check_host_status([],Status)->
    Status;
check_host_status([{Alias,HostId,IpAddr,Port,[HostId]}|T],Acc)->
    NewAcc=[{running,Alias,HostId,IpAddr,Port}|Acc],
    check_host_status(T,NewAcc);
check_host_status([{Alias,HostId,IpAddr,Port,{error,_Err}}|T],Acc) ->
    check_host_status(T,[{missing,Alias,HostId,IpAddr,Port}|Acc]);
check_host_status([{Alias,HostId,IpAddr,Port,{badrpc,timeout}}|T],Acc) ->
    check_host_status(T,[{missing,Alias,HostId,IpAddr,Port}|Acc]);
check_host_status([X|T],Acc) ->
    check_host_status(T,[{x,X}|Acc]).

%% -------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
read_status(all)->
    AllServers=if_db:server_read_all(),
    AllServersStatus=[{Status,HostId}||{HostId,_User,_PassWd,_IpAddr,_Port,Status}<-AllServers],
    Running=[HostId||{running,HostId}<-AllServersStatus],
    Missing=[HostId||{missing,HostId}<-AllServersStatus],
    [{running,Running},{missing,Missing}];

read_status(XHostId) ->
    AllServers=if_db:server_read_all(),
    [ServersStatus]=[Status||{_Alias,HostId,_User,_PassWd,_IpAddr,_Port,Status}<-AllServers,
		     XHostId==HostId],
    ServersStatus.
					
    
%% -------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
update_status( [{running,Running},{missing,Missing}])->
    [if_db:server_update(HostId,running)||HostId<-Running],
    [if_db:server_update(HostId,Missing)||HostId<-Missing],    
    ok.

%% -------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
status(all)->
    Status=status(),
    Running=[HostId||{running,HostId}<-Status],
    NotAvailable=[HostId||{not_available,HostId}<-Status],
    [{running,Running},{not_available,NotAvailable}];

status(HostId) ->
    Status=status(),
    Result=[XHostIdStatus||{XHostIdStatus,XHostId}<-Status,
	   HostId==XHostId],
    Result.

status()->
    F1=fun get_hostname/2,
    F2=fun check_host_status/3,
    
    AllServers=if_db:server_read_all(),
  %  io:format("AllServers = ~p~n",[{?MODULE,?LINE,AllServers}]),
    Status=mapreduce:start(F1,F2,[],AllServers),
  %  io:format("Status = ~p~n",[{?MODULE,?LINE,Status}]),
    Status.
        

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------



    

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
