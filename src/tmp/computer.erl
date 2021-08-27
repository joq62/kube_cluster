%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(computer). 



-export([get_computer_status/1,
	 update_status_computers/0,
	 start_computer/2,
	 clean_computer/1,
	 clean_computer/2
	]).

-define(DbaseVmId,"10250").
-define(HostVmId,"10250").
-define(TimeOut,3000).
-define(ControlVmIds,["10250"]).

%% ====================================================================
%% External functions
%% ============================ ========================================
% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get_computer_status(Key)->
    F1=fun get_hostname/2,
    F2=fun check_host_status/3,
    
    Computers=db_computer:read_all(),
 %   io:format("Computers = ~p~n",[{?MODULE,?LINE,Computers}]),
    Status=mapreduce:start(F1,F2,[],Computers),
 %   io:format("Computers Status = ~p~n",[{?MODULE,?LINE,Status}]),
    Running=[db_computer:update(Host,running)||{running,Host}<-Status],
 %   io:format("Running = ~p~n",[{?MODULE,?LINE,Running}]),
    Available=[db_computer:update(Host,available)||{available,Host}<-Status],
 %   io:format("Available = ~p~n",[{?MODULE,?LINE,Available}]),
    NotAvailable=[db_computer:update(Host,not_available)||{not_available,Host}<-Status],
 %   io:format("NotAvailable = ~p~n",[{?MODULE,?LINE,NotAvailable}]),
    Result=case Key of
	       running->
		   Running;
	       available->
		   Available;
	       not_available->
		   NotAvailable
	   end,
    Result.
        

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

%@doc, spec etc

update_status_computers()->
    F1=fun get_hostname/2,
    F2=fun check_host_status/3,
    {ok,HostId}=net:gethostname(),
    DbaseVm=list_to_atom(?DbaseVmId++"@"++HostId),
    Computers=rpc:call(DbaseVm,if_db,computer_read_all,[]),
%    io:format("Computers = ~p~n",[{?MODULE,?LINE,Computers}]),
    Status=mapreduce:start(F1,F2,[],Computers),
 %   io:format("Computers Status = ~p~n",[{?MODULE,?LINE,Status}]),
    _Running=[if_db:computer_update(Host,running)||{running,Host}<-Status],
  %  io:format("Running = ~p~n",[{?MODULE,?LINE,Running}]),
    _Available=[if_db:computer_update(Host,available)||{available,Host}<-Status],
  %  io:format("Available = ~p~n",[{?MODULE,?LINE,Available}]),
    _NotAvailable=[if_db:computer_update(Host,not_available)||{not_available,Host}<-Status],
  %  io:format("NotAvailable = ~p~n",[{?MODULE,?LINE,NotAvailable}]),
    ok.
    
   

		  
get_hostname(Parent,{HostId,User,PassWd,IpAddr,Port,_Status})->    
    Msg="hostname",
    Result=my_ssh:ssh_send(IpAddr,Port,User,PassWd,Msg,?TimeOut),
   % io:format("Result, HostId= ~p~n",[{?MODULE,?LINE,Result,HostId}]),
    Parent!{computer_status,{HostId,Result}}.

check_host_status(computer_status,Vals,_)->
    Result=check_host_status(Vals,[]),
    Result.

check_host_status([],Status)->
    Status;
check_host_status([{HostId,[HostId]}|T],Acc)->
    HostVm=list_to_atom(?DbaseVmId++"@"++HostId),
  %  io:format("HostVm = ~p~n",[{?MODULE,?LINE,net_adm:ping(HostVm),HostVm}]),
    NewAcc=case net_adm:ping(HostVm) of
	       pong->
		   [{running,HostId}|Acc];
	       pang->
		   [{available,HostId}|Acc]
	   end,
  %  io:format("NewAcc = ~p~n",[{?MODULE,?LINE,NewAcc}]),
    check_host_status(T,NewAcc);
check_host_status([{HostId,{error,_Err}}|T],Acc) ->
    check_host_status(T,[{not_available,HostId}|Acc]).


% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
clean_computer(HostId)->

% [{'30000@asus',"asus","30000",controller,not_available}]=db_vm:host_id(HostId),
% [{"asus","pi","festum01","192.168.0.100",60110,not_available}]=db_computer:read(HostId)),
    Result=case if_db:computer_read(HostId) of
	       []->
		   {error,[eexists,HostId,?MODULE,?LINE]};
	       [{HostId,User,PassWd,IpAddr,Port,_ComputerStatus}]->
		   case if_db:vm_host_id(HostId) of
		       []->
			   {error,[eexists_vms,HostId,?MODULE,?LINE]};
		       VmIdList->
			   do_clean(VmIdList,HostId,User,PassWd,IpAddr,Port)			       
						%io:format("VmId = ~p",[{VmId,?MODULE,?LINE}])
		   end
	   end,
    Result.
do_clean([],_,_,_,_,_)->
    ok;
do_clean([{Vm,_HostId,VmId,_Type,_VmStatus}|T],HostId,User,PassWd,IpAddr,Port)->
    if_db:vm_update(Vm,not_available),
    [if_db:sd_delete(ServiceId,Vsn,ServiceVm)||{ServiceId,Vsn,_XHostId,_VmId,ServiceVm}<-if_db:sd_read_all(),
							      ServiceVm==Vm],
    rpc:call(Vm,init,stop,[],5000),
    ok=my_ssh:ssh_send(IpAddr,Port,User,PassWd,"rm -rf "++VmId,2*?TimeOut),
    do_clean(T,HostId,User,PassWd,IpAddr,Port).
% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
clean_computer(HostId,VmId)->
    % Read computer info 
    Result=case if_db:computer_read(HostId) of
	       []->
		   {error,[eexists,HostId]};
	       [{HostId,User,PassWd,IpAddr,Port,_Status}]->
		   Vm=list_to_atom(VmId++"@"++HostId),
		   if_db:vm_update(Vm,not_available),
		   [if_db:sd_delete(ServiceId,Vsn,ServiceVm)||{ServiceId,Vsn,_HostId,_VmId,ServiceVm}<-if_db:sd_read_all(),
							      ServiceVm==Vm],
		   Vm=list_to_atom(VmId++"@"++HostId),
		   rpc:call(Vm,init,stop,[],1000),
		   ok=my_ssh:ssh_send(IpAddr,Port,User,PassWd,"rm -rf "++VmId,2*?TimeOut)
	    %	    io:format("VmId = ~p",[{VmId,?MODULE,?LINE}])
		       
	   end,
    Result.

start_computer(HostId,VmId)->
    io:format("start_computer HostId,VmId = ~p~n",[{HostId,VmId,?MODULE,?LINE}]),
    StartResult=case if_db:computer_read(HostId) of
		    []->
			{error,[eexists,HostId,?MODULE,?LINE]};
		    [{HostId,User,PassWd,IpAddr,Port,_Status}]->
			io:format("HostId,User,PassWd,IpAddr,Port= ~p~n",[{HostId,User,PassWd,IpAddr,Port,?MODULE,?LINE}]),
			io:format("mkdir = ~p~n",[{my_ssh:ssh_send(IpAddr,Port,User,PassWd,"mkdir "++VmId,2*?TimeOut),?MODULE,?LINE}]),
			io:format("erl  = ~p~n",[{my_ssh:ssh_send(IpAddr,Port,User,PassWd,"erl -sname "++VmId++" -setcookie abc -detached ",2*?TimeOut),?MODULE,?LINE}]),
			io:format(" = ~p~n",[{?MODULE,?LINE}]),
		%	ok=my_ssh:ssh_send(IpAddr,Port,User,PassWd,"mkdir "++VmId,3*?TimeOut),
		%	ok=my_ssh:ssh_send(IpAddr,Port,User,PassWd,"erl -sname "++VmId++" -setcookie abc -detached ",3*?TimeOut),
			Vm=list_to_atom(VmId++"@"++HostId),
			io:format("Vm = ~p~n",[{Vm,?MODULE,?LINE}]),
			R=check_started(50,Vm,10,{error,[Vm]}),
			io:format("check_started = ~p~n",[{R,?MODULE,?LINE}]),
			case R of
			    ok->
				rpc:call(Vm,mnesia,start,[]),
				if_db:computer_update(HostId,running),
				if_db:vm_update(Vm,allocated),
			        % starta common !!!!!
				{ok,HostId};
			    Err->
				{error,[Err,Vm,?MODULE,?LINE]}
			end			
		end,
    io:format("StartResult= ~p~n",[{StartResult,?MODULE,?LINE}]),
    StartResult.


check_started(_N,_Vm,_Timer,ok)->
    ok;
check_started(0,_Vm,_Timer,Result)->
    Result;
check_started(N,Vm,Timer,_Result)->
    NewResult=case net_adm:ping(Vm) of
		  pong->
		      ok;
		  Err->
		      timer:sleep(Timer),
		      {error,[Err,Vm]}
	      end,
    check_started(N-1,Vm,Timer,NewResult).



