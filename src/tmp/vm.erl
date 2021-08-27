%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%%  c
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(vm). 



-export([start_vm/2,
	 start_vms/2,
	 clean_vm/2,
	 clean_vms/2,
	 vm_status/2,
	 allocate/0,
	 free/1,
%
	 status_vms/2,
	 candidates/2
	]).


-define(DbaseVmId,"10250").
-define(ControlVmId,"10250").
-define(TimeOut,3000).

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
allocate()->
    {ok,DbaseHostId}=inet:gethostname(),
    DbaseVm=list_to_atom(?DbaseVmId++"@"++DbaseHostId),
    Result= case rpc:call(DbaseVm,db_vm,status,[free],5000) of
		[]->
		    {error,[no_free_vms,?MODULE,?LINE]};
		FreeVms ->
		    [{HostId,VmId}|_]=FreeVms,
		    Vm=list_to_atom(VmId++"@"++HostId),
		    rpc:call(DbaseVm,db_vm,update,[Vm,allocated],5000),
		    {ok,Vm}
	    end,
    Result.

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
free(Vm)->
    {ok,DbaseHostId}=inet:gethostname(),
    DbaseVm=list_to_atom(?DbaseVmId++"@"++DbaseHostId),
    Result= case rpc:call(DbaseVm,db_vm,info,[Vm],5000) of
		[]->
		    {error,[eexist,Vm,?MODULE,?LINE]};
		[{_,HostId,VmId,_,_}]->
		    clean_vm(VmId,HostId) 
	    end,
    Result.

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_vm(VmId,HostId)->
    {ok,DbaseHostId}=inet:gethostname(),
    DbaseVm=list_to_atom(?DbaseVmId++"@"++DbaseHostId),
    StartResult=case rpc:call(DbaseVm,db_computer,read,[HostId]) of
		    []->
			{error,[eexists,HostId,?MODULE,?LINE]};
						%[{HostId,User,PassWd,IpAddr,Port}]->
		    _->
			ControlVm=list_to_atom(?ControlVmId++"@"++HostId),
			ok=rpc:call(ControlVm,file,make_dir,[VmId]),
			[]=rpc:call(ControlVm,os,cmd,["erl -sname "++VmId++" -setcookie abc -detached "],2*?TimeOut),
			Vm=list_to_atom(VmId++"@"++HostId),
			R=check_started(500,Vm,10,{error,[Vm]}),
			case R of
			    ok->
				db_vm:update(Vm,free),
				{ok,Vm};
			    Err->
				{error,[Err,Vm,?MODULE,?LINE]}
			end
		end,
    StartResult.

start_vms(VmIds,HostId)->
    F1=fun start_node/2,
    F2=fun start_node_result/3,
%    L=[{XHostId,XVmId}||XVmId<-VmIds],
    L=[{HostId,VmId}||VmId<-VmIds],
    ResultNodeStart=mapreduce:start(F1,F2,[],L),
    ResultNodeStart.


start_node(Parent,{HostId,VmId})->
    {ok,DbaseHostId}=inet:gethostname(),
    DbaseVm=list_to_atom(?DbaseVmId++"@"++DbaseHostId),
    StartResult=case rpc:call(DbaseVm,db_computer,read,[HostId]) of
		    []->
			{error,[eexists,HostId,?MODULE,?LINE]};
		    %[{HostId,User,PassWd,IpAddr,Port}]->
		    _->
			ControlVm=list_to_atom(?ControlVmId++"@"++HostId),
			ok=rpc:call(ControlVm,file,make_dir,[VmId]),
			[]=rpc:call(ControlVm,os,cmd,["erl -sname "++VmId++" -setcookie abc -detached "],2*?TimeOut),
			Vm=list_to_atom(VmId++"@"++HostId),
			R=check_started(500,Vm,10,{error,[Vm]}),
			io:format("~p~n",[{?MODULE,?LINE,HostId,VmId,R}]),
			case R of
			    ok->
				db_vm:update(Vm,free),
				{ok,Vm};
			    Err->
				{error,[Err,Vm,?MODULE,?LINE]}
			end
		end,
    Parent!{start_node,StartResult}.

start_node_result(start_node,Vals,_)->		
    Vals.


% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------


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


% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
clean_vm(VmId,HostId)->
    {ok,DbaseHostId}=inet:gethostname(),
    DbaseVm=list_to_atom(?DbaseVmId++"@"++DbaseHostId),
    Result=case rpc:call(DbaseVm,db_computer,read,[HostId]) of
	       []->
		   {error,[eexists,HostId,?MODULE,?LINE]};
	     %  [{HostId,User,PassWd,IpAddr,Port}]->
	       _->
						%	    ok=rpc:call(list_to_atom(?ControlVmId++"@"++HostId),
						%			file,del_dir_r,[VmId]),
%		   io:format("HostId,VmId ~p~n",[{?MODULE,?LINE,HostId,VmId}]),
		   ControlVm=list_to_atom(?ControlVmId++"@"++HostId),
		   rpc:call(ControlVm,os,cmd,["rm -rf "++VmId]),
		   R=rpc:call(ControlVm,filelib,is_dir,[VmId]),
		   timer:sleep(300),
		   Vm=list_to_atom(VmId++"@"++HostId),
		   rpc:call(Vm,init,stop,[]),
		   rpc:call(DbaseVm,db_vm,update,[Vm,not_available],5000),	     	   
		   timer:sleep(300),
		   timer:sleep(300),
%		   io:format("rm -rf VmId = ~p~n",[{R,VmId,?MODULE,?LINE}]),
		   {R,VmId}
    end,
    Result.

clean_vms(VmIds,HostId)->
    F1=fun clean_node/2,
    F2=fun clean_node_result/3,
%    io:format("HostId,VmIds ~p~n",[{?MODULE,?LINE,HostId,VmIds}]),
    L=[{HostId,XVmId}||XVmId<-VmIds],
%    io:format("L  ~p~n",[{?MODULE,?LINE,L}]),
    ResultNodeStart=mapreduce:start(F1,F2,[],L),
    ResultNodeStart.

clean_node(Parent,{HostId,VmId})->
    % Read computer info 
    {ok,DbaseHostId}=inet:gethostname(),
    DbaseVm=list_to_atom(?DbaseVmId++"@"++DbaseHostId),
    Result=case rpc:call(DbaseVm,db_computer,read,[HostId]) of
	       []->
		   {error,[eexists,HostId,?MODULE,?LINE]};
	     %  [{HostId,User,PassWd,IpAddr,Port}]->
	       _->
						%	    ok=rpc:call(list_to_atom(?ControlVmId++"@"++HostId),
						%			file,del_dir_r,[VmId]),
%		   io:format("HostId,VmId ~p~n",[{?MODULE,?LINE,HostId,VmId}]),
		   ControlVm=list_to_atom(?ControlVmId++"@"++HostId),
		   rpc:call(ControlVm,os,cmd,["rm -rf "++VmId]),
		   R=rpc:call(ControlVm,filelib,is_dir,[VmId]),
		   timer:sleep(300),
		   Vm=list_to_atom(VmId++"@"++HostId),
		   rpc:call(Vm,init,stop,[]),
		   rpc:call(DbaseVm,db_vm,update,[Vm,not_available],5000),		   
		   timer:sleep(300),
%		   io:format("rm -rf VmId = ~p~n",[{R,VmId,?MODULE,?LINE}]),
		   {R,VmId}
    end,
    Parent!{clean_node,Result}.

clean_node_result(_Key,Vals,_)->		
    Vals.

%% ====================================================================
%% External functions
%% ====================================================================

vm_status(VmStatus,Type)->
    Reply= case Type of
	       running->
		   [{HostId,R}||{HostId,
				 {running,R},
				 {available,_A},
				 {not_available,_NA}}<-VmStatus];
	       available->
		   [{HostId,A}||{HostId,
				 {running,_R},
				 {available,A},
				 {not_available,_NA}}<-VmStatus];
	       not_available->
		   [{HostId,NA}||{HostId,
				 {running,_R},
				  {available,_A},
				 {not_available,NA}}<-VmStatus];
	       Err->
		   {error,[edefined,Err]}
	   end,
    Reply.
% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
% [{HostId,[{HostId,VmId}]} 
%[{HostId1,VmId1},{HostId2,VmId1},{HostIdN,VmId1},{HostId1,Vmid2},
% 
% 
%
candidates(CandidateList,RunningVms)->
 %   io:format( "CandidateList : ~p~n",[{?MODULE,?LINE,CandidateList}]), 
    R =x1(RunningVms,[]),
    AddToCandidateList=[{HostId,VmId}||{HostId,VmId}<-R,
				       false==lists:member({HostId,VmId},CandidateList)],
    L2=[{HostId,VmId}||{HostId,VmId}<-CandidateList,
		       lists:member({HostId,VmId},R)],
    NewCandidateList=lists:append(AddToCandidateList,L2),
 %  io:format( "NewCandidateList : ~p~n",[{?MODULE,?LINE,NewCandidateList}]), 
    NewCandidateList.
x1([],R)->
    R;
x1(L,Acc)->
 %   io:format( "L : ~p~n",[{?MODULE,?LINE,L}]),  
    Lx= [{HostId,VmId}||{_,[{HostId,VmId}|_]}<-L],
 %   io:format( "Lx : ~p~n",[{?MODULE,?LINE,Lx}]),  
 
    case Lx of
	[]->
	    T=[],
	    NewAcc=Acc;
	Lx->
	    T=x2(Lx,L),
	    NewAcc=lists:append(Lx,Acc)
    end,	      
    x1(T,NewAcc).

x2([],R)->
    R;
x2([{HostId,VmId}|T],Acc)->
    case lists:keyfind(HostId,1,Acc) of
	false->
	    NewAcc=Acc;
	{HostId,L} ->
%	    io:format( "Acc : ~p~n",[{?MODULE,?LINE,Acc}]), 
%	    io:format( "HostId,L : ~p~n",[{?MODULE,?LINE,HostId,L}]), 
	    NewL=lists:delete({HostId,VmId},L),
%	    io:format( "NewL : ~p~n",[{?MODULE,?LINE,NewL}]),  
	    NewAcc=lists:keyreplace(HostId,1,Acc,{HostId,NewL})
    end,
    x2(T,NewAcc).

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

%@doc, spec etc

status_vms(HostId,VmIds)->
    F1=fun do_ping/2,
    F2=fun check_vm_status/3,

    io:format("HostId,VmIds  ~p~n",[{?MODULE,?LINE,HostId,VmIds}]),
    Vms=[{HostId,VmId,list_to_atom(VmId++"@"++HostId)}||VmId<-VmIds],
    Status=mapreduce:start(F1,F2,[],Vms),
    Running=[{HostIdX,VmIdX}||{running,HostIdX,VmIdX}<-Status],
    Available=[{HostIdX,VmIdX}||{available,HostIdX,VmIdX}<-Status],  
    NotAvailable=[{HostIdX,VmIdX}||{not_available,HostIdX,VmIdX}<-Status],   
    {HostId,{running,Running},{available,Available},{not_available,NotAvailable}}.
		  
do_ping(Parent,{HostId,VmId,Vm})->
    Result=net_adm:ping(Vm),
    Parent!{vm_status,{HostId,VmId,Result}}.

check_vm_status(vm_status,Vals,_)->
    Result=check_vm_status(Vals,[]),
    Result.

check_vm_status([],Status)->
    Status;
check_vm_status([{HostId,VmId,Result}|T],Acc)->
  %  io:format("Vm  ~p~n",[{?MODULE,?LINE,Vm,Result}]),
    NewAcc=case Result of
	       pong->
		   [{running,HostId,VmId}|Acc];
	       pang->
		   [{available,HostId,VmId}|Acc];
	       _ ->
		   [{not_available,HostId,VmId}|Acc]
	   end,
    check_vm_status(T,NewAcc).

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
