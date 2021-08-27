%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Manage Computers
%%% 
%%% Created : 
%%% -------------------------------------------------------------------
-module(iaas). 

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("timeout.hrl").
%-include("log.hrl").
%-include("config.hrl").
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state, {computer_running,
		computer_available,
		computer_not_available,
		vms_running,
		vms_available,
		vms_not_available,
		vm_candidates}).



%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------
-define(HbInterval,20*1000).
-define(ControlVmId,"10250").
-define(WorkerVmIds,["30000","30001","30002","30003","30004","30005","30006","30007","30008","30009"]).

-export([vm_status/1,computer_status/1,
	 start_node/3,stop_node/1,
	 active/0,passive/0,all/0,
	 get_vm/0,get_vm/2,get_all_vms/0,
	 log/0
	]).

-export([start/0,
	 stop/0,
	 ping/0,
	 heart_beat/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Asynchrounus Signals



%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


ping()-> 
    gen_server:call(?MODULE, {ping},infinity).

%%-----------------------------------------------------------------------
vm_status(Type)->
    gen_server:call(?MODULE, {vm_status,Type},infinity).
computer_status(Type)->
    gen_server:call(?MODULE, {computer_status,Type},infinity).

get_vm()->
    gen_server:call(?MODULE, {get_vm},infinity).
get_vm(Restrictions,HostIds)->
    gen_server:call(?MODULE, {get_vm,Restrictions,HostIds},infinity).
get_all_vms()->
    gen_server:call(?MODULE, {get_all_vms},infinity).
    

    
start_node(IpAddr,Port,VmId) ->
    gen_server:call(?MODULE, {start_node,IpAddr,Port,VmId},infinity).
stop_node(Vm) ->
    gen_server:call(?MODULE, {stop_node,Vm},infinity).
active()->
    gen_server:call(?MODULE, {active},infinity).
passive()->
    gen_server:call(?MODULE, {passive},infinity).
all()->
    gen_server:call(?MODULE, {all},infinity).

log()->
    gen_server:call(?MODULE, {log},infinity).
%%----------------------------------------------------------------------

heart_beat({Interval,ComputerStatus,VmStatus})->
    gen_server:cast(?MODULE, {heart_beat,{Interval,ComputerStatus,VmStatus}}).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------

% To be removed
-define(TEXTFILE,"./test_src/dbase_init.hrl").

init([]) ->
%    ssh:start(),
%    ok=application:start(dbase_service),
    % To be removed
%    dbase_service:load_textfile(?TEXTFILE),
 %   timer:sleep(1000),

    spawn(fun()->h_beat(?HbInterval) end),
    {ok, #state{computer_running=[],
		computer_available=[],
		computer_not_available=[],
		vms_running=[],
		vms_available=[],
		vms_not_available=[],
		vm_candidates=[]}}.
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------
handle_call({ping},_From,State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};


handle_call({get_all_vms},_From,State) ->
    Reply=State#state.vm_candidates,
    {reply, Reply, State};

handle_call({get_vm},_From,State) ->
    Reply=case State#state.vm_candidates of
	      []->
		  NewState=State,
		  {error,[no_vms_running]};
	      [{HostId,VmId}|T]->
		  NewState=State#state{vm_candidates=lists:append(T,[{HostId,VmId}])},
		  {ok,{HostId,VmId,list_to_atom(VmId++"@"++HostId)}}
	  end,
    	  
    {reply, Reply, NewState};
handle_call({get_vm,Restriction,HostIds},_From,State) ->
    
    Reply=case Restriction of
	      not_from->
		  case [{XHostId,XVmId}||{XHostId,XVmId}<-State#state.vm_candidates,
					 false==lists:member(XHostId,HostIds)] of
		      []->		
			  NewState=State,	  
			  {error,[no_vms_running]};
		      [{YHostId,YVmId}|_]->
			  X=lists:delete({YHostId,YVmId},State#state.vm_candidates),
			  NewState=State#state{vm_candidates=lists:append(X,[{YHostId,YVmId}])},
			  {ok,{YHostId,YVmId,list_to_atom(YVmId++"@"++YHostId)}}
		  end;
	      from ->
		  case [{XHostId,XVmId}||{XHostId,XVmId}<-State#state.vm_candidates,
					 true==lists:member(XHostId,HostIds)] of
		      []->		
			  NewState=State,	  
			  {error,[no_vms_running]};
		      [{YHostId,YVmId}|_]->
			  X=lists:delete({YHostId,YVmId},State#state.vm_candidates),
			  NewState=State#state{vm_candidates=lists:append(X,[{YHostId,YVmId}])},
			  {ok,{YHostId,YVmId,list_to_atom(YVmId++"@"++YHostId)}}
		  end;
	      Err->
		  NewState=State,
		  {error,[eexists,Err]}
	  end,  
    {reply, Reply, NewState};

handle_call({vm_status,Type},_From,State) ->
    Reply= case Type of
	       running->
		   State#state.vms_running;
	       available->
		   State#state.vms_available;
	       not_available->
		   State#state.vms_not_available;
	       Err->
		   {error,[edefined,Err]}
	   end,
    {reply,Reply,State};

handle_call({computer_status,Type},_From,State) ->
    Reply= case Type of
	       running->
		   State#state.computer_running;
	       available->
		   State#state.computer_available;
	       not_available->
		   State#state.computer_not_available;
	       Err->
		   {error,[edefined,Err]}
	   end,
    {reply,Reply,State};

handle_call({start_node,IpAddr,Port,VmId},_From,State) ->
    Reply={not_implemented,start_node,IpAddr,Port,VmId},
    {reply,Reply,State};

handle_call({stop_node,Vm},_From,State) ->
    Reply={not_implemented,stop_node,Vm},
    {reply,Reply,State};

handle_call({passive},_From,State) ->
    Reply={not_implemented,passive},
    {reply,Reply,State};

handle_call({active},_From,State) ->
    Reply={not_implemented,active},
    {reply,Reply,State};

handle_call({all},_From,State) ->
    Reply={not_implemented,all},
    {reply,Reply,State};

handle_call({log},_From,State) ->
    Reply={not_implemented,log},
    {reply,Reply,State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% -------------------------------------------------------------------
handle_cast({heart_beat,{Interval,ComputerStatus,VmStatus}}, State) ->
 %   io:format("ComputerStatus ~p~n",[{?MODULE,?LINE,time(),ComputerStatus}]),
 %   io:format("VmStatus ~p~n",[{?MODULE,?LINE,time(),VmStatus}]),
 %   io:format("h_beat ~p~n",[{time(),?MODULE,?LINE}]),
    RunningComputers=[HostId||{running,HostId}<-ComputerStatus],
    AvailableComputers=[HostId||{available,HostId}<-ComputerStatus],
    NotAvailableComputers=[HostId||{not_available,HostId}<-ComputerStatus],

    RunningVms=vms:vm_status(VmStatus,running),
    AvailableVms=vms:vm_status(VmStatus,available),
    NotAvailableVms=vms:vm_status(VmStatus,not_available),

    NewVmCandidates=vms:candidates(State#state.vm_candidates,RunningVms),
    NewState=State#state{computer_running=RunningComputers,
			 computer_available=AvailableComputers,
			 computer_not_available=NotAvailableComputers,
			 vms_running=RunningVms,
			 vms_available=AvailableVms,
			 vms_not_available=NotAvailableVms,
			 vm_candidates=NewVmCandidates},
 
    spawn(fun()->h_beat(Interval) end),    
    {noreply, NewState};
			     
handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
h_beat(Interval)->
    timer:sleep(Interval),
 %   io:format(" *************** "),
 %   io:format(" ~p",[{time()}]),
 %   io:format(" *************** ~n"),
    ComputerStatus=computer:status_computers(),
%    io:format("ComputerActualState ~p~n",[{?MODULE,?LINE,time(),ComputerStatus}]),
    AvailableComputers=[HostId||{available,HostId}<-ComputerStatus],
 %   io:format("AvailableComputers ~p~n",[{?MODULE,?LINE,AvailableComputers}]),
 
    [ControlVmId]=db_vm_id:read(controller),
%    io:format("ControlVmId ~p~n",[{?MODULE,?LINE,ControlVmId}]),
    WorkerVmIds=db_vm_id:read(worker),
%    io:format("WorkerVmIds ~p~n",[{?MODULE,?LINE,WorkerVmIds}]),

    _CleanComputers=[computer:clean_computer(HostId,ControlVmId)||HostId<-AvailableComputers],
%    io:format("CleanComputers ~p~n",[{?MODULE,?LINE,CleanComputers}]),

    _StartComputers=[computer:start_computer(HostId,ControlVmId)||HostId<-AvailableComputers],
 %   io:format("StartComputers ~p~n",[{?MODULE,?LINE,StartComputers}]),  

    _CleanVms=[computer:clean_vms(WorkerVmIds,HostId)||HostId<-AvailableComputers],
  %  io:format("CleanVms ~p~n",[{?MODULE,?LINE,CleanVms }]),

    _StartVms=[computer:start_vms(WorkerVmIds,HostId)||HostId<-AvailableComputers],
 %   io:format("StartVms ~p~n",[{?MODULE,?LINE,StartVms}]),
  
    RunningComputers=[HostId||{running,HostId}<-ComputerStatus],
    VmStatus=[vms:status_vms(HostId,WorkerVmIds)||HostId<-RunningComputers],
 %   io:format("VmStatus ~p~n",[{?MODULE,?LINE,VmStatus}]),

    _CleanVms2=[computer:clean_vms(VmIds,HostId)||{HostId,_,{available,VmIds},_}<-VmStatus],
%    io:format("CleanVms2 ~p~n",[{?MODULE,?LINE,CleanVms2 }]),

    _StartVms2=[computer:start_vms(VmIds,HostId)||{HostId,_,{available,VmIds},_}<-VmStatus],
%    io:format("StartVms2 ~p~n",[{?MODULE,?LINE,StartVms2}]),
    
    
    ComputerStatus2=computer:status_computers(),
    VmStatus2=[vms:status_vms(HostId,WorkerVmIds)||HostId<-RunningComputers],


    rpc:cast(node(),?MODULE,heart_beat,[{Interval,ComputerStatus2,VmStatus2}]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
