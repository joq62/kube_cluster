%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%%  c
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(oam_iaas). 



-export([vm_status/1,
	 computer_status/1
	]).


-define(DbaseVmId,"10250").
-define(ControlVmId,"10250").
-define(TimeOut,3000).

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
vm_status(Status)->
    L=if_db:vm_status(Status),
    R=[{HostId,VmId}||{_Vm,HostId,VmId,_Type,_XStatus}<-L],
    R.
% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
computer_status(Status)->
    L=if_db:computer_status(Status),
    R=[HostId||{HostId,_XStatus}<-L],
    
    R.
	
	
