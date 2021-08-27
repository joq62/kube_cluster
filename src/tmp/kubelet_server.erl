%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point 
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(kubelet_server).  
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube_logger.hrl").
-include_lib("kernel/include/file.hrl").
%% --------------------------------------------------------------------
-define(LogDir,"logs").
-define(Latest,filename:join(?LogDir,"latest.log")).

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state, {cluster_id,
		monitor_node,
		pods}).



%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

-export([log_to_file/1]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================


%%-----------------------------------------------------------------------

%%----------------------------------------------------------------------


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
    {ok,_}=monitor:start(),
    {ok,ClusterId}=application:get_env(cluster_id),
    {ok,MonitorNode}=application:get_env(monitor_node),
    kubelet_lib:init_dbase(),
    ?PrintLog(log,"Successful starting of server",[?MODULE]),
    ?PrintLog(debug,"Cookie",[erlang:get_cookie(),node(),?FUNCTION_NAME,?MODULE,?LINE]),
    {ok, #state{cluster_id=ClusterId,
		monitor_node=MonitorNode,
		pods=[]}}.
    
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
handle_call({add_monitor,Node},_From,State) ->
    {reply,ok, State#state{monitor_node=Node}};

handle_call({get_state},_From,State) ->
    Reply=State,
    {reply, Reply, State};

handle_call({get_pods},_From,State) ->
    Reply=State#state.pods,
    {reply, Reply, State};

handle_call({create_pod,PodId},_From,State) ->
    Reply=case db_pod_spec:read(PodId) of
	      []->
		  ?PrintLog(ticket,"pod eexist",[PodId]),
		  NewState=State;
	      [PodSpec]->
		  ClusterId=State#state.cluster_id,
		  MonitorNode=State#state.monitor_node,
		  case rpc:call(node(),pod,load_start,[ClusterId,MonitorNode,PodSpec]) of
		      ok->
			  ?PrintLog(log,"pod started",[PodId]),
			  PodList=State#state.pods,
			  NewPods=[PodId|PodList],
			  NewState=State#state{pods=NewPods},
			  ok;
		      Reason->
			  ?PrintLog(ticket,"pod couldnt be started",[PodId,Reason]),
			  NewState=State
		  end
	  end,
    {reply, Reply, NewState};

handle_call({delete_pod,PodId},_From,State) ->
    Reply=case db_pod_spec:read(PodId) of
	      []->
		  ?PrintLog(ticket,"pod eexist",[PodId]),
		  NewState=State;
	      [PodSpec]->
		  case lists:keymember(PodId,1,State#state.pods) of
		      true->
			  ClusterId=State#state.cluster_id,
			  ?PrintLog(log,"pod deleted",[PodId]),
			  PodsList=lists:keydelete(PodId,1,State#state.pods),
			  NewState=State#state{pods=PodsList},
			  rpc:call(node(),pod,stop_unload,[ClusterId,PodSpec]);
		      false->
			  NewState=State,
			  ?PrintLog(ticket,"pod eexist",[PodId]),
			  {error,[eexists,PodId]}
		  end
	  end,
    {reply, Reply,NewState};


handle_call({create_pod,PodName,Spec},_From,State) ->
    PodId=rpc:call(node(),pod,create,[PodName,Spec]),
    Reply=PodId,
    {reply, Reply, State};



handle_call({add_pod_spec,PodName,Spec},_From,State) ->
    Reply=glurk,
    {reply, Reply, State};
handle_call({delete_pod_spec,PodName},_From,State) ->
    Reply=glurk,
    {reply, Reply, State};


handle_call({start_slaves,HostId,SlaveNames,ErlCmd},_From,State) ->
    Master=list_to_atom("master"++"@"++HostId),
    Reply=rpc:call(node(),cluster_lib,start_slaves,[Master,HostId,SlaveNames,ErlCmd],2*5000),
    {reply, Reply, State};


handle_call({read_config},_From,State) ->
    Reply=glurk,
    {reply, Reply, State};

handle_call({load_config},_From,State) ->
    Reply=glurk,
    {reply, Reply, State};


handle_call({install},_From,State) ->
    Reply=rpc:call(node(),cluster_lib,install,[],2*5000),
    {reply, Reply, State};


handle_call({start_app,ApplicationStr,Application,CloneCmd,Dir,Vm},_From,State) ->
    Reply=cluster_lib:start_app(ApplicationStr,Application,CloneCmd,Dir,Vm),
    {reply, Reply, State};
handle_call({stop_app,ApplicationStr,Application,Dir,Vm},_From,State) ->
    Reply=cluster_lib:stop_app(ApplicationStr,Application,Dir,Vm),
    {reply, Reply, State};
handle_call({app_status,Vm,Application},_From,State) ->
    Reply=cluster_lib:app_status(Vm,Application),
    {reply, Reply, State};

handle_call({ping},_From,State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

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
handle_cast({print,{Date,Time,Node,Type,Msg,InfoList}}, State) ->
  io:format("~s: ~w, , ~w, ~s, ~p, ~n",
	    [misc_fun:date_time(Date,Time),Node,Type,Msg,InfoList]),
    {noreply, State};

handle_cast({print_type,Type}, State) ->
    logger_print:print_type(Type,?Latest),
    {noreply, State};

handle_cast({log_msg,Info}, State) ->
 %  io:format("Info ~p~n",[Info]),
    case State#state.monitor_node of
	node_defined->
	    ok;
	Node->
	    rpc:cast(Node,monitor,print,[Info]),
	    rpc:cast(Node,kubelet_server,log_to_file,[Info])
    end,
    
%    io:format("Info ~w~n",[Info]),
    {noreply, State};
    
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

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


log_to_file(Info)->
    case filelib:is_dir(?LogDir) of
	false->
	    file:make_dir(?LogDir),
	    write_info(Info);
	true ->
	    write_info(Info)
    end,
    ok.


write_info({Date,Time,Node,Type,Msg,InfoList})->
    LogFile=?Latest,
    case file:read_file_info(LogFile) of
	{error,_Reason}->
	    ok;
	{ok,FileInfo}->
	    if
		5*1000*1000<FileInfo#file_info.size->
%		1*1000<FileInfo#file_info.size->
		    F1=integer_to_list(erlang:system_time(millisecond)),
		    F2=F1++".log",
		    FileName=filename:join(?LogDir,F2),
		    file:rename(LogFile,FileName),
	      % max three log files;
		    {ok,FileNames}=file:list_dir(?LogDir),
		    Num=lists:foldl(fun(_X,Num)->Num+1 end, 0,FileNames),
		    io:format("FileNames ~p~n",[{Num,FileNames}]),
		    if 
			3<Num->
			    remove_oldest_log(FileNames); 
			true->
			    ok
		    end;
		true->
		    ok
	    end
    end,
    {ok,S}=file:open(LogFile,[append]),
	io:format(S,"~p.~n",
		  [{Date,Time,Node,Type,Msg,InfoList}]),
    file:close(S).

remove_oldest_log(FileNames)->
    [File1|T]=[filename:join(?LogDir,FileName)||FileName<-FileNames],
    {ok,FileInfo}=file:read_file_info(File1),
    FileToDelete=oldest(T,File1,FileInfo),
    file:delete(FileToDelete).

oldest([],OldestFile,_)->
    OldestFile;
oldest([File|T],OldestFile,OldestInfo)->
    {ok,FileInfo}=file:read_file_info(File),
    if 
	FileInfo#file_info.mtime<OldestInfo#file_info.mtime->
	    NewOldest=File,
	    NewOldestInfo=FileInfo;
	true ->
	    NewOldest=OldestFile,
	    NewOldestInfo=OldestInfo
    end,
    oldest(T,NewOldest,NewOldestInfo). 
