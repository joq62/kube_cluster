%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created : 
%%% Pod is an erlang vm and 1-n erlang applications (containers) 
%%% Pods network id is the node name  
%%% The pod lives as long as the applications is living 
%%% In each pod there is a mnesias dbase
%%% Pod template {apiVersion, kind, metadata,[{namen,striang}]
%%%               spec,[{containers,[{name,},{image,busybox},
%%%                     {command,['erl cmd]},restart policy]
%%% storage in Pod
%%% File System:
%%%  
%%% -------------------------------------------------------------------
-module(kube_logger_server).   
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").

%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state, {log_files_dir,
		log_file
	       }).



%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------
-define(LogDirName,"logs").
-define(LogFileName,"latest.log").


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------



-export([nice/1]).

% OaM related


%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================


%%---------------------------------------------------------------





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
%   {ok,ClusterIdAtom}=application:get_env(cluster_id),
 %   ClusterId=atom_to_list(ClusterIdAtom),
    ClusterId=rpc:call(node(),db_cluster_info,cluster,[],5*1000),
    LogFilesDir=filename:join([ClusterId,?LogDirName]),
    LogFile=filename:join([ClusterId,?LogDirName,?LogFileName]),
    {ok,_}=monitor:start(),
    {ok, #state{log_files_dir=LogFilesDir,
		log_file=LogFile}}.
    
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
handle_cast({print_type,Type}, State) ->
    logger_print:print_type(Type,State#state.log_file),
    {noreply, State};

handle_cast({log_msg,Info}, State) ->
 %  io:format("Info ~p~n",[Info]),
    Monitor=rpc:call(node(),db_cluster_info,monitor,[],5*1000),
    rpc:cast(Monitor,monitor,print,[Info]),
    
    LogFilesDir=State#state.log_files_dir,
    LogFile=State#state.log_file,
    log_to_file(Info,LogFilesDir,LogFile),
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
log_to_file(Info,LogFilesDir,LogFile)->
    Result=case filelib:is_dir(LogFilesDir) of
	       false->
		   case file:make_dir(LogFilesDir) of
		       {error,Reason}->
			   {error,Reason};
		       ok->
			   write_info(Info,LogFilesDir,LogFile),
			   ok
		   end;
	       true ->
		   
		   write_info(Info,LogFilesDir,LogFile),		   
		   ok
	   end,
    
    Result.


write_info({Date,Time,Node,Type,Msg,InfoList},LogFilesDir,LogFile)->
    case file:read_file_info(LogFile) of
	{error,_Reason}->
	    ok;
	{ok,FileInfo}->
	    if
		5*1000*1000<FileInfo#file_info.size->
%		1*1000<FileInfo#file_info.size->
		    F1=integer_to_list(erlang:system_time(millisecond)),
		    F2=F1++".log",
		    FileName=filename:join(LogFilesDir,F2),
		    file:rename(LogFile,FileName),
	      % max three log files;
		    {ok,FileNames}=file:list_dir(LogFilesDir),
		    Num=lists:foldl(fun(_X,Num)->Num+1 end, 0,FileNames),
		    io:format("FileNames ~p~n",[{Num,FileNames}]),
		    if 
			3<Num->
			    remove_oldest_log(FileNames,LogFilesDir); 
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

remove_oldest_log(FileNames,LogFilesDir)->
    [File1|T]=[filename:join(LogFilesDir,FileName)||FileName<-FileNames],
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

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
nice({Severity,{Y2,M,D},{H,Min,S},
      Node,Application,Module,Fun,Line,Info})->
    Y=Y2-2000,
    {Y1,M1,D1}={integer_to_list(Y),integer_to_list(M),integer_to_list(D)},
    {H1,Min1,S1}={integer_to_list(H),integer_to_list(Min),integer_to_list(S)}, 
    DateTime=H1++":"++Min1++":"++S1++" "++D1++"/"++M1++"-"++Y1,
    Severity1=atom_to_list(Severity),
    Node1=atom_to_list(Node),
    App1=atom_to_list(Application),
    Module1=atom_to_list(Module),
    Fun1=atom_to_list(Fun),
    Line1=integer_to_list(Line),

    Msg=DateTime++" | "++Severity1++" | "++Node1++" | "++App1++"| "++Module1++"| "++Fun1++" | "++Line1++" | "++Info,
    io:format("~p~n",[Msg]), 
    io:format("~-16s ~-7s ~-25s ~-10s ~-15s ~-15s ~-4s ~s ~n",[DateTime,Severity1,Node1,App1,Module1,Fun1,Line1,Info]),
    ok.
 
