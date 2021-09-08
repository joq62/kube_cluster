-module(db_lock).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-define(LockTimeOut, 15). %% 30 sec 

-define(TABLE,lock).
-define(RECORD,lock).
-record(lock,
	{
	 lock_id,
	 time,
	 leader
	}).
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create({?MODULE,LockId}) ->
    create(LockId,0).
create(LockId,Time) ->
    F = fun() ->
		Record=#?RECORD{
				lock_id=LockId,
				time=Time,
				leader=node()
			       },		
		mnesia:write(Record) end,
    mnesia:transaction(F).

add_node(Node,StorageType)->
    Result=case mnesia:change_config(extra_db_nodes, [Node]) of
	       {ok,[Node]}->
		   mnesia:add_table_copy(schema, node(),StorageType),
		   mnesia:add_table_copy(?TABLE, node(), StorageType),
		   Tables=mnesia:system_info(tables),
		   mnesia:wait_for_tables(Tables,20*1000);
	       Reason ->
		   Reason
	   end,
    Result.

read_all_info() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{LockId,Time,Leader}||{?RECORD,LockId,Time,Leader}<-Z].

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [LockId||{?RECORD,LockId,_Time,_Leader}<-Z].

	


read(Object) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.lock_id==Object])),
    [{YLockId,Time,Leader}||{?RECORD,YLockId,Time,Leader}<-Z].

leader(Object)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.lock_id==Object])),
    [Leader||{?RECORD,_LockId,_Time,Leader}<-Z].
    
is_leader(Object,Node)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.lock_id==Object,
		     X#?RECORD.leader==Node])),
    Result=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Result.
    
is_open(Object)->
    is_open(Object,?LockTimeOut).
is_open(Object,LockTimeOut)->
    F=fun()->
	      case mnesia:read({?TABLE,Object}) of
		  []->
		      mnesia:abort({error,[eexists,Object,?FUNCTION_NAME,?MODULE,?LINE]});
		  [LockInfo] ->
		      CurrentTime=erlang:system_time(seconds),
		      LockTime=LockInfo#?RECORD.time,
		      TimeDiff=CurrentTime-LockTime,
		      if
			  TimeDiff > LockTimeOut->
			      LockInfo1=LockInfo#?RECORD{time=CurrentTime,leader=node()},
			      mnesia:write(LockInfo1);
			  TimeDiff == LockTimeOut->
			      LockInfo1=LockInfo#?RECORD{time=CurrentTime,leader=node()},
			      mnesia:write(LockInfo1);
			  TimeDiff < LockTimeOut->
			       mnesia:abort(Object)
		      end
	      end
      end,
    IsOpen=case mnesia:transaction(F) of
	       {atomic,ok}->
		   true;
	       _->
		   false
	   end,
    IsOpen.
		      
	      
delete(Object) ->

    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,Object}),
			    X#?RECORD.lock_id==Object],
		case RecordList of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1) 
		end
	end,
    mnesia:transaction(F).


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
