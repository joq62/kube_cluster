-module(db_app_info).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,app_info).
-define(RECORD,app_info).
-record(app_info,{
		  app,
		  info
		 }).

% Start Special 

% End Special 
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

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

create(App,Info)->
    Record=#?RECORD{
		    app=App,
		    info=Info
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

member(Object)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.app==Object])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.

git(Object)->
    read(Object,git_path).
env(Object)->
    read(Object,env).
read(Object,Key)->
    Return=case read(Object) of
	       []->
		   {error,[eexist,Object,?FUNCTION_NAME,?MODULE,?LINE]};
	       [{Object,Info}] ->
		   case  Key of
		       git_path->
			   {git_path,GitPath}=lists:keyfind(git_path,1,Info),
			   GitPath;
		       env->
			   {env,Env}=lists:keyfind(env,1,Info),
			   Env;
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Return.

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{App,Info}||{?RECORD,App,Info}<-Z].

read(Object)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.app==Object])),
    [Info]=[{App,Info}||{?RECORD,App,Info}<-Z],
    Info.

delete(Object) ->
    F = fun() -> 
		mnesia:delete({?TABLE,Object})
		    
	end,
    mnesia:transaction(F).


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
