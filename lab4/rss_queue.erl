-module(rss_queue).

-import(xmerl_xpath, [string/2]).
-include ("../lab3/rss_parse.erl").

-compile(export_all).
-define(TIMEOUT, 10000).


server(Queue, Subs)->
	receive
		{add_item, RSSItem} ->
			NewQueue = queue_push(RSSItem, Queue, Subs),
			server(NewQueue, Subs);

		{get_all, ReqPid} ->
			ReqPid ! {self(), Queue},
			server(Queue,Subs);

		_Msg -> io:format("Unknown msg~p~n",[_Msg])	
	end.

start() ->
	Queue = [],
	spawn(?MODULE, server, [Queue, sets:new()]).

init([]) -> start().

add_item(QPid,Item)->
	QPid ! {add_item,Item},
	ok.

queue_push(NewItem, L1, [], Subs)->
	broadcast(NewItem, Subs),
	L1++[NewItem];

queue_push(NewItem, L1, L = [OldItem|Rest], Subs)->
	case rss_parse:compare_feed_items(OldItem,NewItem) of
		same -> 
			L1++L ;
		updated -> 
			 broadcast(NewItem,Subs), L1++Rest++[NewItem] ;
		different -> 
			queue_push(NewItem,L1++[OldItem],Rest,Subs)
	end.
queue_push(NewItem,Queue,Subs)-> queue_push(NewItem,[],Queue,Subs).

add_feed(QPid, RSS2Feed)->
	Items = rss_parse:get_feed_items(RSS2Feed),
	[add_item(QPid,Item) || Item <- Items], 
	ok.

add_feed_from_file(QPid,File)->
	Items=rss_parse:get_feed_items_test(File),
	[add_item(QPid,Item) || Item <- Items], 
	ok.

get_all(QPid)->
	QPid ! {get_all, self()},
	receive
		{QPid, Queue} -> Queue;
		_Msg -> {error, unknown_msg, _Msg}
	after 
		? TIMEOUT -> {error, timeout}
	end.

broadcast(Item, PidSet) ->
	[add_item(Pid, Item) || Pid <- sets:to_list(PidSet)].