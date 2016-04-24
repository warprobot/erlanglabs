-module(rss_queue).

-import(xmerl_xpath, [string/2]).
% -include("../lab3/rss_parse.erl").

-compile(export_all).
-define(TIMEOUT, 10000).

init([]) ->
  start().

start() ->
  Queue = [],
  spawn(?MODULE, server, [Queue]).

server(Queue) ->
  receive
    {add_item, RSSItem} ->
      UpdatedQueue = push_item(RSSItem, Queue),
      server(UpdatedQueue);
    {get_all, RegPid} ->
      RegPid ! {self(), Queue},
      server(Queue)
  end.

search_item(RSSItem, []) ->
  {different, RSSItem};

search_item(RSSItem, Queue) ->
  [Head | Tail] = Queue,
  case rss_parse:compare_feed_items(RSSItem, Head) of
    same -> {same, Head};
    updated -> {updated, Head};
    different -> search_item(RSSItem, Tail)
  end.

push_item(RSSItem, Queue) ->
  {State, FoundItem} = search_item(RSSItem, Queue),
  case State of
    same -> Queue;
    updated ->
      Queue = Queue--[FoundItem],
      lists:sort(fun date_comporator/2, Queue++[RSSItem]);
    different ->
      lists:sort(fun date_comporator/2, Queue++[RSSItem])
  end.

date_comporator(A, B) ->
  rss_parse:get_item_time(A) < rss_parse:get_item_time(B).

add_item(QPid, Item)
    when is_pid(QPid) ->
      QPid ! {add_item, Item},
      ok.

add_feed(QPid, RSS2Feed) ->
  case rss_parse:is_rss2_feed(RSS2Feed) of
    true ->
      Items = rss_parse:get_feed_items(RSS2Feed),
      lists:foreach(fun(Item) ->
                        add_item(QPid, Item)
                    end, Items),
      ok;
    false ->
      not_ok
  end.

get_all(QPid) when is_pid(QPid) ->
  QPid ! {get_all, self()},
  receive 
    {ok, List} -> List
  after ?TIMEOUT ->
    {error, timeout}
  end.
