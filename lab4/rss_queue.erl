-module(rss_queue).
% logger from 5 lab
-include("logging.hrl").

-compile(export_all).
-define(TIMEOUT, 10000).

init([]) ->
  start().

%% @doc Старт сервера
start() ->
  Queue = [],
  spawn(?MODULE, server, [Queue]).

%% @doc Основный цикл программы, который слушает события
%% {add_item, RSSItem} и {get_all, RegPid}
server(Queue) ->
  receive
    {add_item, RSSItem} ->
      UpdatedQueue = push_item(RSSItem, Queue),
      server(UpdatedQueue);
    {get_all, RegPid} ->
      RegPid ! {self(), Queue},
      server(Queue)
  end.

%% @doc Поиск в листе элемента
search_item(RSSItem, []) ->
  {different, RSSItem};

%% @doc Поиск в листе элемента
search_item(RSSItem, Queue) ->
  [Head | Tail] = Queue,
  case rss_parse:compare_feed_items(RSSItem, Head) of
    same -> {same, Head};
    updated -> {updated, Head};
    different -> search_item(RSSItem, Tail)
  end.

%% @doc Добавление RSS-элемента очередь
push_item(RSSItem, Queue) ->
  {State, FoundItem} = search_item(RSSItem, Queue),
  case State of
    same ->
      ?INFO("PUSHED: same item ~p ~n",[self()]),
      Queue;
    updated ->
      QueueUpdated = Queue--[FoundItem],
      ?INFO("PUSHED: updated item ~p ~n",[self()]),
      lists:sort(fun date_comporator/2, QueueUpdated++[RSSItem]);
    different ->
      ?INFO("PUSHED: different item ~p ~n",[self()]),
      lists:sort(fun date_comporator/2, Queue++[RSSItem])
  end.

%% @doc Функция для сравнения дат
date_comporator(A, B) ->
  rss_parse:get_item_time(A) < rss_parse:get_item_time(B).

%% @doc Добавление элемента
add_item(QPid, Item)
    when is_pid(QPid) ->
      QPid ! {add_item, Item},
      ok.

%% @doc Добавление Фида
add_feed(QPid, RSS2Feed) ->
  Items = rss_parse:get_feed_items(RSS2Feed),
  lists:foreach(fun(Item) ->
                    add_item(QPid, Item)
                end, Items),
  ok.

%% @doc Получения RSS
get_all(QPid) when is_pid(QPid) ->
  QPid ! {get_all, self()},
  receive 
    {QPid, List} ->
      ?INFO("Count:~p in PID=~p ~n",[length(List),QPid]),
      List
  after ?TIMEOUT ->
    {error, timeout}
  end.