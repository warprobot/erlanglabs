-module(rss_queue).
% logger from 5 lab
-include("logging.hrl").

-compile(export_all).
-define(TIMEOUT, 1000).

init([]) ->
  start();

init([Url]) ->
  start(Url).


%% @doc Старт сервера
start() ->
  Queue = [],
  Subscribers = sets:new(),
  spawn(?MODULE, server, [Queue, Subscribers]).

start(Url)->
  QPid = start(),
  rss_reader:start(Url,QPid),
  QPid.

%% @doc Основный цикл программы, который слушает события
%% {add_item, RSSItem} и {get_all, RegPid}
server(Queue, Subscribers) ->
  receive
    {add_item, RSSItem} ->
      UpdatedQueue = push_item(RSSItem, Queue, Subscribers),
      server(UpdatedQueue, Subscribers);
    {get_all, RegPid} ->
      RegPid ! {self(), Queue},
      server(Queue, Subscribers);
    {unsubscribe, QPid} ->
      server(Queue, sets:del_element(QPid,Subscribers));
    {subscribe, QPid} ->
      [add_item(QPid,Item) || Item <- Queue],
      server(Queue, sets:add_element(QPid,Subscribers));
    {'DOWN',_ , _, QPid, _} ->
      server(Queue, sets:del_element(QPid,Subscribers))
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

%% @doc Широковещательное сообщение
broadcast(Item, PidSet)->
  [ add_item(Pid,Item) || Pid <- sets:to_list(PidSet) ].

%% @doc Добавление RSS-элемента очередь
push_item(RSSItem, Queue, Subscribers) ->
  {State, FoundItem} = search_item(RSSItem, Queue),
  case State of
    same -> Queue;
    updated ->
      QueueUpdated = Queue--[FoundItem],
      broadcast(FoundItem, Subscribers),
      lists:sort(fun date_comporator/2, QueueUpdated++[RSSItem]);
    different ->
      broadcast(FoundItem, Subscribers),
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
    {QPid, List} -> List
  after ?TIMEOUT ->
    {error, timeout}
  end.
