-module(rss_queue).

%% export from 5l ab
-include("logging.hrl").

-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

-behaviour(gen_server).

%% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).
%% start that server
-export([start/1]).
%% defining
-define(TIMEOUT, 10000).
-record(rssState, {queue, subscribers}).


init([]) ->
	process_flag(trap_exit, true),
	{ok, #rssState{queue=[], subscribers=sets:new()}};


init([Url]) ->
	RSS = #rssState{queue=[], subscribers=sets:new()},
	process_flag(trap_exit, true),
	rss_reader:start(Url, self()), {ok, RSS}.

%% @doc Функция запуска
%% Запускает очередь с заданным именем
start(Name) ->
	gen_server:start({local, Name}, ?MODULE, [], []).

%% @doc Функция запуска
%% Запускает очередь с заданным именем и URL-адресом
start(Name, Url) ->
	gen_server:start({local, Name}, ?MODULE, [Url], []).

%% @doc Добавление в очередь
%% Функция добавляет в очередь очередной элемент
add_item(QPid, Item) when is_pid(QPid) ->
	State = gen_server:cast(QPid, {add_item, Item}),
	State.

%% @doc Добавление Фида
add_feed(QPid, RSS2Feed) ->
	Feed = rss_parse:get_feed_items(RSS2Feed),
	lists:foreach(
		fun(Item) ->
			add_item(QPid, Item)
		end, Feed),
	?INFO("Collection length: ~p ~n", [length(Feed)]),
	ok.

%% @doc Получение элементов
%% Получает все эелемент фидов
get_all(QPid) when is_pid(QPid) ->
	gen_server:call(QPid, {get_all}).

%% @doc Передача новых элементов 
broadcast(Item, Subscribers) ->
	[add_item(SubscriberPid, Item) || SubscriberPid <- sets:to_list(Subscribers)].

%% Вспомогательные функции
%% @doc Регистрация новых элементов в очереди
push_item(RSSItem, Queue, Subscribers) ->
	{Res, OldItem} = search_item(RSSItem, Queue),
	case Res of
		same ->
			?INFO("`update_queue`: same_item ~n", []),
			Queue;
		updated ->
			?INFO("`update_queue`: updated_item ~n", []),
			broadcast(RSSItem, Subscribers),
			QueueUpdated = Queue--[OldItem],
			lists:sort(fun date_comporator/2, QueueUpdated++[RSSItem]);
		different ->
			?INFO("`update_queue`: different_item ~n", []),
			broadcast(RSSItem, Subscribers),
			lists:sort(fun date_comporator/2, Queue++[RSSItem])
	end.

%% @doc Поиск в листе элемента
search_item(RSSItem, []) -> {different, RSSItem};

%% @doc Поиск в листе элемента
search_item(RSSItem, RssQueue) -> [H | T] = RssQueue,
	case rss_parse:compare_feed_items(RSSItem, H) of
		same -> {same, H};
		updated -> {updated, H};
		different -> search_item(RSSItem, T)
	end.

%% @doc Функция для сравнения дат
date_comporator(A, B) ->
	rss_parse:get_item_time(A) < rss_parse:get_item_time(B).

%% @doc Обработка запроса
handle_call(_Request={subscribe, QPid}, _From, State=#rssState{queue=Queue1, subscribers=Subscribers1}) ->
	{Reply, NewState} = case sets:is_element(QPid, Subscribers1) of
		true ->
			{{error, already_subscribed}, State};
		false ->
			erlang:monitor(process,QPid),
			?INFO("`handle_call`: New subscriber ~p to ~p~n",[QPid, self()]),
			[add_item(QPid, Item) || Item <- Queue1],
			{ok, State#rssState{subscribers=sets:add_element(QPid, Subscribers1)}}
	end,
	{reply, Reply, NewState};

%% @doc Обработка запроса
handle_call(_Request={get_all}, _From, State=#rssState{queue=Queue1}) ->
	{reply, Queue1, State};

%% @doc Обработка запроса
handle_call(_Request, _From, State) ->
	{reply, {error, {unknown_request,_Request}}, State}.

%% @doc Обработка `cast`
handle_cast(_Msg={add_item, RSSItem=#xmlElement{name=item}}, State=#rssState{queue=Queue1, subscribers=Subscribers1}) ->
	NewQ = push_item(RSSItem, Queue1, Subscribers1),
	{noreply, State#rssState{queue=NewQ}};

%% @doc Обработка `cast` - для отписки
handle_cast(_Msg={unsubscribe, QPid}, State=#rssState{subscribers=Subscribers1}) -> { noreply, State#rssState{subscribers=sets:del_element(QPid, Subscribers1)} };

%% @doc Обработка `cast`
handle_cast(_Msg, State) -> ?WARN("Unknown msg {~p} to Q{~p}", [_Msg, State]), {noreply, State}.


%% @doc Обработка info
handle_info(_Info={'DOWN', _, _, QPid, _Reason}, State=#rssState{subscribers=Subscribers1}) ->
	{noreply, State#rssState{subscribers=sets:del_element(QPid, Subscribers1)}};

%% @doc Обработка info
handle_info(_Info={'EXIT', FromPid, _Reason}, State) ->
	?ERROR("Something went wront in ~p coz: ~n", [FromPid, _Reason]),
	{noreply, State};

%% @doc Обработка info
handle_info(_Info, State) -> {noreply, State}.

%% @doc Pre-terminated call
terminate(_Reason, _State) -> ok.

%% @doc Изменение кода
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @doc подписка
subscribe(From, To) ->
	gen_server:call(To, {subscribe, From}).

get_feed_xml(Name, Request) -> 
	QPid = whereis(Name),
	{ok, Feed} = rss_queue:get_all(QPid),
	RSSItem = lists:flatten(xmerl:export_content(Feed, xmerl_xml)),
	URL = io_lib:format("<link>~s</link>\n", [Request]),
	if 
		is_atom(Name) == true ->
			Title = io_lib:format("<title>~s</title>\n", [Name]),
			Desc = io_lib:format("<description>Aggregated feed queue from ~s</description>\n", [Name]);
		true ->
			Title = io_lib:format("<title>Unknown queue</title>\n", []),
			Desc = io_lib:format("<description>Aggregated feed queue</description>\n", [])
	end,
	RSSResponse = 
		["<?xml version=\"1.0\"?>\n"
			"<rss xmlns:media=\"http://search.yahoo.com/mrss/\""
			" xmlns:feedburner=\"http://rssnamespace.org/feedburner/ext/1.0\""
			" xmlns:digg=\"http://digg.com/docs/diggrss/\""
			" xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
			" version=\"2.0\">\n"
			"<channel>\n" ++
				Title ++
				Desc ++
				URL ++
				RSSItem ++
			"</channel>\n"
			"</rss>\n"
		],
	RSSResponse.