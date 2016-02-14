% A template for implementing gen_server callback modules.


-module(genserver_template).

% The server implements the gen_server behavior.
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% Additional helper functions exported by the callback module.
-export([start/1]).


% TODO:  Implement module helper functions.

start(Name) -> gen_server:start({local, Name}, ?MODULE, [], []).


% TODO:  Behavior callback functions.  Implement the appropriate callback
%        functions as necessary for your server.

init([]) -> {ok, todo_state}.

handle_call(_Request, _From, State) -> {reply, todo_reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


