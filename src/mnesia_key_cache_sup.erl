-module(mnesia_key_cache_sup).

-author('voluntas').

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [?CHILD(mnesia_key_cache_srv, worker)]} }.

