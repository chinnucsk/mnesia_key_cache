-module(mnesia_key_cache).

-author('voluntas').

%% -export([start/0, stop/0]).

-export([start/1, stop/1]).

-export([activity/2]).

-export([name/1]).

-define(DEFAULT_TRANSACTION_RETRY, 16).
-define(DEFAULT_RETRY, 16).

-define(MNESIA_KEY_CACHE_TABLE_PREFIX, mnesia_key_cache_table_prefix).

%% start() ->
%%   application:start(mnesia_key_cache).
%% 
%% stop() ->
%%   application:stop(mnesia_key_cache).

start(Table) ->
  {ok, _Pid} = supervisor:start_child(mnesia_key_cache_sup, [Table]).

stop(Table) ->
  supervisor:terminate_child(mnesia_key_cache_sup, name(Table)),
  supervisor:delete_child(mnesia_key_cache_sup, name(Table)).

-spec activity(atom(), fun()) -> ok | {ok, term()} | {error, not_found} | {error, giveup}.
activity(Table, F) ->
  activity0(lists:seq(0, 16), Table, F).

-spec activity0(list(), atom(), fun()) -> ok | {ok, term()} | {error, not_found} | {error, giveup}.
activity0([], _Table, _F) ->
  {error, giveup};
activity0([_H|T], Table, F) ->
  case mnesia_key_cache_srv:maybe_key(Table) of
    not_found ->
      {error, not_found};
    Key ->
      case catch mnesia:activity({transaction, 16}, F, [Key], mnesia_frag) of
        {'EXIT',{aborted, _Reason}} ->
          activity0(T, Table, F);
        ok ->
          ok;
        {ok, Result} ->
          {ok, Result};
        {error, retry} ->
          activity0(T, Table, F);
        {error, Reason} ->
          {error, Reason}
      end
  end.

-spec name(atom()) -> atom().
name(Table) ->
  list_to_atom(lists:flatten(io_lib:format("~s_~s", [Table, ?MNESIA_KEY_CACHE_TABLE_PREFIX]))).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

name_test() ->
  ?assertEqual(test_mnesia_key_cache_table_prefix, name(test)),
  ok.

-endif.
