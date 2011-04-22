-module(mnesia_key_cache).

-author('voluntas').

-export([activity/2]).

-export([name/1]).

-define(DEFAULT_TRANSACTION_RETRY, 16).
-define(DEFAULT_RETRY, 32).

start() ->
  application:start(mnesia_key_cache).

start(Table) ->
  supervisor:start_child(mnesia_key_cache_sup, [Table]).

stop(Table) ->
  supervisor:terminate_child(name(Table)),
  supervisor:delete_child(name(Table)),
  ok.

-spec activity(atom(), fun()) -> ok | {ok, term()} | {error, not_found} | {error, giveup}.
activity(Table, F) ->
  activity0(lists:seq(0, 32), Table, F).

-spec activity0(list(), fun()) -> ok | {ok, term()} | {error, not_found} | {error, giveup}.
activity0([], Table, _F) ->
  {error, giveup};
activity0([H|T], Table, F) ->
  case mnesia_key_cache_server:maybe_key(Table) of
    not_found ->
      {error, not_found};
    Key ->
      case catch mnesia:activity({transaction, 16}, F, [Key], mnesia_frag) of
        {'EXIT',{aborted, _Reason}} ->
          activity0(T, F);
        ok ->
          ok;
        {ok, Result} ->
          {ok, Result};
        {error, retry} ->
          activity0(T, F)
      end
  end.

-spec name(atom()) -> atom().
name(Table) ->
  list_to_atom(io_lib:format("~s_~s", [Table, ?MNESIA_KEY_CACHE_SRV_PREFIX])).

-ifdef(TEST).
-endif.
