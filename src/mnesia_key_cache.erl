-module(mnesia_key_cache).

-author('voluntas').

-export([activity/1]).

-define(DEFAULT_TRANSACTION_RETRY, 16).
-define(DEFAULT_RETRY, 32).

-spec activity(fun()) -> ok | {ok, term()} | {error, not_found} | {error, giveup}.
activity(F) ->
  activity0(lists:seq(0, 32), F).

-spec activity0(list(), fun()) -> ok | {ok, term()} | {error, not_found} | {error, giveup}.
activity0([], F) ->
  {error, giveup};
activity0([H|T], F) ->
  case random_key() of
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

-ifdef(TEST).
-endif.
