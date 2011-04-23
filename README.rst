######################
Mnesia Key Cache
######################

erl option::

  erl +P 500000 -env ERL_MAX_ETS_TABLES 500000 -env ERL_MAX_PORTS 100000

sample code::

  -module(sample).

  -define(TABLE, table).

  init() ->
    mnesia:create_table(?TABLE, ...),
    mnesia_key_cache:start(?TABLE).

  activity(Fun) ->
    mnesia_key_cache:activity(?TABLE, Fun).

  update() ->
    F = fun(Key) ->
          case mnesia:read(?TABLE, Key, read) of
            [] ->
              {error, retry};
            [Record] ->
              mnesia:write(?TABLE, Record, sticky_write)
          end
        end,
    case activity(F) of
      {ok, Result} ->
        ...
      {error, not_found} ->
        ...
    end.
