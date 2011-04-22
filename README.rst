######################
Mnesia Key Cache
######################

erl::

  erl +P 500000 -env ERL_MAX_ETS_TABLES 500000 -env ERL_MAX_PORTS 100000

sample code::

  Table = ...
  mnesia:create_table(Table, ...),
  mnesia_key_cache:start(Table),
  F = fun(Key) ->
        case mnesia:read(Table, Key, read) of
          [] ->
            {error, retry};
          [Record] ->
            mnesia:write(Table, Record, sticky_write)
        end
      end,
  case mnesia_key_cache:activity(Table, F) of
    {ok, Result} ->
      ...
    {error, not_found} ->
      ...
  end,
  ...
