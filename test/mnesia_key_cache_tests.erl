-module(mnesia_key_cache_tests).

-include_lib("eunit/include/eunit.hrl").

-define(STORE_TABLE, store_table).

-record(store, {key :: binary(),
                value0 :: binary(),
                value1 :: binary()}).

activity_test_() ->
  {setup,
    fun() ->
      mnesia:start(),
      application:start(mnesia_key_cache),
      mnesia:create_table(?STORE_TABLE,
                          [{frag_properties,
                            [{node_pool, [node()]},
                             {n_fragments, 128}]},
                           {record_name, store},
                           {attributes, record_info(fields, store)}]),
      F = fun(_) ->
            mnesia:activity(async_dirty,
                            fun mnesia:write/3,
                            [?STORE_TABLE,
                             #store{key    = crypto:rand_bytes(32),
                                    value0 = crypto:rand_bytes(32),
                                    value1 = crypto:rand_bytes(32)},
                             sticky_write],
                            mnesia_frag)
          end,
      lists:foreach(F, lists:duplicate(500, dummy)),
      mnesia_key_cache:start(?STORE_TABLE),
      lists:foreach(F, lists:duplicate(500, dummy)),
      ok
    end,
    fun(_) ->
      mnesia:stop(),
      application:stop(mnesia_key_cache),
      ok
    end,
    ?_test(
      begin
        F = fun(Key) ->
              case mnesia:read(?STORE_TABLE, Key, write) of
                [] ->
                  {error, retry};
                [_] ->
                  mnesia:delete(?STORE_TABLE, Key, write),
                  ok
              end
            end,
        lists:foreach(fun(_) ->
                        mnesia_key_cache:activity(?STORE_TABLE, F)
                      end,
                      lists:duplicate(1000, dummy)),
        ?assertEqual({error, not_found}, mnesia_key_cache:activity(?STORE_TABLE, F))
      end
    )
  }.
