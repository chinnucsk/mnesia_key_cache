-module(mnesia_key_cache_srv).

-author('voluntas').
-author('shino').

-behaviour(gen_server).

-export([maybe_key/1]).

-export([start_link/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {keys :: []}).  


-spec maybe_key(atom()) -> not_found | binary().
maybe_key(Table) ->
  gen_server:call(mnesia_key_cache:name(Table), {maybe_key, Table}).

start_link(Table) ->
  gen_server:start_link({local, mnesia_key_cache:name(Table)}, ?MODULE, [Table], []).

init([Table]) ->
  {ok, #state{keys = maybe_keys(Table)}}.

handle_call({maybe_key, Table}, _From, State) ->
  case State#state.keys of
    [] ->
      case maybe_keys(Table) of
        [] ->
          {reply, not_found, State#state{keys = []}};
        [Key|Keys] ->
          {reply, Key, State#state{keys = Keys}}
      end;
    [Key] ->
      {reply, Key, State#state{keys = maybe_keys(Table)}};
    [Key|Keys] ->
      {reply, Key, State#state{keys = Keys}}
  end;
handle_call(_Request, _From, State) ->
  {noreply, State}.  

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
                                                                                
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



-spec maybe_keys(atom()) -> list().
maybe_keys(Table) ->
  mnesia:activity(async_dirty, fun mnesia:all_keys/1, [Table], mnesia_frag).


