-module(mnesia_key_cache_server).

-author('@voluntas').
-author('@itawasa').

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_RETRY, 3).

-record(state, {keys :: [],
                table :: atom()}).  

%% erl +P 500000 -env ERL_MAX_ETS_TABLES 500000 -env ERL_MAX_PORTS 100000
  
-spec random_key() -> not_found | binary().
random_key() ->
  gen_server:call(?MODULE, random_key).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, #state{keys = keys()}}.

handle_call(random_key, _From, State) ->
  case State#state.keys of
    [] ->
      case keys() of
        [] ->
          {reply, not_found, State#state{keys = []}};
        [Key|Keys] ->
          {reply, Key, State#state{keys = Keys}}
      end;
    [Key] ->
      {reply, Key, State#state{keys = keys()}};
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

-spec keys() -> list().
keys() ->
  %% テーブルサイズを事前に調べてそもそも 0 だったら何もしないってのがあるといいのか？
  mnesia:activity(async_dirty, fun mnesia:all_keys/1, [?STORE_TABLE], mnesia_frag).

