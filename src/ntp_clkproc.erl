-module(ntp_clkproc).
-behaviour(gen_server).

-author("Nolan Robidoux").

-include("../include/ntp.hrl").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3
]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([get_vars/1, set_vars/1]).

get_vars(List) ->
    gen_server:call(?MODULE, {get_vars, List}).

set_vars(List) ->
    gen_server:cast(?MODULE, {set_vars, List}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Framework Functions
%% ====================================================================

%% init/1
%% ====================================================================
init([]) ->

    put(t,0),
    put(state, not_set),
    put(offset, 0.0),
    put(last, 0.0),
    put(count, 0),
    put(freq, 0.0),
    put(jitter, 0.0),
    put(wander, 0.0),

    {ok,[]}.

%% handle_call/3
%% ====================================================================
handle_call({get_vars, Vars},_,[]) ->
    Result = if
                Vars =:= all -> get();
                true ->
                    lists:foldr(fun(VN, In) ->
                        [get(VN) | In]
                        end
                    ,[],Vars)
            end,
    {reply, {ok, Result}, []}.


%% handle_cast/2
%% ====================================================================
handle_cast({set_vars, Vars}, []) ->
    lists:foldl(fun({K,V}, _) ->
                    put(K,V)
                    end
                , [], Vars),
    {noreply, []}.


%% Placeholders
%% ====================================================================
handle_info(clock_adjust, []) ->
    erlang:send_after(1000, self(), clock_adjust),

    {noreply, []};
handle_info(_, []) ->
    {noreply, []}.

terminate(_, _) ->
    ok.

code_change(_, [], _) ->
    {ok, []}.