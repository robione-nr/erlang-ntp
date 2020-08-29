-module(ntp_clkproc).
-behaviour(gen_server).

-author("Nolan Robidoux").

-include("../include/ntp.hrl").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3
]).

%% Valid States: init, file, spike, freq, unsync

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_vars/1, set_vars/1, rstclock/3]).

get_vars(List) ->           gen_server:call(?MODULE, {get_vars, List}).
set_vars(List) ->           gen_server:call(?MODULE, {set_vars, List}).

rstclock(State, Offset, Time) ->
    ntp_clkproc:set_vars([{state,  State}, {last, Offset}, {offset,Offset}]),
    ntp_sysproc:set_vars([{t, Time}]).

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
    ntp_util:set_vars([t, state, offset, last, count, freq, jitter, wander]),

    case dets:lookup(?DETS_TABLE, freq) of
        [] -> put(state, init);
        [{_, V, Expiry}] ->
            case erlang:monotonic_time() > Expiry of
                true ->     put(state, init);
                false ->    put(freq,V),
                            put(state, file)
            end
    end,

    put(jitter, ?log2_frac(ntp_sysproc:get_vars([precision]))),

    {ok,[]}.

%% handle_call/3
%% ====================================================================
handle_call({get_vars, V},_,State) ->   {reply, ntp_util:get_vars(V), State};
handle_call({set_vars, V},_,State) ->   ntp_util:set_vars(V), {noreply, State}.

%% handle_cast/2
%% ====================================================================
handle_cast(_, State) ->
    {noreply, State}.

%% handle_info/2
%% ====================================================================
handle_info(clock_adjust, []) ->
    erlang:send_after(1000, self(), clock_adjust),
    Time = get(t) + 1,
    put(t, Time),

    %% STUFF

    case Time rem 3600 of
        0 -> dets:insert(?DETS_TABLE, {freq, get(freq)});
        _ -> continue
    end,

    {noreply, []};

handle_info(_, []) ->
    {noreply, []}.

%% Placeholders
%% ====================================================================

terminate(_, _) ->
    ok.

code_change(_, [], _) ->
    {ok, []}.