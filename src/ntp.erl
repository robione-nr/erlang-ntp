-module(ntp).
-behaviour(gen_server).

-compile({inline,[get_time/0, get_time/1, get_offset/0, get_offset/1]}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([get_time/0, get_time/1, get_offset/0, get_offset/1]).

get_time() -> get_time(nanosecond).

get_time(Unit) when is_atom(Unit) ->
    gen_server:call(?MODULE, {get_time, Unit});
get_time(_) ->
    {error, badarg}.

get_offset() -> get_offset(nanosecond).

get_offset(Unit) when is_atom(Unit)->
    gen_server:call(?MODULE, {get_offset, Unit});
get_offset(_) ->
    {error, badarg}.

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start_link() ->
	start_link([]).

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% ====================================================================
%% Framework Functions
%% ====================================================================

%% init/1
%% ====================================================================
init(OptList) when is_list(OptList) ->
    {ok}.

%% handle_call/3
%% ====================================================================
handle_call({get_offset, Unit}, _, State) ->
    {reply, convert(Time, Unit), State};
handle_call({get_time, Unit}, _, State) ->
    {reply, convert(Time, Unit), State};
handle_call({_,_,State}) ->
    {reply, {error, badarg}, State}.


%% handle_cast/2
%% ====================================================================
handle_cast(_, State) ->
    {noreply, State}.


%% Placeholders
%% ====================================================================
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

convert(Time, Unit) ->
    case Time of
		nanosecond -> (I * 100 -?OFFSET_1582); 
		microsecond -> (I * 100 -?OFFSET_1582) div 1000; 
		millisecond -> (I * 100 -?OFFSET_1582) div 1000000; 
		second ->  (I * 100 -?OFFSET_1582) div 1000000000;
		seconds ->  (I * 100 -?OFFSET_1582) div 1000000000;
		_ ->	{error, badarg}
	end;