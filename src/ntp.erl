-module(ntp).
-behaviour(gen_server).

-include("../include/ntp.hrl").

-compile(export_all).
-compile(inline).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([get_time/0, get_time/1, get_offset/0, get_offset/1]).

get_time() -> get_time(nanosecond).

get_time(Unit) when is_atom(Unit) ->
    Before = erlang:monotonic_time(nanosecond),

    {Time, Precision} = gen_server:call(?MODULE, get_time),
    After = erlang:monotonic_time(nanosecond),

    convert(Time + (Precision * 3) + (After - Before), Unit);
get_time(_) ->
    {error, badarg}.

get_offset() -> get_offset(nanosecond).

get_offset(Unit) when is_atom(Unit)->
    {Time, Precision} = gen_server:call(?MODULE, get_offset),
    convert(Time + Precision, Unit);
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
    application:start(crypto),

    RefID = proplists:get_value(refId, OptList, 
                                refid(util:ip_to_binary(util:get_ipv4()))),
    Precision =  find_precision(proplists:get_value(sample_clock, OptList, ?CLOCK_SAMPLES)),

    %% MAC = md5 | OTP < 22 cmac/aes_128cbc | OTP >= 22 mac/{cmac,aes_128_cbc}
    %% use apply/3

    {ok}.

%% handle_call/3
%% ====================================================================
handle_call(get_offset, _, State) ->
    {reply, ok, State};
handle_call(get_time, _, State) ->
    {reply, ok, State};
handle_call(_,_,State) ->
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
    case Unit of
		nanosecond ->  Time; 
		microsecond -> Time div 1000; 
		millisecond -> Time div 1000000; 
		second ->      Time div 1000000000;
		seconds ->     Time div 1000000000;
		_ ->	{error, badarg}
	end.


-define(RANGE(A,B,C),(A >= B andalso A =< C)).

%% Determine the node ID

refid(<<_:4/binary>> = IP) -> IP;
refid(<<_:16/binary>> = IP) ->
     <<Return:4/binary, _:12/binary>> = crypto:hash(md5, IP),
    Return.


%% Function(s) to determine precision of local clock

find_precision(N) ->
    [H|T] = sample_clock(N, []),

    {_, Diff} = lists:foldl(fun(E, {P, In}) -> {E, [P-E | In]} end, {H, []}, T),
    Mean = mathx:mean(mathx:cull(Diff,2)),
    
    bitshift_prec(?MAX_LONG, 0, Mean).


bitshift_prec(Error, SHL, Mean) ->
    NewErr = abs(1000000000/(1 bsl SHL) - Mean),
    if
        NewErr =< Error ->
            bitshift_prec(NewErr, SHL + 1, Mean);
        true ->
            -SHL
    end.


sample_clock(0, Out) -> Out;
sample_clock(N, In) ->
    sample_clock(N-1, [erlang:monotonic_time() | In]).

%% Test Zone

poll_update(N) ->
    Poll = max(min(?POLL_MAX,N),?POLL_MIN).

%% === "Kernel"
%% Erlang: Do nothing (store offset if different beforehand)
%% adjust_time(offset)
%% set_time(offset)

%%Sockets: recv_packet / xmit_packet: do the task

gettime() -> 
    Time = (erlang:system_time() + ?OFFSET_1900) / 1000000000,
    
    High = floor(Time),
    Low = round((Time - High) * ?MAX_LONG),

    io:format("~p ~p ~p~n",[High, Time, Low]),

    <<High:32/integer-unsigned, Low:32/integer-unsigned>>.

