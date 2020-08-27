-module(ntp).
-behaviour(application).

-author("Nolan Robidoux").

-compile(inline).

-export([start/0, start/1, start/2, stop/1]).

-export([get_time/0, get_time/1, get_offset/0, get_offset/1]).
-export([add_peer/1, peer_info/1, drop_peer/1, enum_peers/0]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start() -> start(normal, []).

start(OptList) -> start(normal, OptList).

start(normal, OptList) ->
    ntp_supervisor:start_link(OptList).

stop(_) -> ok.

%% ====================================================================
%% API functions
%% ====================================================================

get_time() -> get_time(nanosecond).

get_time(Unit) when is_atom(Unit) ->
    try
        Before = erlang:monotonic_time(nanosecond),

        {Time, Precision} = gen_server:call(ntp_sysproc, get_time),
        After = erlang:monotonic_time(nanosecond),

        convert(Time + (Precision * 3) + (After - Before), Unit)
    catch
        _:_ -> {error, timeout}
    end;
get_time(_) ->
    {error, badarg}.


get_offset() -> get_offset(nanosecond).

get_offset(Unit) when is_atom(Unit)->
    try
        {Offset, Precision} = gen_server:call(ntp_sysproc, get_offset),
        convert(Offset + Precision, Unit)
    catch
        _:_ -> {error, timeout}
    end;
get_offset(_) ->
    {error, badarg}.


add_peer({_,_,_,_} = IP) -> gen_server:call(ntp_sysproc, {add_peer, IP});
add_peer({_,_,_,_,_,_,_,_} = IP) -> gen_server:call(ntp_sysproc, {add_peer, IP});
add_peer(Host) ->
    %% gethostbyname takes care of errors for return
    case inet:gethostbyname(Host) of
        {ok, {_,_,_,_,_,IPAddrs}} ->
            gen_server:call(ntp_sysproc, {add_peer, IPAddrs});
        Ret -> Ret
    end.

drop_peer(Pid) ->
    gen_server:cast(ntp_sysproc, {drop_peer, Pid}).

peer_info(Pid) ->
    try
        {_, Ret} = gen_server:call(Pid, {get_vars, all}),
        Ret
    catch
        _:_ -> {error, enotconn}
    end.

enum_peers() ->
    Peers = supervisor:which_children(ntp_peer_supervisor),
    lists:foldl(fun(E, Acc) ->
            {_, Pid, _, _} = E,
            [Pid | Acc]
        end, [], Peers).


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