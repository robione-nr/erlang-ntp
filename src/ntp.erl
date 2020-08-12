-module(ntp).
-behaviour(application).

-author("Nolan Robidoux").

-compile(inline).

-export([start/0, start/2, stop/1]).

-export([get_time/0, get_time/1, get_offset/0, get_offset/1]).
-export([add_peer/1, peer_info/1, drop_peer/1]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% echo pw | sudo -S cmd
%% iptables / ip6tables
%% sudo iptables -t nat -I OUTPUT -p tcp -d 127.0.0.1 --dport 123 -j REDIRECT --to-ports 3000
%% sudo iptables -t nat -I PREROUTING -p tcp --dport 123 -j REDIRECT --to-ports 3000
%% sudo ip6tables -t nat -I OUTPUT -p tcp -d ::1 --dport 123 -j REDIRECT --to-ports 3000
%% sudo ip6tables -t nat -I PREROUTING -p tcp --dport 123 -j REDIRECT --to-ports 3000


start() -> start(normal, []).

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
        _:_ -> {error, system_offline}
    end;
get_time(_) ->
    {error, badarg}.


get_offset() -> get_offset(nanosecond).

get_offset(Unit) when is_atom(Unit)->
    try
        {Offset, Precision} = gen_server:call(ntp_sysproc, get_offset),
        convert(Offset + Precision, Unit)
    catch
        _:_ -> {error, system_offline}
    end;
get_offset(_) ->
    {error, badarg}.


add_peer(HostName) ->
    case Ret = inet:gethostbyname(HostName) of
        {hostent, _, _, _, _, IPaddrs} ->
               lists:foreach(fun(IP) ->
                                {ok, _} = supervisor:start_child(ntp_peer_supervisor, [IP]),
                                gen_server:cast(ntp_sysproc, {add_peer, {HostName, IP}})
                                end
                            , IPaddrs);
        {error, _} -> Ret
    end.

drop_peer(HostName) ->
    gen_server:cast(ntp_sysproc, {drop_peer, HostName}).

peer_info(HostName) ->
    P = gen_server:call(ntp_sysproc, {peer_pid, HostName}),
    {_, Ret} = gen_server:call(P, {get_vars, all}),
    Ret.

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