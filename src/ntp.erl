-author("Nolan Robidoux").

-module(ntp).
-behaviour(application).

-compile(inline).

-export([start/0, start/2, stop/1]).
-export([get_time/0, get_offset/0]).
-export([add_peer/1, peer_info/1, drop_peer/1]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start() -> start(normal, []).

start(normal, OptList) ->
    ntp_supervisor:start_link(OptList).

stop(_) -> ok.

%% ====================================================================
%% API functions
%% ====================================================================

get_time() ->
    ok.

get_offset() ->
    ok.

add_peer(HostName) ->
    case Ret = inet:gethostbyname(HostName) of
        {hostent, _, _, Family, _, IPaddrs} ->
               lists:foreach(fun(IP) ->
                                {ok, _} = supervisor:start_child(ntp_peer_supervisor, [IP, Family])
                                end
                            , IPaddrs);
        {error, _} -> Ret
    end.

drop_peer(HostName) ->
    gen_server:cast(ntp_sysproc, {drop_peer, HostName}).

peer_info(HostName) ->
    P = gen_server:call(ntp_sysproc, {peer_info, HostName}),
    {_, Ret} = gen_server:call(P, {get_vars, all}),
    Ret.