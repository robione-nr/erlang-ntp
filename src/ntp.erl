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

add_peer(_) ->
    ok.

drop_peer(_) ->
    ok.

peer_info(_) ->
    ok.