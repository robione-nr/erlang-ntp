-module(ntp_peer).
-behaviour(gen_server).

-author("Nolan Robidoux").

-include("../include/ntp.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3
]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_vars/2, set_vars/2, halt/1, halt/2, clear/1, clear/2]).

get_vars(Peer, Vars) ->       gen_server:call(Peer, {get_vars, Vars}).
set_vars(Peer, Vars) ->       gen_server:call(Peer, {set_vars, Vars}).

halt(Peer) -> Peer ! {halt, normal}.
halt(Peer, Reason) -> Peer ! {halt, Reason}.

%% TODO: "dates" here probably best implemented as erlang timer refs send_after|interval
clear(Peer) -> clear(Peer, <<"INIT">>).

clear(Peer, KISS) ->
    set_vars(Peer, [org,rec,xmt,t,offset,delay,disp,jitter,hpoll,burst,reach,ttl]),
    set_vars(Peer, [{leap, ?INIT_LEAP},{stratum,?INIT_STRATUM},{ppoll,?MAX_POLL},{hpoll,?DEFAULT_POLL}]),

    %% Peer Filters

    Time = ntp_clkproc:get_vars([t]),
    set_vars(Peer,[{t,Time},{outdate,Time}]),
    set_vars(Peer,[{nextdate, Time + (ntp_util:random() band (?log2(?DEFAULT_POLL)-1))}]),

    ok.

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start_link(IP) ->
	gen_server:start_link(?MODULE, IP, []).

%% ====================================================================
%% Framework Functions
%% ====================================================================

%% init/1
%% ====================================================================
init(_) ->
    %% Probably set from init param
    ntp_util:set_vars([saddr,daddr,version, hmode, keyid, flags]),
    ntp_util:set_vars([pmode,rtdelay,rtdisp,refid,reftime]),
    %% TODO: Peer Filters init
    ntp_util:set_vars({leap, ?INIT_LEAP},{stratum,?INIT_STRATUM},{ppoll,?MAX_POLL},{hpoll,?DEFAULT_POLL}),
    %% Clear block (mostly)
    ntp_util:set_vars([org,rec,xmt,t,offset,delay,disp,jitter,burst,reach,ttl]),

    %% Probably erlang timers maintained in server state -unreach
    ntp_util:set_vars([unreach,outdate,nextdate]),
    
    {ok,[]}.

%% handle_call/3
%% ====================================================================
handle_call({get_vars, V},_,State) ->   {reply, ntp_util:get_vars(V), State};
handle_call({set_vars, V},_,State) ->   ntp_util:set_vars(V), {noreply, State}.


%% handle_cast/2
%% ====================================================================
handle_cast(_, []) ->
    {noreply, []}.


%% handle_info/2
%% ====================================================================
handle_info({halt, Reason}, []) ->
    {stop, Reason, []}.

%% Placeholders
%% ====================================================================
terminate(_, _) ->
    io:format("Child terminating ~p~n",[self()]),
    ok.

code_change(_, [], _) ->
    {ok, []}.