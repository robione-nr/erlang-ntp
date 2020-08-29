-module(ntp_sysproc).
-behaviour(gen_server).

-include("../include/ntp.hrl").

-compile(export_all).
-compile(inline).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% ====================================================================
%% Exposed functions
%% ====================================================================
-export([get_vars/1, set_vars/1]).

get_vars(List) ->           gen_server:call(?MODULE, {get_vars, List}).
set_vars(List) ->           gen_server:call(?MODULE, {set_vars, List}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start_link(OptList) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, OptList, []).

%% ====================================================================
%% Framework Functions
%% ====================================================================

%% init/1
%% ====================================================================
init(OptList) when is_list(OptList) ->
    ntp_util:set_vars([t, precision, rtdelay, 
                rtdisp, reftime, offset, jitter, flags, n]),

    ntp_util:set_vars([{leap, ?INIT_LEAP}, {stratum, ?INIT_STRATUM},
                {poll, ?DEFAULT_POLL}]),

    put(precision, find_precision(proplists:get_value(sample_clock, OptList, ?DEFAULT_SAMPLES))),

    put(refid, 
        case (V = proplists:get_value(ref_id, OptList)) of
            ipv4 -> refid(iface:to_binary(iface:get_ipv4()));
            ipv6 -> refid(iface:to_binary(iface:get_ipv6()));
            {_,_,_,_} -> refid(iface:to_binary(V));
            {_,_,_,_,_,_,_,_} -> refid(iface:to_binary(V));
            undefined -> refid(iface:to_binary(iface:get_ipv4()));
            _ -> refid(iface:to_binary(iface:get_ipv4()))
        end
    ),

    %% TODO: Intersection betweens DETS servers and list
    %% Record: (Idea) host, mode
    Servers = proplists:get_value(servers, OptList, ?NTP_SERVER_LIST),
    lists:foreach(fun ntp:add_peer/1, Servers),

    %% m[] = [{int type, dbl, edge}]
    %% f[] = [dbl metric]
    
    Mode = proplists:get_value(mode, OptList, ?DEFAULT_MODE),
    Password = proplists:get_value(sudo, OptList),

    RunLevel = 
        case {Password, Mode} of 
            {undefined, Mode} when Mode =/= client ->
                logger:log(notice, "No sudo password supplied. Cannot access port 123.\nRunning as client-only."),
                client;
            _ -> Mode
        end,
    
    Port = proplists:get_value(port, OptList, ?FW_PORT),

    

    put(sudo, Password),
%   ip_routing(Port, insert_rules),
    
    {ok,{#{},Port}}.

%% handle_call/3
%% ====================================================================
handle_call({get_vars, V},_,State) ->   {reply, ntp_util:get_vars(V), State};
handle_call({set_vars, V},_,State) ->   ntp_util:set_vars(V), {noreply, State};

handle_call(get_time,_,State) ->        {reply, get(t), State};
handle_call(get_offset,_,State) ->      {reply, get(offset), State};

handle_call({add_peer, IP}, _, {Map , Port}) when is_tuple(IP) ->
    {ok, Pid} = supervisor:start_child(ntp_peer_supervisor, [IP]),
    monitor(process, Pid),

    Out = Map#{
        Pid => IP,
        IP => Pid
    },
    {reply, Pid, {Out, Port}};

handle_call({add_peer, IPAddrs}, _, {Map , Port}) when is_list(IPAddrs) ->
    {IP, Pid} =
        lists:foldl(fun(IP, {I,P}) ->
            {ok, Pid} = supervisor:start_child(ntp_peer_supervisor, [IP]),
            monitor(process, Pid),
            {[{IP, Pid} | I],[{Pid, IP} | P]}
        end, {[],[]}, IPAddrs),

    Out = maps:merge(
                Map,
                maps:merge(
                    maps:from_list(IP),
                    maps:from_list(Pid)
                )),

    {reply, proplists:get_keys(Pid), {Out, Port}};

handle_call(_,_,State) ->
    {reply, {error, badarg}, State}.


%% handle_cast/2
%% ====================================================================
handle_cast({drop_peer, Pid}, {Map , Port}) ->
    demonitor(Pid),
    IP = maps:get(Pid, Map, undefined),
    exit(Pid, shutdown),
    {noreply, {maps:without([Pid, IP], Map), Port}};

handle_cast(_, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
handle_info({'DOWN', _, process, Pid, _}, {Map, Port}) ->
    IP = maps:get(Pid, Map, undefined),
    exit(Pid, shutdown),
    {noreply, {maps:without([Pid, IP], Map), Port}};

handle_info(state, State) ->
    io:format("~p~n",[State]),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
terminate(_, {_, Port}) ->
%    ip_routing(Port, delete_rules),
    ok.


%% Placeholders
%% ====================================================================
code_change(_, State, _) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% Init/End helpers

ip_routing(Port, Mode) ->
    StrPort = integer_to_list(Port),
    Password = get(sudo),

    Action = case Mode of
        insert_rules -> 
            os:cmd(lists:flatten(["echo \"" | [Password | 
                ["\" | sudo -S ip6tables -t nat -I OUTPUT -p udp -d ::1 --dport 123 -j REDIRECT --to-ports " | StrPort]]])),
            os:cmd(lists:flatten(["echo \"" | [Password | 
                ["\" | sudo -S ip6tables -t nat -I PREROUTING -p udp --dport 123 -j REDIRECT --to-ports " | StrPort]]]));
        delete_rules ->
            os:cmd(lists:flatten(["echo \"" | [Password | 
                ["\" | sudo -S ip6tables -t nat -D OUTPUT -p udp -d ::1 --dport 123 -j REDIRECT --to-ports " | StrPort]]])),
            os:cmd(lists:flatten(["echo \"" | [Password | 
                ["\" | sudo -S ip6tables -t nat -D PREROUTING -p udp --dport 123 -j REDIRECT --to-ports " | StrPort]]]))
    end.
    


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
    Poll = max(min(?MAX_POLL,N),?MIN_POLL).

%% === "Kernel"
%% Erlang: Do nothing (store offset if different beforehand)
%% adjust_time(offset)
%% set_time(offset)

%%Sockets: recv_packet / xmit_packet: do the task

mobilize(src,dst,ver,mode,keyid,flags) ->
    %% zeroed ; otherwise values; poll = POLL_MIN?? or default?
    ok.

find_assoc(RxPacket) ->
    %% Find peer by Rx->IP and Rx->mode
    ok.

rstclock(State, Offset, T) ->
    %%c.state = state; c.last,offset = offset; s.t = t
    ok.

gettime() -> %%ntp_time() :)
    Time = (erlang:system_time() + ?OFFSET_1900) / 1000000000,
    
    High = floor(Time),
    Low = round((Time - High) * ?MAX_LONG),

    <<High:32/integer-unsigned, Low:32/integer-unsigned>>.


%% PACKETS===========
%% 
%% R = { IPSrc, IPDst, NTP_hdr, 3ts, keyid, mac, ts}
%% X = { IPSrc, IPDst, NTP_hdr, 3ts, keyid, mac}

%% ASSOCIATION ==============
%% F /*Filter*/ = {t, fOffset, fDelay, fDisp} //t = ts
%% 
%% P /*peer*/ = {
%%      /configs on init
%%      IPSrc. IPDst, version, hmode (host), keyid, flags 
%%      /set by RxPacket
%%      ntp_hdr
%%      3ts /begin clear block ***********
%%      /computed
%%      t, f[n], ofset, delay, disp, jitter
%%      /poll data
%%      hpoll, burst, reach, ttl
%%      unreach /end clear block  ***********
%%      outdate, nextdate / lastor next poll times; latter Erlang timer ref

%%================================================
%%               NOTES ---- TODO
%%================================================
%%initialize system structure
%%initialize clock structure
%%load persistent configs (assoc, freq file)
%%place map in ETS table
%%
%%mobilize starts peer process and clears appropriate data
%%      - peer process startup
%%
%%find_assoc -> during receive(), 
%%      - refers to sysproc to lookup essentially same as access()
%%
%%time conversion routines: ntp <-> native
%%
%%DSCRD exits, ERR closes sym_connect -> exits
%%      exits == in erlang??
%%
%%iface:is_bcast - get from getifaddrs
%%iface:is_multicast - ipv4 & 1110_ _ _ _
%%                  ipv6 & ff00 == ff00
%%================================================
%%================================================

%% System S = {
%%      t //timestamp
%%      ntp_hdr
%%      struct m //list of {pptr, 1|0|-1, float edge}   %%intersection
%%      struct v //list of {pptr, float metric}         %%cluster
%%      pptr
%%      float offset, jitter //combined
%%      flags, n               //options, survivors}

%% CLOCK ================
%%  C /*clock*/ = {
%%      t   /updated
%%      offset  /current
%%      last    /previous
%%      count   /jiggle count
%%      freq, jitter    /?,RMSjitter
%%      wander}     /RMS wander

%% INIT==========
%% S(0) -> init leap, strat; poll = min prec = find_prec
%% C(0) -> jitter = float val LOG2(s.prec)
%% mobilize assoc
%% while 0???? {recv_packet; r->dst = gettime(); receive(r)}


%% receive()!!!!!
%% 
%% is IP allowed? (opt) Proceed/Return
%% checks ->
%%      version > my version
%%      packet  length. mac length, ext lengths
%% has_mac ->
%%      0 ->auth = none
%%      4 -> auth = crypto_nak
%%      _ -> md5(r->keyid) ????  =:= r->mac
%% 
%% p = find_assoc(r)
%% lookup dispatch table based on p->hmode, r->mode
%%      FXMIT: return or fast_xmit(...)
%%      MANY: mobilize manycast client eph assoc
%%      NEWPS: New symmetric passive - fast_xmit / mobilize / return
%%      NEWBC: return / mobilize
%%      DSCD: return
%%      PROC: 
%%          Timestamp checks... auth checks...
%%          packet(p,r)

%% packet(p,r) !!!!!!!
%% 
%% Some stuff
%% poll_update(p,p->hpoll)
%% clock_filter(p,offset,delay,disp)

%% clock_filter(...)
%% 
%% Some stuff
%% clock_select()

%%clock_select() -> clock_update() -> {clock_combine() -> root_dist(), local_clock()}

%%CALCULUS TIME!!!! :D