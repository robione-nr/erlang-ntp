-module(ntp_socket).
-behaviour(gen_server).

-author("Nolan Robidoux").

-include("../include/ntp.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_vars/2, set_vars/2]).

get_vars(Peer, Vars) ->
    gen_server:call(Peer, {get_vars, Vars}).

set_vars(Peer, Vars) ->
    gen_server:cast(Peer, {set_vars, Vars}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start_link(Port) ->
	gen_server:start_link(?MODULE, Port, []).

%% ====================================================================
%% Framework Functions
%% ====================================================================

%% init/1
%% ====================================================================
init(Port) ->
    case Ret = gen_udp:open(Port, [inet6, binary, {active,3}, {ipv6_v6only, false}]) of
        {ok, _} -> Ret;
        {error, _} -> {stop, Ret}
    end.

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


%% handle_info/2
%% ====================================================================
handle_info({udp, _, IP, _, Data}, Socket) ->
    inet:setopts(Socket, [{active,1}]),

    try
        case ntp_sysproc:access(IP) of
            true -> 
                case byte_size(Data) >= 48 of
                    true -> continue;
                    false -> throw({error, malformed_packet})
                end;
            false ->
                throw({error, eaccess})
        end,
        
        <<Packet:48/binary, ExtMAC/binary>> = Data,

    %% byte_size of 20,22,>22, (Good) <8, Sz%4 (Bad)
    
        <<Leap:2/integer-unsigned, Version:3/integer-unsigned,
          Mode:3/integer-unsigned, Stratum:8/integer-unsigned,
          Poll:8/integer-unsigned, Precision:8/integer-unsigned,
          RootDelay:4/binary, RootDisp:4/binary, RefId:4/binary,
          RefTS:8/binary, OriTS:8/binary, RxTS:8/binary, TxTS:8/binary>> = Packet,

        if
            (Version >= 3) and (Version =< 4) -> continue;
            true -> throw({error, bad_version})
        end,

        MapExtMAC = parse_packet(ExtMAC, #{}, #{}),

        
    catch
        throw:{Type, Reason} -> 
            StrOut = case Reason of
                eaccess -> io_lib:format("Unsolicated packet from ~p.",[IP]);
                bad_version -> "Incompatible NTP version.";
                malformed_packet -> io_lib:format("Malformed packet received from ~p.",[IP])
            end,

            logger:log(Type, StrOut);

        Type:Reason ->
            logger:info("Unhandled exception of type (~p), reason (~p).",[Type,Reason])
    end,
    {noreply, Socket}.


%% terminate/2
%% ====================================================================
terminate(_, Socket) ->
    gen_udp:close(Socket),
    ok.

%% Placeholders
%% ====================================================================
code_change(_, [], _) ->
    {ok, []}.


%% ====================================================================
%% Internal Functions
%% ====================================================================

parse_packet(<<>>, _, AccOut) -> AccOut;
parse_packet(ExtMAC, State, AccIn) ->
    Sz = byte_size(ExtMAC),

    {AccOut, Tail} = 
    if
        Sz =:= 28 ->
            case ExtMAC of
                %% Section 3.2.4 of RFC 7821 recommends the NTP layer ignore
                %% the UDP Checksum Complement field. Handled at UDP layer.
                <<16#2005:16/integer-unsigned, 28:16/integer-unsigned,
                  0:176, CheckSum:16/integer-unsigned>> ->
                      {AccIn#{udp_checksum => CheckSum},<<>>};
                _ ->
                    throw({error, malformed_packet})
            end;
        (Sz =:= 22) or (Sz =:=20) ->
            <<KeyID:4/binary, Digest/binary>> = ExtMAC,
            {AccIn#{keyid => KeyID, digest => Digest}, <<>>};
        Sz > 22 ->
            ;
        (Sz < 8) or ((Sz band 3) =/= 0) ->
            throw({error, malformed_packet});
        true -> throw({error, malformed_packet})
    end,
    parse_packet(Tail, State, AccOut).


