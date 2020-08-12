-module(ntp_socket).
-behaviour(gen_server).

-author("Nolan Robidoux").

-include("../include/ntp.hrl").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
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

start_link() ->
	gen_server:start_link(?MODULE, [], []).

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

    <<Packet:48/binary, ExtMAC/binary>> = Data,

    %% byte_size of 20,22,>22, (Good) <8, Sz%4 (Bad)
    
    <<Leap:2/integer-unsigned, Version:3/integer-unsigned,
      Mode:3/integer-unsigned, Stratum:8/integer-unsigned,
      Poll:8/integer-unsigned, Precision:8/integer-unsigned,
      RootDelay:4/binary, RootDisp:4/binary, RefId:4/binary,
      RefTS:8/binary, OriginTS:8/binary, RxTS:8/binary, TxTS:8/binary,
      Tail/binary>> = Packet,

    

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