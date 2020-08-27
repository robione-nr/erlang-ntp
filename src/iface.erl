-module(iface).
-author("Nolan Robidoux").
-timestamp("2020/08/07 19:38 UTC").

-export([to_binary/1, enum/0]).
-export([get_ipv4/0, get_ipv4/1, get_ipv6/0, get_ipv6/1]).
-export([get_mac/0, get_mac/1]).

%% ====================================================================
%% API functions
%% ====================================================================

to_binary({A,B,C,D}) ->
    <<A, B, C, D>>;
to_binary({A,B,C,D,E,F,G,H}) ->
    <<A:16/integer-unsigned, B:16/integer-unsigned, C:16/integer-unsigned, D:16/integer-unsigned,
      E:16/integer-unsigned, F:16/integer-unsigned, G:16/integer-unsigned, H:16/integer-unsigned>>;
to_binary(MAC) when is_list(MAC)->
    list_to_binary(MAC).


get_ipv4() -> get_ip(4,[]).
get_ipv4(IFace) -> get_ip(4, IFace).

get_ipv6() -> get_ip(8,[]).
get_ipv6(IFace) -> get_ip(8, IFace).


get_mac() -> get_mac([]).

get_mac([]) ->
    {ok, List} = inet:getifaddrs(),
	
    hd([MAC || {_, Opts} <- List, {hwaddr, MAC} <- Opts,
					{addr, _} <- Opts, {flags, Flags} <- Opts,
					lists:member(loopback,Flags) =/= true]
				);
get_mac(IFace) when is_list(IFace) ->
    {ok, List} = inet:getifaddrs(),
    hd([MAC || {NIC, Opts} <- List, {hwaddr, MAC} <- Opts, NIC =:= IFace]);
get_mac(N) when is_integer(N) ->
    {ok, List} = inet:getifaddrs(),

    try
        {_,Opts} = lists:nth(N,List),
        proplists:get_value(hwaddr,Opts)
    catch
        _:_ -> {error, out_of_bounds}
    end;
get_mac(_) ->
    {error, badarg}.



enum() ->
    {ok, List} = inet:getifaddrs(),
    lists:foldr(
        fun({ID, _}, {N, Tail}) -> 
            {N+1, [ID | Tail]} end,
        {0,[]}, List).

%% ====================================================================
%% Internal functions
%% ====================================================================

get_ip(Sz, IFace) ->
    {ok, List} = inet:getifaddrs(),
    
    try
        case IFace of
         [] -> 
                hd([IP || {_, Opts} <- List, {addr, IP} <- Opts,
                        tuple_size(IP) =:= Sz, {flags, Flags} <- Opts,
			    		lists:member(loopback,Flags) =/= true]
    				);
            _ ->
                hd([IP || {ID, Opts} <- List, {addr, IP} <- Opts, ID =:= IFace, tuple_size(IP) =:= Sz])
        end
    catch
        _:_ -> {error, bad_iface}
    end.
