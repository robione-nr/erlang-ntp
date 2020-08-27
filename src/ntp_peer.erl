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
-export([get_vars/2, set_vars/2, halt/1, halt/2]).

get_vars(Peer, Vars) ->
    gen_server:call(Peer, {get_vars, Vars}).

set_vars(Peer, Vars) ->
    gen_server:cast(Peer, {set_vars, Vars}).

halt(Peer) -> Peer ! {halt, normal}.
halt(Peer, Reason) -> Peer ! {halt, Reason}.

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
init(IP) ->

    put(pid,self()),

    {ok,[]}.

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
handle_info({halt, Reason}, []) ->
    {stop, Reason, []}.

%% Placeholders
%% ====================================================================
terminate(_, _) ->
    io:format("Child terminating ~p~n",[self()]),
    ok.

code_change(_, [], _) ->
    {ok, []}.