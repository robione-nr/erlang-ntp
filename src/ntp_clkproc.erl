-author("Nolan Robidoux").

-module(ntp_clkproc).
-behaviour(gen_server).

-include("../include/ntp.hrl").

-export([start_link/0, start_link/1]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start_link() ->
	start_link([]).

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% ====================================================================
%% Framework Functions
%% ====================================================================

%% init/1
%% ====================================================================
init(OptList) when is_list(OptList) ->
    application:start(crypto),

    RefID = proplists:get_value(refId, OptList, 
                                refid(util:ip_to_binary(util:get_ipv4()))),
    Precision =  find_precision(proplists:get_value(sample_clock, OptList, ?CLOCK_SAMPLES)),

    %% MAC = md5 | OTP < 22 cmac/aes_128cbc | OTP >= 22 mac/{cmac,aes_128_cbc}
    %% use apply/3

    %% 

    {ok}.

%% handle_call/3
%% ====================================================================
handle_call(get_offset, _, State) ->
    {reply, ok, State};
handle_call(get_time, _, State) ->
    {reply, ok, State};
handle_call(_,_,State) ->
    {reply, {error, badarg}, State}.


%% handle_cast/2
%% ====================================================================
handle_cast(_, State) ->
    {noreply, State}.


%% Placeholders
%% ====================================================================
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.