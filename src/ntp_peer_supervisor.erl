-module(ntp_peer_supervisor).
-behaviour(supervisor).

-author("Nolan Robidoux").

-export([start_link/0, start_link/1, init/1]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
start_link() -> start_link([]).

start_link([]) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, { #{strategy => simple_one_for_one}, [

        % === Peer/Poll process in RFC reference implementation
		#{	id => ntp_peer,
			start => {ntp_peer, start_link, []},
            restart => temporary
            %% shutdown => n	   %Default: 5000 | infinity
			%% type => worker,	   %DEFAULT
			%% modules => [...]	   %Default: [M]	
		}
	]}}.

%sup_flags() = #{strategy => strategy(),        % optional (one_for_one)
%              intensity => non_neg_integer(),  % optional (1) 
%              period => pos_integer()}         % optional (5)