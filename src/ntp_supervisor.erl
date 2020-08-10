-author("Nolan Robidoux").

-module(ntp_supervisor).
-behaviour(supervisor).

-export([start_link/1, init/1]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start_link(OptList) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, OptList).

init(OptList) ->
	{ok, { #{strategy => one_for_one}, [

        % === System process in RFC reference implementation
		#{	id => ntp_sysproc,
			start => {ntp_sysproc, start_link, []}
            %% shutdown => n	    %Default: 5000 | infinity
			%% type => worker,		%DEFAULT
			%% modules => [...]		%Default: [M]	
		},
		
		% === Clock adjust and discipline process
    	#{	id => ntp_clkproc,
			start => {ntp_clkproc, start_link, []}
		 }
		
		% === Peer / Poll Supervisor
%		#{	id => ntp_peer_supervisor,
%			start => {ntp_peer_supervisor, start_link, []},
%			shutdown => supervisor
%		}
	]}}.

%sup_flags() = #{strategy => strategy(),        % optional (one_for_one)
%              intensity => non_neg_integer(),  % optional (1) 
%              period => pos_integer()}         % optional (5)