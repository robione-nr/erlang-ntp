-module(ntp_supervisor).
-behaviour(supervisor).

-author("Nolan Robidoux").

-include("../include/ntp.hrl").

-export([start_link/1, init/1]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start_link(OptList) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, OptList).

init(OptList) ->
	Conf = proplists:get_value(config, OptList, ?DEFAULT_CONFIG),

	case (R = dets:open_file(?DETS_TABLE,[{file, Conf}])) of
		{error, _} -> R;
		_ ->

			Port = proplists:get_value(port, OptList, ?NTP_PORT),

			{ok, { #{strategy => one_for_one}, [

	    	    % === System process in RFC reference implementation
				#{	id => ntp_sysproc,
					start => {ntp_sysproc, start_link, [OptList]}
				},
		
				% === Clock adjust and discipline process
		   		#{	id => ntp_clkproc,
					start => {ntp_clkproc, start_link, []}
				},

        		% === UDP socket process
		   		#{	id => ntp_socket,
					start => {ntp_socket, start_link, [Port]}
				},
		
				% === Peer / Poll Supervisor
				#{	id => ntp_peer_supervisor,
					start => {ntp_peer_supervisor, start_link, []},
					type => supervisor
				}
			]}}
	end.