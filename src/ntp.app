{application, ntp, [
	{description,"Erlang-based NTP Server"},
	{vsn,"0.0.3"},

	%List of modules added by this application
	{modules, [ntp, ntp_supervisor, ntp_sysproc, ntp_peer_supervisor,
               ntp_peer, ntp_socket, ntp_clkproc]},

	%List of registered atoms by modules in application
	{registered, []},

	%List of {K,V} that can serve as configuration info
	{env, []},

	{maxT, infinity},	%Explicit declaration of default time to live.

	%Applications to which this is dependent on
	{applications, [kernel, stdlib, crypto]},

	{mod, {ntp,[]}}		%Callback module for application
]}.
