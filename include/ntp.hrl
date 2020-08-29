-define(MAX_SHORT, 65536).
-define(MAX_LONG, 4294967296).

-define(OFFSET_1900, 2208988800000000000).

-define(NTP_PORT, 123).
-define(FW_PORT, 23123).
-define(NTP_VERSION, 4).

-define(INIT_LEAP, 3).
-define(INIT_STRATUM, 16).

-define(MIN_STRATUM, 2).
-define(MIN_POLL, 4).

-define(MAX_STRATUM, 15).
-define(MAX_POLL, 17).
-define(MAX_DISP, 16).

-define(DEFAULT_POLL, 6).
-define(DEFAULT_MODE, client).
-define(DEFAULT_SAMPLES,50).
-define(DEFAULT_CONFIG,"ntp.config").

-define(DETS_TABLE, ntp_config).

-define(PHI, 15.0e-6).

-define(NTP_SERVER_LIST, ["cronos.cenam.mx","time.esa.int","ntp.nict.jp","atom.uhr.de",
                          "clock.isc.org","ntp.ix.ru","tick.usask.ca","time.nist.gov"]).

%% Change as needed
-define(MS_SERVERS,{{'$1','_','_','_','_'},[],['$1']}).

%% Function Macros =============================================
%% TODO: Determine if binary best output
%% Consider clamping on inputs to fractionalize
-define(log2_frac(V), 1.0 / (1 bsl -(V))).     %% General
-define(log2(V), 1 bsl V).

-define(frac_u16(V), trunc(V * 65536)).        %% NTP Short
-define(u16_frac(V), V / 65536).

-define(frac_u32(V), trunc(V * 4294967296)).   %% NTP Long
-define(u32_frac(V), V / 4294967296).