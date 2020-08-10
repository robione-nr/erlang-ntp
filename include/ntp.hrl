-define(MAX_SHORT, 65536).
-define(MAX_LONG, 4294967296).

-define(OFFSET_1900, 2208988800000000000).

-define(NTP_PORT, 123).
-define(NTP_VERSION, 4).

-define(INIT_LEAP, 3).
-define(INIT_STRATUM, 16).

-define(STRATUM_MIN, 2).
-define(STRATUM_MAX, 15).

-define(POLL_MIN, 4).
-define(POLL_MAX, 17).
-define(POLL_DEFAULT, 6).

-define(CLOCK_SAMPLES,50).

-define(PHI, 15.0e-6).

-define(SERVER_LIST_DEFAULT, ["cronos.cenam.mx","time.esa.int","ntp.nict.jp","atom.uhr.de",
                              "clock.isc.org","ntp.ix.ru","tick.usask.ca","time.nist.gov"]).