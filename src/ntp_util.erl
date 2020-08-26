-module(ntp_util).

-author("Nolan Robidoux").

-export([clock_source/1, kod_string/1]).

%% STRATUM == 1 // Reference ID field
clock_source(Code = <<_:4/binary>>) ->
    case Code of
        <<"GOES">> -> "Geosynchronous Orbit Environment Satellite";
        <<"GPS",0>> -> "Global Position System";
        <<"GAL",0>> -> "Galileo Positioning System";
        <<"PPS",0>> -> "Generic pulse-per-second";
        <<"IRIG">> -> "Inter-Range Instrumentation Group";
        <<"WWVB">> -> "LF Radio WWVB Ft. Collins, CO 60 kHz";
        <<"DCF",0>> -> "LF Radio DCF77 Mainflingen, DE 77.5 kHz";
        <<"HBG",0>> -> "LF Radio HBG Prangins, HB 75 kHz";
        <<"MSF",0>> -> "LF Radio MSF Anthorn, UK 60 kHz";
        <<"JJY",0>> -> "LF Radio JJY Fukushima, JP 40 kHz, Saga, JP 60 kHz";
        <<"LORC">> -> "MF Radio LORAN C station, 100 kHz";
        <<"TDF",0>> -> "MF Radio Allouis, FR 162 kHz";
        <<"CHU",0>> -> "HF Radio CHU Ottawa, Ontario";
        <<"WWV",0>> -> "HF Radio WWV Ft. Collins, CO";
        <<"WWVH">> -> "HF Radio WWVH Kauai, HI";
        <<"NIST">> -> "NIST telephone modem";
        <<"ACTS">> -> "NIST telephone modem";
        <<"USNO">> -> "USNO telephone modem";
        <<"PTB",0>> -> "European telephone modem";
        <<"X",_:3/binary>> -> "Experimental Use";
        _ -> undefined
    end.

%% STRATUM == 0 // Reference ID field
kod_string(Code = <<_:4/binary>>) ->
    case Code of
        <<"ACST">> -> "The association belongs to a unicast server.";
        <<"AUTH">> -> "Server authentication failed.";
        <<"AUTO">> -> "Autokey sequence failed.";
        <<"BCST">> -> "The association belongs to a broadcast server.";
        <<"CRYP">> -> "Cryptographic authentication or identification failed.";
        <<"DENY">> -> "Access denied by remote server.";
        <<"DROP">> -> "Lost peer in symmetric mode.";
        <<"RSTR">> -> "Access denied due to local policy.";
        <<"INIT">> -> "The association has not yet synchronized for the first time.";
        <<"MCST">> -> "The association belongs to a dynamically discovered server.";
        <<"NKEY">> -> "No key found. Either the key was never installed or is not trusted.";
        <<"RATE">> -> "Rate exceeded. The server has temporarily denied access because the client exceeded the rate threshold.";
        <<"RMOT">> -> "Alteration of association from a remote host running ntpdc.";
        <<"STEP">> -> "A step change in system time has occurred, but the association has not yet resynchronized.";
        _ -> undefined
    end.