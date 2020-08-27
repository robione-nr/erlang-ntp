# Erlang NTP Server

Erlang implementation of reference code in RFC 5905: Appendix A.

I had implemented a small client-only version that was incorporated into a TOTP module for a personal project and wanted to create a full server for use in any application for any purpose. I've seen other small implementations on GitHub similar to what I had done originally. Felt that others may want something more... flexible and flushed out. 

This is a gen_server intended for use in a supervisor tree.

### Dependencies (Included if applciable)

 - Linux-based OS.
 - [iface](https://github.com/robione-nr/erlang-utils/blob/master/iface.erl): Used to gather NIC information.
 - [mathx](https://github.com/robione-nr/erlang-utils/blob/master/mathx.erl): Extends math library with some statistical (and other) functions.

### Usage:

**Starting:** 

 - ntp:start(): Default values, NTP port, and servers.
 - ntp:start(Proplist): Property list defining startup parameters.

 Please note port 123 is priviledged and the application makes attempts to forward port 123 using the shell command `ip6tables`. This requires the `sudo` option be set. This is the only purpose for sudo. If the port is not accessible it will only run in `client` mode.

 Start-up Options:
  - {port, _n_}: Port for NTP communication.
  - {mode, _atom_}: Operating mode of applciation. Valid values are - client, server, sym_active, sym_passive, bclient.
  - {sudo, _string_}: Sudo password to attempt port remap.
  - {ref_id, ipv4 | ipv6 | _ip\_tuple_}: Indicates the usage of the IPv4 or IPv6 address of NIC or the ability to specify the IP if one is in a private network and wishes to use the outward-facing IP.
  - {servers, _list_}: Servers to synchronize with. The default is 8 servers in 8-ish countries across 3 continents.
  - {sample_clock, _n_}: Number of consecutive time requests to the clock to establish precision. Default 50.

**API:**
  - get_time/0: Return adjusted system time from the ntp_sysproc gen_server in nanoseconds.
  - get_time/1: As above but per specified time unit identical to erlang time functions.
  - get_offset/0: Gets current offset used by the NTP application to adjust erlang time functions.
  - get_offset/1: As above with specified time units.
  - add_peer/1: Add peers during run time using IPs or names. If the host resolves to multiple IPs a peer process is started for each. (i.e. "pool.ntp.org"). Returns PIDs.
  - drop_peer/1: Terminate a peer during run time by PID.
  - peer_info/1: Get peer information (TBD) by the passed peer PID.
  - enum_peers/0: List of all active peer processes.

### References:

[Network Time Protocol Version 4: Protocol and Algorithms Specification](https://tools.ietf.org/html/rfc5905)

[Network Time Security for the Network Time Protocol](https://datatracker.ietf.org/doc/draft-ietf-ntp-using-nts-for-ntp/?include_text=1)

[Network Time Protocol Version 4 (NTPv4) Extension Fields](https://tools.ietf.org/html/rfc7822)

### Informative:

#### Autokey
 - [Network Time Protocol Version 4: Autokey Specification](https://tools.ietf.org/html/rfc5906)
 - [Message Authentication Code for the Network Time Protocol](https://tools.ietf.org/html/rfc8573)

#### UDP Checksums
 - [UDP Checksum Complement in the Network Time Protocol (NTP)](https://tools.ietf.org/html/rfc7821)
 - [Computation of the Internet Checksum via Incremental Update](https://tools.ietf.org/html/rfc1624)
 - [Incremental Updating of the Internet Checksum](https://tools.ietf.org/html/rfc1624)
 - [Computing the Internet Checksum](https://tools.ietf.org/html/rfc1071)