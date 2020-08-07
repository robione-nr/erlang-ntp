# Erlang NTP Server

Erlang implementation of reference code in RFC 5905: Appendix A.

I had implemented a small client-only version that was incorporated into a TOTP module for a personal project and wanted to create a full server for use in any application for any purpose. I've seen other small implementations on GitHub similar to what I had done originally. Felt that others may want something more... flexible and flushed out. 

This is a gen_server intended for use in a supervisor tree.

### Dependencies

 - https://github.com/robione-nr/erlang-etsmanager

### Usage:

Starting:
  - start_link/0
  - start_link/1
  
API:
  - get_time/1 

### References:

[Network Time Protocol Version 4: Protocol and Algorithms Specification](https://tools.ietf.org/html/rfc5905)

[Network Time Protocol Version 4: Autokey Specification](https://tools.ietf.org/html/rfc5906)

[UDP Checksum Complement in the Network Time Protocol (NTP)](https://tools.ietf.org/html/rfc7821)

[Network Time Protocol Version 4 (NTPv4) Extension Fields](https://tools.ietf.org/html/rfc7822)

[Message Authentication Code for the Network Time Protocol](https://tools.ietf.org/html/rfc8573)
