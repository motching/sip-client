### What is this
A SIP client, definitely not full-featured
### Todos

- [X] Create project
- [X] Basic types for SIP messages
- [X] Types for headers
- [X] Parse request method
- [X] State handling
- [X] Establish SIP/UDP connection
- [X] Construct and send a valid SIP message over UDP
- [X] Change naive parsing: use attoparsec
- [X] Parse whole Request-Line
- [ ] Parse mandatory headers necessary for constructing a 100 Trying message
- [ ] Send a semantically valid 100 Trying to an incoming INVITE
- [ ] Minimal working voice call signaling
- [ ] Tests for Request-Line parsing
- [ ] Parse other headers (a general solution ideally)
- [ ] Tests for general message parsing
- [ ] Transaction management (requesting side)
- [ ] Transaction management (responding side)
- [ ] Some form of "end-to-end" testing
- [ ] Dialog management (requesting side)
- [ ] Dialog management (responding side)
- [ ] Dialog test cases
- [ ] TCP buffering
- [ ] SDP parsing
- [ ] Strict mode / tolerant mode
