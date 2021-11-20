Compiler for the [B programming language](https://www.bell-labs.com/usr/dmr/www/bintro.html).

It will only run on Linux x86_64 systems.

Changes from the official spec:
- Everything is 64 bits, not 36 bits
- Therefore a "character" is is 8 ASCII characters, encoded in a 64 bit word
- The only built in function is "syscall"
