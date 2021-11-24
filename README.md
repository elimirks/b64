Compiler for the [B programming language](https://www.bell-labs.com/usr/dmr/www/bintro.html).

It will only run on Linux x86_64 systems.

Changes from the official spec:
- Everything is 64 bits, not 36 bits
- Therefore a "character" is is 8 ASCII characters, encoded in a 64 bit word
- The only built in function is "syscall"


To compile or run, you'll need to have the GNU assembler (`as`) and GNU linker (`ld`) installed.

## Important differences from C
### Vector sizing
When creating a new vector (called "array" in C), the number provided is the _max index_, not the size.

So the following code will create a vector of 1 element:

```
vec[0];
```

And the following will be of 2 elements, etc:

```
vec[1];
```
### String & char escape sequences
B uses the asterisk character for escape sequences. Here is the mapping of B escape sequences to equivalent C sequences:
```
'**' => '*'
'*n' => '\n'
'*0' => '\0'
'*t' => '\t'
'*'' => '\''
'*"' => '\"'
```
