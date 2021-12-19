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

In the B manual it also mentions `*e` for EOF. Since there is no EOF char in ASCII, this code is left out. It also mentions `*{` and `*}` for `{` and `}`. I'm guessing there was some implementation detail restricting usage of `{` and `}`. So you _could_ use those escape sequences if you want, but you don't need to.
### Logical vs bitwise operators
There are no logical `&&` or `||` operators like in C. However, using the `==`, `!=`, `>=`, `<=`, `>`, or `<` operators will always return `0` or `1`. So you can us the bitwise `&` and `|` operators instead.

In C, this expression would be true: `2 && 1`. But this would be false: `2 & 1`. If you want to replicate the behavior of `&&` for integers, you must do `(2 != 0) & (1 != 0)`.
### Assignment combo operators
The `+=`, `-=`, ..., operators in C are written as `=+`, `=-`, ...

This leads to some whitespace dependence to avoid ambiguity. Specifically, `a =*b` is the same as `a =* b`, but is different than `a = *b`. Similarly with `=-`.

## Compiler notes
### UTF-8
- It will mostly work inside comments (except for UTF-8 sequences with trailing `*/` bytes!).
- It won't work anywhere else

### SIMD
Compile with `RUSTFLAGS='-C target-feature=+avx2'` to leverage SIMD in the lexer. It slightly improves throughput (by about 1% from my benchmarks).
