Compiler for the [B programming language](https://www.bell-labs.com/usr/dmr/www/bintro.html).

It will only run on Linux x86_64 systems.

To compile or run, you do NOT need any GNU tools like `as` or `ld` anymore!

The generated binary isn't great, but it supports everything in the original B.

## Using b64
Run `-h` for help:
```
USAGE:
    ./target/release/b64 [OPTIONS] [INPUTS]
OPTIONS:
    -h, --help     Print this message
    -o <OUTPUT>    Write the output to the given path
    -r             Directly run instead of saving the binary
    --             Any args after '--' will pass through
```

### Examples
See the `examples` or `test` directory! The official [B reference manual](https://www.bell-labs.com/usr/dmr/www/bref.html) and [B tutorial](https://www.bell-labs.com/usr/dmr/www/btut.html) explain all the supported functionality.

Use this commands to run compiler tests:
```
cargo run -- -r run_tests.b
```
### Standard Library
There are some standard library functions defined in `assets`. They aren't bundled with the release for now, so if you want to use them you'll have to download them.

In the future when the stdlib is more complete, there will be a more convenient way to access it.
## Changes from the official B spec
### Everything is 64 bits, not 36 bits
Therefore a "character" is is 8 ASCII characters, encoded in a 64 bit word
### The only built in function is "syscall"
See [this article](https://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/) for reference.
### String terminator
In B, the EOT character was used for character & string termination. To make interoperability with other languages easier, I made them null terminated instead.
### Import keyword
To make life a bit easier, I also added an `#import` keyword which will compile the B program at the given path. Importing the same file twice will have no effect. Unlike C, it must end with a semicolon:

```
#import "assets/stdlib.b";
```
### Define keyword
`#define` is supported for variable-like and function-like macros:

```
#import "../assets/best.b";

#define THE_ANSWER 42;
#define LUCKY (13);
#define SEMIPRIME() 35;
#define BINCEIL(n, m) (n + m) & ~m;

main() {
    assert_eq_int(42, THE_ANSWER);
    assert_eq_int(13, LUCKY());
    assert_eq_int(35, SEMIPRIME);
    assert_eq_int(8, BINCEIL(3, 7));
}
```

Unlike C, the body of the `#define` must be a valid expression, and terminated with a semicolon `;`. So, you DON'T need to escape newlines with `\`.
### CLI args
`argc` and `argv` are passed into the `main` function:

```
main(argc, argv) {
    ...
}
```

To pass args when using the `-r` flag, use `--`:

```
$ b64 -r examples/args.b -- hello world
argc:    3
argv:    140723691158248
argv[0]: /tmp/b64_61992.bin
argv[1]: hello
argv[2]: world
```
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
'*e' => 0x04 (EOT char)
'*0' => '\0'
'*t' => '\t'
'*'' => '\''
'*"' => '\"'
```

It also mentions `*{` and `*}` for `{` and `}`. I'm guessing there was some implementation detail restricting usage of `{` and `}`. So you _could_ use those escape sequences if you want, but you don't need to.
### Logical vs bitwise operators
There are no logical `&&` or `||` operators like in C. However, using the `==`, `!=`, `>=`, `<=`, `>`, or `<` operators will always return `0` or `1`. So you can us the bitwise `&` and `|` operators instead.

In C, this expression would be true: `2 && 1`. But this would be false: `2 & 1`. If you want to replicate the behavior of `&&` for integers, you must do `(2 != 0) & (1 != 0)`.
### Assignment combo operators
The `+=`, `-=`, ..., operators in C are written as `=+`, `=-`, ...

This leads to some whitespace dependence to avoid ambiguity. Specifically, `a =*b` is the same as `a =* b`, but is different than `a = *b`. Similarly with `=-`.
### Function pointers
Any expression can be called. If it's an expression other than a function name, the expression value will be considered a function pointer.

To create a reference to a function, us the syntax `&fun`.
## Compiler notes
### UTF-8
- It will mostly work inside comments (except for UTF-8 sequences with trailing `*/` bytes!).
- It won't work anywhere else
### Max binary size
The max binary size is effectively 2^31. This is a constraint of Linux.
Technically I think you can increase that by avoiding instruction-relative addressing, but then you're looking at some gnarly generated opcodes.
### SIMD
Compile with `RUSTFLAGS='-C target-feature=+avx2'` to leverage SIMD in the lexer. It slightly improves throughput (by about 1% from my benchmarks).
