# build
- make

# use with
```sh
./parsefile thefile
```
(multiline and nested comments are supported, syntax as in ocaml: `(*...*)`)
(perl required to run script)

or:
```sh
(<thefile tr "\n" " "; echo) | ./parser
```

or just `./parser` for stdin from terminal

**note:
comments (`(*` to `*)`) are only supported when using the ./parsefile script**

# language
see garmmar in ./parser.mly, lexer in ./lexer.mll

## ex
excercise in ./fpv-ex
