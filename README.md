# build
- make

# use with
```sh
(tr "\n" " "; echo) | sed "s/\(\*.*\*\)//g" <a.ml | ./parser
```

or just `./parser` for stdin from terminal

note:
**nested comments (`(*` to `*)`) are not supported**
