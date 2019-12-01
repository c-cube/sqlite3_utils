# Sqlite3_utils [![Actions Status](https://github.com/c-cube/sqlite3_utils/workflows/build/badge.svg)](https://github.com/c-cube/sqlite3_utils/actions)

`Sqlite3_utils` is a high-level wrapper around the
[sqlite3 bindings](https://github.com/mmottl/sqlite3-ocaml)
with utils and helpers functions to manage resources and handle typing and statements.

## Docs

[online docs](https://c-cube.github.io/sqlite3_utils/)

## Examples

A few examples to illustrate basic usage of this library.
Let's assume you have installed the library and run:

```ocaml
# #require "sqlite3_utils";;
# open Sqlite3_utils;;
```

### Executing non parametrized statements

Here we use `with_db` to open a new handle and run some code with this
handle, ensuring the handle is closed when our code returns (no resource leak).
The function `exec0` is a convenient form for running simple statements
that take no parameters and return no values, and `exec_raw_a` deal with `Sqlite3.Data.t`
values for both parameters and values returned by the cursor:

```ocaml
# with_db ":memory:" (fun db ->
   exec0 db "create table person (name text, age int);";
   exec0 db "insert into person values ('alice', 20), ('bob', 25) ;";
   exec_raw_a db "select age from person where name=? ;" [| Data.TEXT "alice" |]
     ~f:Cursor.to_list);;
- : Data.t array list = [[|Sqlite3_utils.Data.INT 20L|]]
```

### Typed API

TODO

### Computing the fibonacci function

We can use sqlite to compute recursive functions with the
[`WITH RECURSIVE` form](https://www.sqlite.org/lang_with.html):

```ocaml
# let fib n =
  let q = "with recursive fib(a,b,c) as
    ( values (1,1,1),(2,1,2) UNION select a+1, c, b+c from fib where a<100)
    select c from fib where a = ?;"
  in
  with_db ":memory:" (fun db ->
      let l =
        exec db q ~ty:Ty.(p1 int, p1 int, id)
          n ~f:Cursor.to_list
      in
      match l with
      | [n] -> n
      | _ -> assert false
    )
    ;;

# fib 10;;
- : int = 89
# fib 15;;
- : int = 987
```

