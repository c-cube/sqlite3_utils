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

Most functions come with `f` and `f_exn` versions, the latter raising `RcError rc`
where Sqlite returns the error code `rc`, the former returning a `('a, Rc.t) result`.

### Executing non parametrized statements

Here we use `with_db` to open a new handle and run some code with this
handle, ensuring the handle is closed when our code returns (no resource leak).
The function `exec0_exn` is a convenient form for running simple statements
that take no parameters and return no values, and `exec_raw_args` deal with `Sqlite3.Data.t`
values for both parameters and values returned by the cursor:

```ocaml
# with_db ":memory:" (fun db ->
   exec0_exn db "create table person (name text, age int);";
   exec0_exn db "insert into person values ('alice', 20), ('bob', 25) ;";
   exec_raw_args db "select age from person where name=? ;" [| Data.TEXT "alice" |]
     ~f:Cursor.to_list);;
- : (Data.t array list, Rc.t) result = Ok [[|Sqlite3_utils.Data.INT 20L|]]
```

### Typed API

Let us re-consider the previous example but with the typed API:

```ocaml
# with_db ":memory:" (fun db ->
   exec0_exn db "create table person (name text, age int);";
   exec0_exn db "insert into person values ('alice', 20), ('bob', 25) ;";
   exec db "select age from person where name=? ;"
    ~ty:Ty.([text], [int], (fun (x:int) -> x))
    "alice"
     ~f:Cursor.to_list);;
- : (int list, Rc.t) result = Ok [20]
```

We provide a `~ty` argument that, in the most general case, `exec`,
for a parametrized statement that returns values, defines the type
of parameters and the type of return values.
Here `ty` is a triple `(type of params, type of result columns, function f)`
where `f` turns the list of returned columns into a single value. `f`
can typically be used to build a tuple or record for each result row.

The module `Sqlite3_utils.Ty` contains combinators for declaring types
(base types: `text`, `int`, `blob`, etc. and composition operators including
`::` so that one can one `[int; text]`)
as well as for making simple tuples.

### Computing the fibonacci function

We can use sqlite to compute recursive functions with the
[`WITH RECURSIVE` form](https://www.sqlite.org/lang_with.html):

```ocaml
# let fib n =
  let q = "with recursive fib(a,b,c) as
    ( values (1,1,1),(2,1,2) UNION select a+1, c, b+c from fib where a<=?)
    select c from fib where a = ?;"
  in
  with_db ":memory:" (fun db ->
      let l =
        exec db q ~ty:Ty.([int; int], [int], p1 int, id)
          n n ~f:Cursor.to_list
      in
      match l with
      | Ok [n] -> n
      | _ -> assert false
    )
    ;;

# fib 10;;
- : int = 89
# fib 15;;
- : int = 987
```

