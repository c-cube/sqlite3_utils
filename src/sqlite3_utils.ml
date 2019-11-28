
(* This file is free software, part of bender-ocaml. See file "license" for more details. *)

(** {1 Utils for SQLite} *)

include Sqlite3

exception RcError of Sqlite3.Rc.t
exception Type_error of Data.t

let () = Printexc.register_printer
    (function
      | RcError rc -> Some ("sqlite error: " ^ Sqlite3.Rc.to_string rc)
      | _ -> None)

type t = db

let check_ret = function
  | Sqlite3.Rc.DONE
  | Sqlite3.Rc.OK -> ()
  | rc -> raise (RcError rc)

(* on "busy", wait 300ms before failing *)
let setup_timeout ?(ms=300) db : unit =
  Sqlite3.busy_timeout db ms

let bind_ stmt i d : unit = check_ret (bind stmt i d)

(** Parameters passed to a statement *)
module Ty = struct
  type _ arg =
    | Int : int arg
    | Int64 : int64 arg
    | Float : float arg
    | String : [`Blob|`Text|`Both] -> string arg
    | Data : Data.t arg

  type (_,_) t =
    | Nil : ('res, 'res) t
    | Cons : 'a arg * ('b, 'res) t -> ('a -> 'b, 'res) t
    (* TODO: add this (with distinct count for args and row)
       | Raw : (Data.t array, 'res) t
    *)

  let nil = Nil
  let int = Int
  let int64 = Int64
  let float = Float
  let text = String `Text
  let blob = String `Blob
  let any_str = String `Both
  let data = Data

  let (@>) x y = Cons (x,y)
  let p1 x = Cons (x,Nil)
  let p2 x y = Cons (x,Cons (y,Nil))
  let p3 x y z = Cons (x,Cons (y,Cons (z,Nil)))
  let p4 x y z w = Cons (x,Cons (y,Cons (z,Cons (w,Nil))))

  let rec count : type a r. (a, r) t -> int
    = function
    | Nil -> 0
    | Cons (_, tl) -> 1 + count tl

  (* translate parameters *)
  let rec tr_args
    : type a res. Sqlite3.stmt -> int -> (a,res) t -> (unit->res) -> a
    = fun stmt i p cb ->
      match p with
      | Nil -> cb()
      | Cons(Int, k) ->
        (fun x -> bind_ stmt i (Data.INT (Int64.of_int x)); tr_args stmt (i+1) k cb)
      | Cons (Int64, k) ->
        (fun x -> bind_ stmt i (Data.INT x); tr_args stmt (i+1) k cb)
      | Cons (String (`Text|`Both),k) ->
        (fun x -> bind_ stmt i (Data.TEXT x); tr_args stmt (i+1) k cb)
      | Cons (String `Blob,k) ->
        (fun x -> bind_ stmt i (Data.BLOB x); tr_args stmt (i+1) k cb)
      | Cons (Float, k) ->
        (fun x -> bind_ stmt i (Data.FLOAT x); tr_args stmt (i+1) k cb)
      | Cons (Data, k) ->
        (fun x -> bind_ stmt i x; tr_args stmt (i+1) k cb)

  (* translate results *)
  let rec tr_row
    : type a res. (int->Data.t) -> int -> (a,res) t -> a -> res
    = fun get i ty f -> match ty with
      | Nil -> f
      | Cons (Data, k) ->
        let data = get i in
        tr_row get (i+1) k (f data)
      | Cons (Int64, k) ->
        (match get i with
         | Data.INT x -> tr_row get (i+1) k (f x)
         | d -> raise (Type_error d))
      | Cons (Int, k) ->
        (match get i with
         | Data.INT x as d -> 
           let x = try Int64.to_int x with _ -> raise (Type_error d) in
           tr_row get (i+1) k (f x)
         | d -> raise (Type_error d))
      | Cons (Float, k) ->
        (match get i with
         | Data.FLOAT x -> tr_row get (i+1) k (f x)
         | d -> raise (Type_error d))
      | Cons (String kind, k) ->
        (match get i, kind with
         | Data.BLOB x, `Blob -> tr_row get (i+1) k (f x)
         | Data.TEXT x, `Text -> tr_row get (i+1) k (f x)
         | (Data.BLOB x | Data.TEXT x), `Both -> tr_row get (i+1) k (f x)
         | d, _ -> raise (Type_error d))
end

module Cursor = struct
  type 'a t = {
    stmt: Sqlite3.stmt;
    read: Sqlite3.stmt -> 'a;
    mutable cur: 'a option;
  }

  let next_ self : unit =
    match Sqlite3.step self.stmt with
    | Sqlite3.Rc.DONE ->
      self.cur <- None;
    | Sqlite3.Rc.ROW ->
      let x = self.read self.stmt in
      self.cur <- Some x
    | rc -> raise (RcError rc)

  let make_ stmt read =
    let self = { stmt; cur=None; read; } in
    next_ self;
    self

  let make stmt ty f =
    let read stmt = Ty.tr_row (Sqlite3.column stmt) 0 ty f in
    make_ stmt read

  let make_raw stmt : Data.t array t =
    make_ stmt Sqlite3.row_data

  (* next value in the cursor *)
  let next self : _ option =
    match self.cur with
    | None -> None
    | Some _ as x ->
      next_ self;
      x

  let rec iter ~f self = match self.cur with
    | None -> ()
    | Some res ->
      f res;
      next_ self;
      iter ~f self

  let to_seq self =
    let rec get_next () =
      let n = lazy (
        match self.cur with
        | None -> Seq.Nil
        | Some x ->
          next_ self;
          let tl = get_next () in
          Seq.Cons (x, tl)
      ) in
      fun () -> Lazy.force n
    in
    get_next ()

  (* convert a cursor into a list of answers *)
  let to_list_rev (c:'a t) : 'a list =
    let rec aux acc c = match next c with
      | None -> acc
      | Some d -> aux (d::acc) c
    in
    aux [] c

  let to_list c = List.rev (to_list_rev c)
end

let finally_ ~h x f =
  try
    let res = f x in
    h x;
    res
  with e ->
    h x;
    raise e

let finalize_check_ stmt = check_ret @@ Sqlite3.finalize stmt

let with_stmt db str ~f =
  let stmt = Sqlite3.prepare db str in
  finally_ ~h:finalize_check_ stmt f

let check_arity_params_ stmt n : unit =
  if Sqlite3.bind_parameter_count stmt <> n then (
    invalid_arg
      (Format.sprintf "wrong number of parameters: expected %d, got %d"
         (Sqlite3.bind_parameter_count stmt) n);
  )

let check_arity_res_ stmt n : unit =
  if Sqlite3.column_count stmt <> n then (
    invalid_arg
      (Format.sprintf "wrong number of columns in result: expected %d, got %d"
         (Sqlite3.column_count stmt) n);
  )

(* execute statement, return cursor *)
let exec_raw db str ~f =
  with_stmt db str
    ~f:(fun stmt ->
        check_arity_params_ stmt 0;
        f (Cursor.make_raw stmt))

let exec0 db str : unit = check_ret @@ Sqlite3.exec db str

(* execute statement parametrized by the array of arguments *)
let exec_raw_a db str a ~f =
  with_stmt db str
    ~f:(fun stmt ->
        check_arity_params_ stmt (Array.length a);
        Array.iteri (fun i x -> check_ret (Sqlite3.bind stmt (i+1) x)) a;
        f (Cursor.make_raw stmt))

(* execute statement parametrized by the array of arguments *)
let exec db str ~ty ~f =
  let params, ty_r, f_r = ty in
  let stmt = Sqlite3.prepare db str in
  check_arity_params_ stmt (Ty.count params);
  (* caution, bind starts at [1] *)
  Ty.tr_args stmt 1 params
    (fun () ->
       finally_ ~h:finalize_check_ stmt
         (fun stmt ->
            check_arity_res_ stmt (Ty.count ty_r);
            f (Cursor.make stmt ty_r f_r)))

let exec_no_params db str ~ty ~f =
  with_stmt db str
    ~f:(fun stmt ->
        check_arity_params_ stmt 0;
        let ty_r, f_r = ty in
        f (Cursor.make stmt ty_r f_r))

let exec_no_cursor db str ~ty =
  let stmt = Sqlite3.prepare db str in
  check_arity_params_ stmt (Ty.count ty);
  (* caution, bind starts at [1] *)
  Ty.tr_args stmt 1 ty
    (fun () ->
       finally_ ~h:finalize_check_ stmt
         (fun stmt ->
            check_arity_res_ stmt 0;
            (* just execute one step *)
            check_ret @@ Sqlite3.step stmt))

(* From [ocaml-sqlite3EZ](https://github.com/mlin/ocaml-sqlite3EZ),
   with some changes. Compatible license (MIT) *)
let transact db f =
  exec0 db "BEGIN;";
  try
    let y = f db in
    exec0 db "COMMIT;";
    y
  with
  | e ->
    exec0 db "ROLLBACK;";
    raise e

(* From [ocaml-sqlite3EZ](https://github.com/mlin/ocaml-sqlite3EZ),
   with some changes. Compatible license (MIT) *)
let atomically db f =
  exec0 db "SAVEPOINT a;";
  try
    let y = f db in
    exec0 db "RELEASE a;";
    y
  with
  | exn ->
    exec0 db "RELEASE a;";
    exec0 db "ROLLBACK TO a;";
    raise exn
