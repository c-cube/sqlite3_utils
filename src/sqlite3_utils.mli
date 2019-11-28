include module type of Sqlite3
  with module Data = Sqlite3.Data
  and module Rc = Sqlite3.Rc
  and type db = Sqlite3.db

exception RcError of Rc.t
(** Exception raised by most of the functions below when a Sqlite failure
    occurs, with the corresponding error code. *)

exception Type_error of Data.t
(** Exception raised when the declared {!Ty.t} does not match the
    actual result returned by Sqlite. *)

type t = db
(** Alias for the DB connection *)

val check_ret : Rc.t -> unit
(** Check return code.
    @raise RcError if the code is not {!Sqlite3.Rc.DONE} or {!Sqlite3.Rc.OK}. *)

val setup_timeout : ?ms:int -> t -> unit
(** on "busy", wait [ms] milliseconds before failing. *)

(** Values representing types to pass to a statement, or to extract from 
    a row *)
module Ty : sig
  type ('a, 'res) t

  type 'a arg

  val int : int arg
  val int64 : int64 arg
  val float : float arg
  val text : string arg
  val blob : string arg
  val any_str : string arg
  val data : Data.t arg

  val nil : ('res, 'res) t
  (** 0 arguments *)

  val (@>) : 'a arg -> ('b, 'res) t -> ('a -> 'b, 'res) t
  (** Right-associative chaining.
      [int @> float @> nil] is the same as [int (float nil)]. *)

  val p1: 'a arg -> ('a -> 'res, 'res) t
  val p2: 'a arg -> 'b arg -> ('a -> 'b -> 'res, 'res) t
  val p3: 'a arg -> 'b arg -> 'c arg -> ('a -> 'b -> 'c -> 'res, 'res) t
  val p4: 'a arg -> 'b arg -> 'c arg -> 'd arg -> ('a -> 'b -> 'c -> 'd -> 'res, 'res) t
end

module Cursor : sig
  type 'a t
  (** A cursor yielding values of type ['a] *)

  val next : 'a t -> 'a option

  val iter : f:('a -> unit) -> 'a t -> unit

  val to_seq : 'a t -> 'a Seq.t
  (** Lazy iterator over the values.
      Be careful not to let this leak outside the scope of a statement. *)

  val to_list_rev : 'a t -> 'a list

  val to_list : 'a t -> 'a list
end

val with_stmt : t -> string -> f:(Sqlite3.stmt -> 'a) -> 'a
(** Locally make a statement out of the given string, then cleanup
    when [f] returns. *)

val exec0 : t -> string -> unit
(** Run the query purely for its side effects. *)

val exec_raw :
  t ->
  string ->
  f:(Data.t array Cursor.t -> 'b) ->
  'b

val exec_raw_a :
  t ->
  string ->
  Sqlite3.Data.t array ->
  f:(Data.t array Cursor.t -> 'b) ->
  'b

val exec :
  t -> string ->
  ty:( ('a, 'res) Ty.t * ('b, 'c) Ty.t * 'b ) ->
  f:('c Cursor.t -> 'res) -> 'a

val exec_no_params :
  t -> string ->
  ty:(('b, 'c) Ty.t * 'b) ->
  f:('c Cursor.t -> 'res) ->
  'res

val exec_no_cursor :
  t -> string ->
  ty:('a, unit) Ty.t ->
  'a

val transact : t -> (t -> 'a) -> 'a

val atomically : t -> (t -> 'a) -> 'a
