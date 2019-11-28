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

  type ('a, 'b, 'res) arg = ('b, 'res) t -> ('a -> 'b, 'res) t
  (** The description of an argument of type ['a] *)

  val nil : ('res, 'res) t
  val int : (int, _, _) arg
  val int64 : (int64, _, _) arg
  val float : (float, _, _) arg
  val text : (string, _, _) arg
  val blob : (string, _, _) arg
  val any_str : (string, _, _) arg
  val data : (Data.t, _, _) arg

  val (@>) : ('a, 'b, 'res) arg -> ('b, 'res) t -> ('a -> 'b, 'res) t
  (** Right-associative chaining.
      [int @> float @> nil] is the same as [int (float nil)]. *)
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
  params:('a, 'res) Ty.t ->
  row:(('b, 'c) Ty.t * 'b) ->
  f:('c Cursor.t -> 'res) -> 'a

val transact : t -> (t -> 'a) -> 'a

val atomically : t -> (t -> 'a) -> 'a
