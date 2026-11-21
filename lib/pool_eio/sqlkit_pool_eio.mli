(** Connection pool shared by the Eio drivers. Driver SPI, not application API: use a
    driver's own [Pool] module. *)

val boundary :
  operation:string -> (unit -> ('a, Sqlkit.Error.t) result) -> ('a, Sqlkit.Error.t) result
(** Ordinary exceptions become [Error.Callback]. Cancellation, out-of-memory, stack
    overflow and interruption propagate. *)

module Make
    (B : Sqlkit.Driver.S)
    (_ : sig
      val healthy : B.conn -> bool
      (** Whether an idle connection may be handed out again. Must not block. *)
    end) : sig
  type t

  val create :
    sw:Eio.Switch.t ->
    clock:_ Eio.Time.Mono.t ->
    max_size:int ->
    acquire_timeout:float ->
    observe:Sqlkit.Telemetry.observer ->
    (unit -> (B.conn, Sqlkit.Error.t) result) ->
    (t, Sqlkit.Error.t) result
  (** Lazy pool confined to one domain and switch. The function opens one physical
      connection; it runs inside the acquisition deadline. *)

  val use :
    t -> (Sqlkit.conn -> ('a, Sqlkit.Error.t) result) -> ('a, Sqlkit.Error.t) result

  val transaction :
    ?isolation:[ `Read_committed | `Repeatable_read | `Serializable ] ->
    ?retry:int ->
    t ->
    (Sqlkit.conn -> ('a, Sqlkit.Error.t) result) ->
    ('a, Sqlkit.Error.t) result
end
