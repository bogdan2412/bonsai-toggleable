open! Core
open! Import

module State : sig
  type t =
    | Untoggled
    | Toggled
  [@@deriving sexp_of]
end

type 'a t =
  { value : 'a
  ; toggle : unit -> unit Ui_effect.t
  ; set_state : State.t -> unit Ui_effect.t
  }

val state_machine : State.t t Bonsai.Computation.t
val of_static : (State.t -> 'a) -> 'a t Bonsai.Computation.t

val of_computations
  :  untoggled:'a Bonsai.Computation.t
  -> toggled:'a Bonsai.Computation.t
  -> 'a t Bonsai.Computation.t

include Applicative.S with type 'a t := 'a t

val return_computation : 'a Bonsai.Computation.t -> 'a t Bonsai.Computation.t

module Let_syntax : sig
  val return : 'a -> 'a t

  include Applicative.Applicative_infix with type 'a t := 'a t

  module Let_syntax : sig
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t
  end
end

module Computation : sig
  type nonrec 'a t = 'a t Bonsai.Computation.t

  include Applicative.S with type 'a t := 'a t

  module Let_syntax : sig
    val return : 'a -> 'a t

    include Applicative.Applicative_infix with type 'a t := 'a t

    module Let_syntax : sig
      val return : 'a -> 'a t
      val map : 'a t -> f:('a -> 'b) -> 'b t
      val both : 'a t -> 'b t -> ('a * 'b) t
    end
  end
end
