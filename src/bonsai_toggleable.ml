open! Core
open! Import

module State = struct
  type t =
    | Untoggled
    | Toggled
  [@@deriving compare, sexp]
end

module T = struct
  module T0 = struct
    type 'a t =
      { value : 'a
      ; toggle : unit -> unit Ui_effect.t
      ; set_state : State.t -> unit Ui_effect.t
      }

    let ignore_toggle _ = Ui_effect.Ignore
    let return value = { value; toggle = ignore_toggle; set_state = ignore_toggle }

    let map2
          { value = value_a; toggle = toggle_a; set_state = set_state_a }
          { value = value_b; toggle = toggle_b; set_state = set_state_b }
          ~f
      =
      { value = f value_a value_b
      ; toggle = (fun () -> Ui_effect.Many [ toggle_a (); toggle_b () ])
      ; set_state = (fun state -> Ui_effect.Many [ set_state_a state; set_state_b state ])
      }
    ;;

    let map { value; toggle; set_state } ~f = { value = f value; toggle; set_state }
  end

  include T0

  include Applicative.Make_using_map2 (struct
      include T0

      let map = `Custom map
    end)

  module Let_syntax = struct
    let return = return

    include Applicative_infix

    module Let_syntax = struct
      let return = return
      let map = map
      let both = both
    end
  end
end

include T

module Action = struct
  type t =
    | Toggle
    | Set_state of State.t
  [@@deriving sexp_of]
end

let state_machine =
  let%map.Bonsai.Computation value, inject =
    Bonsai.state_machine
      ~sexp_of_model:[%sexp_of: State.t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~equal:[%compare.equal: State.t]
      ~default_model:Untoggled
      ~apply_action:
        (fun
          (_ : (Action.t, unit) Bonsai.Apply_action_context.t) state action ->
        match action with
        | Toggle ->
          (match state with
           | Untoggled -> Toggled
           | Toggled -> Untoggled)
        | Set_state state -> state)
      ()
  in
  { value
  ; toggle = (fun () -> inject Toggle)
  ; set_state = (fun state -> inject (Set_state state))
  }
;;

let of_static f =
  let%map.Bonsai.Computation state = state_machine in
  let%map.T state = state in
  f state
;;

let of_computations ~untoggled ~toggled =
  let%map.Bonsai.Computation state = state_machine
  and toggled = toggled
  and untoggled = untoggled in
  let%map.T state = state in
  match state with
  | Untoggled -> untoggled
  | Toggled -> toggled
;;

let return_computation t =
  let%map.Bonsai.Computation t = t in
  return t
;;

module Computation = struct
  module T = struct
    type nonrec 'a t = 'a t Bonsai.Computation.t

    let return value = Bonsai.Computation.return (return value)
    let map2 t1 t2 ~f = Bonsai.Computation.map2 t1 t2 ~f:(map2 ~f)
    let map t ~f = Bonsai.Computation.map t ~f:(map ~f)
  end

  include T

  include Applicative.Make_using_map2 (struct
      include T

      let map = `Custom map
    end)

  module Let_syntax = struct
    let return = return

    include Applicative_infix

    module Let_syntax = struct
      let return = return
      let map = map
      let both = both
    end
  end
end
