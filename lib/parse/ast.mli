(** Abstract Syntax Trees for SAX *)

(* TYPES *)

    type label = string
    type tpname = string

    type tp = Times of tp * tp
            | One
            | Plus of (label * tp) list
            | TpName of tpname
    (* | MarkedTp of tp Mark.marked *)

    type varname = string
    type procname = string

    type pat = PairPat of varname * varname
             | UnitPat
             | InjPat of label * varname
    (* | MarkedPat of pat Mark.marked *)

    type cmd = Read of varname * (pat * cmd) list
             | Write of varname * pat
             | Cut of varname * tp * cmd * cmd
             | Id of varname * varname
             | Call of procname * varname * varname list
    (* | MarkedCmd of cmd Mark.marked *)

    type parm = varname * tp

    (* type ext = Mark.ext option *)

    type defn = TypeDefn of tpname * tp (* * ext *)
              | ProcDefn of procname * parm * parm list * cmd (* * ext*)
              | FailDefn of defn (* * ext *)

    type env = defn list

    (** Print as source, with redundant parentheses *)
    (* these print redundant '(...)' and '{...}' *)
    
    module Print : sig
      val pp_tp : tp -> string
      val pp_pat : pat -> string
      val pp_cmd : int -> cmd -> string 
      val pp_defn : defn -> string
      val pp_env : env -> string
    end

