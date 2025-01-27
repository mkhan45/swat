open Ast

type value = Unit
           | Pair of value * value
           | Label of label * value

type proc = (procname * (varname * varname list * cmd))
type cell = varname * value
type env = cell list

let rec print_value (v : value) f : unit =
  match v with
  | Unit -> f "()"
  | Pair(x, y) -> let _ = f "(" in
                  let _ = print_value x f in
                  let _ = f "," in
                  let _ = print_value y f in
                  f ")"
  | Label(lbl, v) -> let _ = f lbl in
                     let _ = f " " in
                     print_value v f

(* this function is partial because lookup on variable may fail ([] case is not specified)
 * this should be statically caught by the typechecker before evaluation
 *
 * this is a lookup which also removes the value from the environment
 *)
let rec go_read (name : varname) (env : env) (ex : env) : value * env =
  match env with
  | (id, v)::t -> if String.equal name id
                    then (v, ex @ t)
                    else go_read name t (ex @ [(id, v)])
                    (* expensive concat in recursive call *)

let read (name : varname) (env : env) : value * env =
  go_read name env []

let write (name : varname) (value : value) (env : env) : env =
  (name, value) :: env

let go_procs (procs : proc list) (defn : defn) : proc list =
  match defn with
  | ProcDefn(name, (dest, _), params, cmd) -> (name, (dest, List.map fst params, cmd))::procs
  | _ -> procs

let procs (prog : defn list) : proc list =
  List.fold_left go_procs [] prog

let match_pat (v : value) (case : pat * cmd) : bool = 
  match fst case with
  | PairPat(_, _) -> (match v with
                      | Pair(_, _) -> true
                      | _ -> false)
  | UnitPat -> (match v with
                | Unit -> true
                | _ -> false)
  | InjPat(label, _) -> (match v with
                         | Label(label', _) -> String.equal label label'
                         | _ -> false)

(* not a go because you will have to interface with env to provide arguments *)
let rec eval_proc (procs : proc list) (env : env) (cmd : cmd) : env =
  match cmd with
  | Id (dst, src) -> let (v, env') = read src env in write dst v env'
  | Write (dst, pat) ->
      (match pat with
       | UnitPat -> write dst Unit env
       | PairPat(x, y) ->
           let (vx, env') = read x env in
           let (vy, env'') = read y env' in
           write dst (Pair(vx, vy)) env''
       | InjPat(label, x) ->
           let (vx, env') = read x env in
           write dst (Label(label, vx)) env'
      )
    | Cut (_, _, p, q) ->
        let env' = eval_proc procs env p in
        eval_proc procs env' q
    | Call (proc, dst, srcs) ->
        (* res unused, unnecessary because of type system *)
        let (res, params, cmd) = snd (List.find (fun (n, _) -> String.equal n proc) procs) in
        let (args, env') = List.fold_left 
            (fun (vs, env) x -> let (v, env') = read x env in (v::vs, env')) 
            ([], env) 
            srcs in
        let [(_, v)] = eval_proc procs (List.combine params args) cmd in
        (* should only be one value bound in environment after call which must
         * be the result of the call
         *)
        write dst v env'
    | Read(src, cases) ->
        let (v, env') = read src env in
        let (pat, cmd) = List.find (match_pat v) cases in
        match pat with
        | PairPat(x, y) ->
            let Pair(vx, vy) = v in
            let env'' = write y vy (write x vx env') in
            eval_proc procs env'' cmd
        | UnitPat -> eval_proc procs env' cmd
        | InjPat(_, n) ->
            let Label(_, v') = v in
            let env'' = write n v' env' in
            eval_proc procs env'' cmd
        (* val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc *)


let go_eval f (procs : proc list) (defn : defn) : unit =
  match defn with
  | ProcDefn(name, (dest, _), [], cmd) -> 
      let [(_, v)] = eval_proc procs [] cmd in
      let _ = f "value " in
      let _ = f name in
      let _ = f " = " in
      let _ = print_value v f in
      f "\n"
  | _ -> ()

let eval_and_print f (prog : defn list) : unit =
  let _ = List.map (go_eval f (procs prog)) prog in ()

