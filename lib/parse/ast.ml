(** Abstract Syntax Tree for Sax *)

type label = [%import: Ast.label]
type tpname = [%import: Ast.tpname]
type tp = [%import: Ast.tp]
type varname = [%import: Ast.varname]
type procname = [%import: Ast.procname]
type pat = [%import: Ast.pat]
type cmd = [%import: Ast.cmd]
type parm = [%import: Ast.parm]
type defn = [%import: Ast.defn]
type env =  [%import: Ast.env]

module Print = struct

  let rec indent n s = if n = 0 then s else " " ^ indent (n-1) s
  let parens s = "(" ^ s ^ ")"

let rec pp_tp (tau : tp) : string = match tau with
  | Times(tau1,tau2) -> parens (pp_tp tau1 ^ " * " ^ pp_tp tau2)
  | One -> "1"
  | Plus(alts) ->
      "+" ^ "{" ^ String.concat ", " (List.map (fun (l, tau_l) -> l ^ " : " ^ pp_tp tau_l) alts) ^ "}"
  | TpName(a) -> a

let pp_pat (pat : pat) : string = match pat with
  | PairPat(x,y) -> "(" ^ x ^ ", " ^ y ^ ")"
  | UnitPat -> "()"
  | InjPat(l,x) -> l ^ "(" ^ x ^ ")"

let rec pp_cmd (col : int) (p : cmd) : string = match p with
  | Read(x, [(pat,p)]) -> "read " ^ x ^ " " ^ pp_pat pat ^ "\n"
                          ^ indent col (pp_cmd col p)
  | Read(x, branches) -> "read " ^ x ^ " {\n"
                         ^ pp_branches col branches
                         ^ "\n" ^ indent col "}"
  | Write(x, pat) -> "write " ^ x ^ " " ^ pp_pat pat
  | Cut(x, tau, p, q) -> "cut " ^ x ^ " : " ^ pp_tp tau ^ " {\n"
                        ^ indent (col+4) (pp_cmd (col+4) p) ^ "\n"
                        ^ indent col "}\n"
                        ^ indent col (pp_cmd col q)
  | Id(x, y) -> "id " ^ x ^ " " ^ y
  | Call(f, x, ys) -> "call " ^ f ^ " " ^ x ^ " " ^ String.concat " " ys

and pp_branches col branches = match branches with
  | [(pat, p)] -> pp_branch col (pat, p)
  | ((pat, p)::branches) -> pp_branch col (pat, p) ^ "\n"
                            ^ pp_branches col branches
  | [] -> raise (Failure "empty branches")

and pp_branch col branch = match branch with
    (pat, p) -> let prefix = "| " ^ pp_pat pat ^ " => " in
                let k = String.length prefix in
                indent col (prefix ^ pp_cmd (col+k) p)

let pp_parm x_tau = match x_tau with
  | (x, tau) -> parens (x ^ " : " ^ pp_tp tau)

let pp_parms y_sigmas = String.concat " " (List.map pp_parm y_sigmas)

let rec pp_defn defn = match defn with
  | TypeDefn(a, tau) -> "type " ^ a ^ " = " ^ pp_tp tau ^ "\n"
  | ProcDefn(f, x_tau, y_sigmas, body) ->
     "proc " ^ f ^ " " ^ pp_parm x_tau ^ " " ^ pp_parms y_sigmas ^ " =\n"
     ^ indent 4 (pp_cmd 4 body) ^ "\n"
  | FailDefn(defn) -> "fail\n" ^ pp_defn defn

let pp_env defns = String.concat "\n" (List.map pp_defn defns)

end (* module Print *)
