open Core

module S = Ast
module C = Compiler
module W = Wasm.Ast
module WV = Wasm.Value
module WT = Wasm.Types

(* TODO: instead of just tags, serialize all the type info somehow so we can later read it from rust
         and do a proper typed recursive print. Representing tags differently across types breaks
         some subtyping stuff unless we properly convert *)
let rec tp_tags (t : S.tp) : string list = match t with
| One -> []
| Times (l, r) -> tp_tags l @ tp_tags r
| Plus ls -> List.concat_map ls ~f:(fun (l, t) -> l :: tp_tags t)

type printer = {
    data_segment : W.data_segment;
    tag_to_idx : (string * int) list;
    init_fn : W.func;
    print_fns : W.func list;
}

let mk_printer (type_ls : (string, A.tp) list) : printer =
    let tags =
        List.concat_map type_ls ~f:(fun (_n, t) -> tp_tags t)
        |> List.dedup_and_sort ~compare:String.compare
    in
    let data_str = String.concat ~sep:" " tags in
    let data_segment = W.{ dinit = data_str; dmode = W.Passive |> C.to_region } |> C.to_region in
    let rec loop (tags : string list) (offs : int) : (string * int) list = match tags with
    | t :: ts -> (t, offs) :: loop ts (offs + String.length t)
    | [] -> []
    in
    let tag_to_idx = loop tags 0 in
    let init_fn =
        let push_z = W.Const (C.to_wasm_int 0 |> C.to_region) in
        let sz = (String.length data_str) |> C.to_wasm_int |> C.to_region in
        let push_sz = W.Const sz in
        let body = [push_z; push_z; push_sz; W.MemoryInit (C.to_wasm_imm 0, C.to_wasm_imm 0)] |> List.map ~f:C.to_region in
        W.{ ftype = C.to_wasm_imm 0; locals = []; body } |> C.to_region
    in
    let fn_idxs = Compiler.fnd_idxs type_ls in
    let tp_idx tpname = List.findi type_ls ~f:(fun i (n, _) -> String.equal n tpname) |> Option.value_exn in
    { data_segment; tag_to_idx; init_fn }
