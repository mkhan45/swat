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
    (*tag_to_idx : (string * int) list;*)
    init_fn : W.func';
}

let rec tp_to_json (t : S.tp) : Yojson.t = match t with
| One -> `Null
| Times (l, r) -> `List [tp_to_json l; tp_to_json r]
| Plus ls -> `Assoc (List.map ls ~f:(fun (l, t) -> (l, tp_to_json t)))
| TpName n -> `String n

let tps_to_json (type_ls : (string * S.tp) list) : Yojson.t =
    `Assoc (List.map type_ls ~f:(fun (s, t) -> (s, tp_to_json t)))

let mk_printer (type_ls : (string * S.tp) list) : printer =
    (*let tags =*)
    (*    List.concat_map type_ls ~f:(fun (_n, t) -> tp_tags t)*)
    (*    |> List.dedup_and_sort ~compare:String.compare*)
    (*in*)
    (*let data_str = String.concat ~sep:" " tags in*)
    (*let data_segment = W.{ dinit = data_str; dmode = W.Passive |> C.to_region } |> C.to_region in*)
    let types_ser = tps_to_json type_ls in
    let data_str = Yojson.to_string types_ser in
    let data_segment = W.{ dinit = data_str; dmode = W.Passive |> C.to_region } |> C.to_region in
    let init_fn =
        let push_z = W.Const (C.to_wasm_int 0 |> C.to_region) in
        let sz = (String.length data_str) |> C.to_wasm_int |> C.to_region in
        let push_sz = W.Const sz in
        let body = [push_z; push_z; push_sz; W.MemoryInit (C.to_wasm_imm 0, C.to_wasm_imm 0); push_sz] |> List.map ~f:C.to_region in
        W.{ ftype = C.to_wasm_imm Typegen.unit_to_i32; locals = []; body }
    in
    { data_segment; init_fn }
