let rec range_tailrec (n : int) (acc : int list) =
    if n == 0 then acc
              else range_tailrec (n - 1) (n :: acc)

let rec rev (l : int list) (acc : int list) =
    match l with
    | [] -> acc
    | hd :: tl -> rev tl (hd :: acc)

let main =
    for _ = 1 to 500 do
        let ls = range_tailrec 10_000 [] in
        let r = rev ls [] in
        Stdlib.print_int (List.length r)
    done
