let rec range_tailrec (n : int) (acc : int list @ unique) : int list @ unique =
    if n == 0 then acc
              else range_tailrec (n - 1) (n :: acc)

let rec rev (l : int list @ unique) (acc : int list @ unique) : int list @ unique =
    match l with
    | [] -> acc
    | hd :: tl -> rev tl (hd :: acc)

let main =
    for _ = 1 to 5000 do
        let ls @ unique = range_tailrec 100_000 [] in
        let r @ unique = rev ls [] in
        Stdlib.print_int (List.length r)
    done
