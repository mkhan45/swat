let rec ack (m : int) (n : int) : int = match (m, n) with
| (0, _) -> n + 1
| (_, 0) -> ack (m - 1) 1
| (_, _) -> ack (m - 1) (ack m (n - 1))

let main =
    print_int (ack 3 11)
