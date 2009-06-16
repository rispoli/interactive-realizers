type pattern = Num of int | Pair of (int -> int -> bool) * int

let already_in pattern sn =
    let rec already_in pattern = function
        (* Comparing between functions obviously works only for named ones *)
        | [] -> None
        | (p', n', m') :: _  when (pattern p' n') -> Some m'
        | _ :: xs -> already_in pattern xs
    in
        match pattern with
            | Num n -> already_in (fun _ n' -> n' = n) sn
            | Pair (p, n) -> already_in (fun p' n' -> p' == p && n' = n) sn

let find_m s n =
    already_in (Num n) s

let consistent (p, n, m) sn =
    (already_in (Pair (p, n)) sn = None) && (p n m)

let incr_know fi_sn sn =
    sn @ [fi_sn]
