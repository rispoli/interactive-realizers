let f n =
    (n + 1) * (n - 2) * ((n - 4) - 2) + 10;;

let g n =
    n + 1;;

let h n =
    (n + 7) * ((n - 4) - 2) + 20;;

(* mu <state> <initial_value> *)
let m s = mu s 1;;

let p n m = f n > f m;;

let fam s =
    [
        (fun s -> ( p, m s, g (m s) ));
        (fun s -> ( p, m s, h (m s) ))
    ];;
