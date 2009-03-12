let f n =
    (n + 1) * (n - 2) * ((n - 4) / 2) + 10;;

let g n =
    n + 3;;

let h n =
    (n + 7) * ((n - 4) / 2) + 20;;

(* mu <state> <initial_value> *)
let m s = mu s 1;;

let P n m = f n > f m;;

let F s =
    [
        lambda s -> [ P; m s; g (m s) ];
        lambda s -> [ P; m s; h (m s) ]
    ];;
