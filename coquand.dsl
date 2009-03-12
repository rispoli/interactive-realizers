let phi x = (x + 1) * (x - 2) * ((x - 4) / 2);;

let a = 1;;

(* mu <state> <initial_value> *)
let m s = mu s 1;;

let P n m = phi n > phi m;;

let F s =
    [
        lambda s -> [ P; m s; (m s) + a ]
    ];;
