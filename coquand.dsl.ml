let phi x = (x + 1) * (x - 2) * ((x - 4) / 2);;

let a = 1;;

(* mu <state> <initial_value> *)
let m s = mu s 1;;

let p n m = phi n > phi m;;

let fam s =
    [
        (fun s -> ( p, m s, (m s) + a ))
    ];;
