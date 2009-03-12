let phi x = (x + 1) * (x - 2) * ((x - 4) / 2);;

let a = 1;;

let m s =
    let n_0 = 1 in
        mu s n_0;;

let P n m = phi n > phi m;;

let F s =
    [
        lambda s -> [ P; m s; (m s) + a ]
    ];;
