let phi x = (x + 1) * (x - 2) * ((x - 4) / 2);;

let a = 1;;

let Z =
    fn f -> (fn x -> f (fn y -> (x x) y))
            (fn x -> f (fn y -> (x x) y));;

let mu f =
    fn s ->
        fn n -> let m_i = find_m s n in
            if m_i then
                (f s) m_i
            else
                n;;

let m s =
    let n_0 = 1 in
        (((Z mu) s) n_0);;

let P n m = phi n > phi m;;

let F s =
    [
        lambda s -> [ P; m s; (m s) + a ]
    ];;
