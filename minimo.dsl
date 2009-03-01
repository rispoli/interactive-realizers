let f n =
    (n + 1) * (n - 2) * ((n - 4) / 2) + 10;;

let g n =
    n + 3;;

let h n =
    (n + 7) * ((n - 4) / 2) + 20;;

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

let P n m = f n > f m;;

let F s =
    [
        lambda s -> [ P; m s; g (m s) ];
        lambda s -> [ P; m s; h (m s) ]
    ];;
