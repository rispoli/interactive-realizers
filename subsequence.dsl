let f x =
    (* http://www.math.utep.edu/classes/calculus/COURSE/Tutorials/Chapter3/ *)
    (x - 6) * (x - 8) * (x - 1);; (* set yrange[-50;50] *)
    (* (3 * (x - 2)^2) * (x - 4) ^ 2;; (* set xrange[0:5]; set yrange[0;8] *) *)


(* mu <state> <initial_value> *)
let m s = mu s 1;;

let P y z = not (if y < z then f y <= f z else true);;

let F s =
    [
        lambda s -> [ P; m s; (m s) + 1 ];
    ];;
