#use "find-state-with-lists.ml"

let rec sn1 fsn sn =
    match fsn with
        | [] -> sn
        | x :: xs -> let fi_sn = x sn in
                        if consistent fi_sn sn then
                            incr_know fi_sn sn
                        else
                            sn1 xs sn

let rec find_state f initial_state =
    let fsn = f initial_state in
        let new_state = sn1 fsn initial_state in
            if new_state == initial_state then
                new_state
            else find_state f new_state
