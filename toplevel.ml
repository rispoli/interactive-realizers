#use "find-state.ml"

let rec mu s n =
    let m_i = find_m s n in
    match m_i with
        | None -> n
        | Some m_i' -> mu s m_i'

let find_solution ?(initial_state = []) f initial_value =
    let final_state = find_state f initial_state in
        print_endline ("valid solution in x = " ^ string_of_int (mu final_state initial_value));
        final_state
