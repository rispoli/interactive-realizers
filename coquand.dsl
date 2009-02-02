let phi x = (x + 1) * (x - 2) * ((x - 4) / 2);;

let a = 1;;

let mu s n =
	let m_i = @(find_m s n) in
		if m_i then
			@(mu s m_i)
		else
			n;;

let m s =
	let n_0 = 1 in
		@(mu s n_0);;

let P n m = @(phi n) > @(phi m);;

let F s =
	{
		lambda s -> { P @(m s) @(m s) + a }
	};;
