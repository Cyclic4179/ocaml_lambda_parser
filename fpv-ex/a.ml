fun a ->
if a = 0 then 0 else
if a = 1 then 1 else
if a = -1 then -1 else

let inner_loop = fun self -> fun loop -> fun i -> fun j ->
    if j = i
    then loop loop self (i+1)
    else
        if (i/j) = a
        then i
        else
            if (j/i) = a
            then j
            else
                if -(i/j) = a
                then -i
                else
                    if -(j/i) = a
                    then -j
                    else self self loop i (j+1)
in

let loop = fun self -> fun inner_loop -> fun i ->
    inner_loop inner_loop self i 1
in

loop loop inner_loop 2
