let get_str_from_file file = 
    let lst = ref [] in
    let in_channel = open_in file in 
    try 
        while true; do
            lst := input_line in_channel :: !lst 
        done; String.concat "" (List.rev !lst)
    with End_of_file ->
        close_in in_channel;
          String.concat "\n" (List.rev !lst);;
