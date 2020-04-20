(*
                         ATM Account Database
                              Version 1
                          (Association List)
 *)
            

type id = int ;;
  
let db : (id * (string * int)) list ref = ref [] ;;                                           

let create (id : id) (name : string) : unit =
  db := (id,  (name, 0)) :: !db ;;
   
let find (id : id) : string * int =
  List.assoc id !db ;;

let exists (id : id) : bool =
  try
    ignore (find id);
    true
  with
  | Not_found -> false ;;
                                                                        
let balance (id : id) : int =
  let _name, bal = find id
  in bal ;;
  
let name (id : id) : string =
  let name, _bal = find id
  in name ;;
  
let update (id : id) (value : int) : unit =
  let nam, _oldval = find id in
  db := (id, (nam, value)) :: List.remove_assoc id !db ;;

let close (id : id) : unit =
  db := List.remove_assoc id !db ;;

let dump () =
  !db
  |> List.iter (fun (i, (nam, bal)) ->
                  Printf.printf "[%d] %s -> %d\n" i nam bal) ;;
