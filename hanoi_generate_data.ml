let fichier = open_out "data10.data";;

(* Counter *)
let c=ref(0);;
let init () = c:=0 ;;
let step () = c:= !c+1;;
let get() = !c ;;

let rec aux_hanoi height origin destination other =
  if height=1
  then (
    step();
  )
  else (
    aux_hanoi (height-1) origin other destination ; (* Move n-1 from A to B  *)
    step();
    aux_hanoi (height-1) other destination origin ; (* Move n-1 from B to C *)
  );;

let hanoi height origin destination other =
  init();
  aux_hanoi height origin destination other;
  get();;

let generate max_height =
  for i=1 to max_height do
    let d = hanoi i 0 1 2 in
    let string = (string_of_int i)^","^ (string_of_int d) ^"\n" in
    output_string fichier string  ; done ;;

generate 10;;

close_out fichier;;
