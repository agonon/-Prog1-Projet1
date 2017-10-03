(* Counter *)
let c=ref(0);;
let init () = c:=0 ;;
let step () = c:= !c+1;;
let get() = !c ;;


let movement origin destination =
  print_string ( "I move a disc from rod " ^ origin ^ "  to rod " ^ destination);
  print_newline() ;;

let rec aux_hanoi height origin destination other =
  if height=1
  then (
    movement origin destination;
    step();
  )
  else (
    aux_hanoi (height-1) origin other destination ; (* Move n-1 from A to B  *)
    movement origin destination ; (* Move the higher from A to C *)
    step();
    aux_hanoi (height-1) other destination origin ; (* Move n-1 from B to C *)
  );;

let hanoi height origin destination other =
  init();
  aux_hanoi height origin destination other;
  get();;

hanoi 20 "A" "C" "B";;
