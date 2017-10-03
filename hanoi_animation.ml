(* Hanoi with 3 rods *)

#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Unix;;

let height=11;; (* Height of the source in the begining *)

(* Arrays *)
let nbr_of_disks = Array.make 3 0 ;;
let ident = Array.make_matrix 3 height (-1) ;;

let draw_disk id n x=
  let lo=10+id*10 and l=20 in
  fill_poly [|(x-lo,n*l);(x+lo,n*l);(x+lo,(n+1)*l);(x-lo,(n+1)*l)|]
;;

(* Draw the actual situation *)
let draw () =
  clear_graph ();

  (* Rods *)
  let height=500 and x = Array.make 3 0 and tn=10 in
  x.(0) <- 250;
  x.(1) <- 550;
  x.(2) <- 850;
  
  set_color black;
  fill_poly [|(x.(0)-tn,0);(x.(0)+tn,0);(x.(0)+tn,height);(x.(0)-tn,height)|];
  fill_poly [|(x.(1)-tn,0);(x.(1)+tn,0);(x.(1)+tn,height);(x.(1)-tn,height)|];
  fill_poly [|(x.(2)-tn,0);(x.(2)+tn,0);(x.(2)+tn,height);(x.(2)-tn,height)|];

  (* Disks *)
  set_color magenta;
  for i=0 to 2 do
    let n=nbr_of_disks.(i) in
    for j=0 to (n-1) do
      draw_disk ident.(i).(j) (n-j-1) x.(i);
      done; done;
;;

let movement origin destination  =
  nbr_of_disks.(origin)<-nbr_of_disks.(origin)-1;
  nbr_of_disks.(destination)<-nbr_of_disks.(destination)+1;
  let n=Array.length ident.(0) and id=ident.(origin).(0) in
  for j=n-1 downto 1 do
    ident.(destination).(j)<-ident.(destination).(j-1); done;
  for j=1 to n-1 do
    ident.(origin).(j-1)<-ident.(origin).(j); done;
  ident.(destination).(0)<-id;
;;

let rec aux_hanoi height origin destination other =
  if height=1
  then (
    movement origin destination;
  )
  else (
    aux_hanoi (height-1) origin other destination ; (* Move n-1 from A to B  *)
    movement origin destination ; (* Move the higher from A to C *)
    aux_hanoi (height-1) other destination origin ; (* Move n-1 from B to C *)
  ) ;
  draw();
  sleepf 0.015;

;;

let hanoi height origin destination other =
  nbr_of_disks.(origin) <- height;
  for i=0 to height-1 do
    ident.(origin).(i)<-i ; done ;
  
  close_graph ();
  open_graph (" 1000x600-0+0");
  draw () ;

  aux_hanoi height origin destination other;
;;

hanoi height 0 2 1;;
(* Rods : 0 - 1 - 2 *)
