#load "graphics.cma";;
#load "unix.cma";;

open Unix;;
open Graphics;;

let phi = (1. +. sqrt(5.))/.2.;;

let vertex_calcul x y =
  let d_xy = sqrt((x.(0) -. y.(0)) ** 2. +. (x.(1) -. y.(1)) ** 2.) in
  let d_xu = (1. /. phi) *. d_xy in
  let cos_psi = (y.(0) -. x.(0)) /. d_xy
  and sin_psi = (y.(1) -. x.(1)) /. d_xy in
  let ux = cos_psi *. d_xu +. x.(0)
  and uy = sin_psi *. d_xu +. x.(1) in
  let u = [|ux; uy|] in
  u;;

let wait dt =
  let t_init = Unix.gettimeofday () in
  let rec aux dt =
    if Unix.gettimeofday () -. t_init < dt then aux dt
  in aux dt;;

let draw points =
  let rec draw_edges points =
    if points == [] then ()
    else let t = List.hd points and q = List.tl points in
      begin
        moveto (int_of_float t.(0).(0)) (int_of_float t.(0).(1));
        lineto (int_of_float t.(1).(0)) (int_of_float t.(1).(1));
        draw_edges q
      end
  in draw_edges points
;;


let width_division generation triangles type_init points =
  let rec divide_current_gen generation triangles type_triangles points next_gen type_next_gen points_next_gen=
    if generation == 0 then
      begin
        wait 1.;
        draw points
      end
    else if triangles == [] then
      begin
        wait 1.;
        draw points;
        divide_current_gen (generation - 1) next_gen type_next_gen points_next_gen [] [] [];
      end
    else let h = List.hd triangles and t = List.tl triangles
      and a = List.hd type_triangles and b = List.tl type_triangles in
      begin
        let x = h.(0)
        and y = h.(1)
        and z = h.(2) in
        let u = vertex_calcul x y in
        if a = "obtuse" then
          begin
            let new_triangles = [[|y;z;u|]; [|z; x; u|]]
            and new_types = ["obtuse"; "acute"]
            and new_points = [|z;u|] in
            divide_current_gen (generation) t b points (new_triangles@next_gen) (new_types@type_next_gen) (new_points::points_next_gen)
          end
        else
          let v = vertex_calcul y z in
          begin
            let new_triangles = [[|z; x; v|]; [|v; x ;u|]; [|y; v; u|]]
            and new_types = ["acute"; "acute"; "obtuse"]
            and new_points = [[|x; v|]; [|v; u|]] in
            divide_current_gen (generation) t b points (new_triangles@next_gen) (new_types@type_next_gen) (new_points@points_next_gen)
          end
      end
  in divide_current_gen generation triangles [type_init] points [] [] []
;;

(*///////////////////////////////////////////////////////////////////////////*)

let init_obtus ratio =
  let x = [|50.; 50.|] and y = [|50. +. ratio *. phi;50.|] in
  let z = [|y.(0) /. 2. +. 50.;50. +. sqrt((ratio) ** 2. -. (y.(0) /. 2.) ** 2.)|] in
  [|x; y; z|];;

let test generation ratio =
  open_graph " 800x600-0+0";
  let t = init_obtus(ratio) in
  width_division generation [t] "obtuse" ([[|t.(0); t.(1)|]; [|t.(1); t.(2)|]; [|t.(2); t.(0)|]]);;

test 5 500.;;
