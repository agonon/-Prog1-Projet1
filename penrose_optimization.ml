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

let draw points =
  let rec draw_edges points =
    if points == [] then ()
    else let t = List.hd points and q = List.tl points in
      begin
        Unix.sleepf 0.1;
        moveto (int_of_float t.(0).(0)) (int_of_float t.(0).(1));
        lineto (int_of_float t.(1).(0)) (int_of_float t.(1).(1));
        draw_edges q
      end
  in draw_edges points
;;

(*the argurment "points" contains the only summits that are necessery to draw the triangles by linking these summits each other*)

let rec divide generation points triangle_type triangle_vertices =
  if generation = 0
  then
    begin
      draw points
    end
  else begin
    let x = triangle_vertices.(0)
    and y = triangle_vertices.(1)
    and z = triangle_vertices.(2) in
    let u = vertex_calcul x y in
    if triangle_type = "obtuse" then
      begin
        divide (generation - 1)  ([|z; u|]::points) ("acute") ([|z; x; u|]);
        divide (generation - 1)  (points) ("obtuse") ([|y; z; u|])
      end
    else let v = vertex_calcul y z in
      begin
        divide (generation - 1) ([|x; v|]::points) ("acute") ([|z; x; v|]);
        divide (generation - 1) ([|v; u|]::points) ("acute") ([|v; x; u|]);
        divide (generation - 1) (points) ("obtuse") ([|y; v; u|])
      end
  end
;;

(*///////////////////////////////////////////////////////////////////////////*)

let init_obtus ratio =
  let x = [|50.; 50.|] and y = [|50. +. ratio *. phi;50.|] in
  let z = [|y.(0) /. 2. +. 50.;50. +. sqrt((ratio) ** 2. -. (y.(0) /. 2.) ** 2.)|] in
  [|x; y; z|];;

let test generation ratio =
  open_graph " 800x600-0+0";
  let t = init_obtus ratio in
  divide generation ([[|t.(0);t.(1)|];[|t.(1);t.(2)|];[|t.(2);t.(0)|]]) "obtuse" (init_obtus ratio)
;;

test 5 500.;;
