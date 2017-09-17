#load "graphics.cma";;
open Graphics;;


(* vertex = (x, y, z) where x, y and z are tuples : the coordinates in the cartesian plan of the triangle sumit *)

let phi = 1.6180339887;;

(*vertex_calcul determine the point u which is on [xy] , phi/[xy] far from x ie. 1/[xy] far from y*)
(*psi is the angle between the horizontal and (xy) *)

let vertex_calcul x y =
  let d_xy = sqrt((x.(0) -. y.(0)) ** 2. +. (x.(1) -. y.(1)) ** 2.) in
  let d_xu = (1. /. phi) *. d_xy in
  let cos_psi = (y.(0) -. x.(0)) /. d_xy
  and sin_psi = (y.(1) -. x.(1)) /. d_xy in
  let ux = cos_psi *. d_xu
  and uy = sin_psi *. d_xu in
  let u = [|ux; uy|] in
  u;;

let draw points =
  open_graph " 800x600-0+0";
  let rec draw_triangle points = match points with
    |[] -> ()
    |t::q ->
       begin
	 moveto (int_of_float t.(0).(0)) (int_of_float t.(0).(1));
	 lineto (int_of_float t.(1).(0)) (int_of_float t.(1).(1));
	 lineto (int_of_float t.(2).(0)) (int_of_float t.(2).(1));
	 lineto (int_of_float t.(0).(0)) (int_of_float t.(0).(1));
	 draw_triangle q
       end
  in draw_triangle points
;;
let rec divide generation points typ triangle_verteces =
					  if generation = 0
					  then draw points
					  else begin
					    let x = triangle_verteces.(0)
					    and y = triangle_verteces.(1)
					    and z = triangle_verteces.(2) in
					    let u = vertex_calcul x y in
					    if typ = "obtuse" then
					         begin
						   divide (generation - 1)  ([|x; u; z|]::points) ("acute") ([|x; u; z|]);
						   divide (generation - 1)  ([|u; y; z|]::points) ("obtuse") ([|u; y; z|])
					         end
					    else let v = vertex_calcul y z in
						 begin
						   divide (generation - 1) ([|x; u; v|]::points) ("acute") ([|x; u; v|]);
						   divide (generation - 1) ([|x; v; z|]::points) ("acute") ([|x; v; z|]);
						   divide (generation - 1) ([|u; y; v|]::points) ("obtuse") ([|u; y; v|])
						 end
					    end

					      ;;

divide 4 [] "obtuse" [|[|0.; 0.|]; [|phi; 0.|]; [|phi/.2.; sqrt(1. +. (phi**2. /. 4.))|]|];;
