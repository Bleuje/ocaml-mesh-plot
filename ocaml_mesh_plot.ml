(*************************************************
==================================================
        Simple mesh ploter written in OCaml
        Works on OFF format with triangles only
        2016
        Etienne JACOB
==================================================
**************************************************)
type plotStyle = Full | Edge;;
(*************************************************)
(***             Main  Arguments               ***)
(*************************************************)
let filePath = "C:/Users/Etienne/Desktop/toscahires-asci/centaur1.off";;

let lightDirection = (1.0,0.5,0.0);;
let (meshPosition_x,meshPosition_y,meshPosition_z) = (-.80.,150.,-70.0);;
let (sphericalPhi,sphericalTheta,zoom) = (2.0,1.40,15.0);;
let print_step = 10000;;
let windowSize = "750x750";;
let myStyle = Edge;;
(*************************************************)
#load "graphics.cma";;
open Graphics;;open Scanf;;

(*************************************************)
(********* 3D --> 2D projection functions ********)
(*************************************************)
(* Euclidean norm *)
let norm x y z=
    sqrt((x*.x)+.(y*.y)+.(z*.z));;

(* Scalar product *)
let scal (x1,y1,z1) (x2,y2,z2) =
    x1*.x2 +. y1*.y2 +. z1*.z2;;

(* Vectorial product *)
let cross (x1,y1,z1) (x2,y2,z2) =
    (y1*.z2 -. z1*.y2,
    z1*.x2 -. x1*.z2,
    x1*.y2 -. y1*.x2);;

(* some trigonometric functions an other things
can be called only once if precomputed *)
let computeCameraTrigo p t g =
    let (u,v,w) = (g*.sin(t)*.cos(p),g*.sin(t)*.sin(p),g*.cos(t)) in
        (u,v,w,cos(p),cos(t),sin(p),sin(t),scal (u,v,w) (u,v,w),size_x()/2,size_y()/2);;


(* Projection from space to the camera plane,
it finds coordinates on the camera plane *)
(* Then it computes the pixel position of a point
from its space position and from the camera parameters,
using the previous function*)
(* Uses precomputed things,
that are constant when the camera doesn't move *)
let screenProjectionFast x y z (u,v,w,cp,ct,sp,st,sc,sz_x,sz_y) =
    let par = sc/.(u*.x +. v*.y +. w*.z) in
        let i = x*.par -. u
        and j = y*.par -. v
        and k = z*.par -. w in
            if (par>0.) then begin
                let a = i*.cp*.ct +. j*.sp*.ct -. k*.st
                and b = j*.cp -. i*.sp in
                    (sz_x - int_of_float(float_of_int(sz_x)*.b)/10,sz_y - int_of_float(float_of_int(sz_y)*.a)/10)
            end
            else
                (0,0);;
    
(*************************************************)
(************** MESH FUNCTIONS *******************)
(*************************************************)

(* Computes normal of a face,
given two vectors from the edges *)
let normalOfFace (x1,y1,z1) (x2,y2,z2) =
    let (a,b,c) = cross (x1,y1,z1) (x2,y2,z2) in
        let aux = norm a b c in
            (a/.aux,b/.aux,c/.aux);;

(* Loads a mesh in arrays *)
let loadMesh filePath =
    let input_file = Scanning.from_file filePath in
    bscanf input_file "%s\n" (fun x -> ());
        let (nVert,nTria,_) = bscanf input_file "%d %d %d\n" (fun x y z -> (x,y,z)) in
        let positions = Array.make_matrix nVert 3 0.0
        and triangles = Array.make_matrix nTria 3 0 in
            for i=0 to nVert - 1 do
                let (x_,y_,z_) = bscanf input_file "%f %f %f\n" (fun x y z -> (x,y,z)) in
                    positions.(i).(0) <- x_;
                    positions.(i).(1) <- y_;
                    positions.(i).(2) <- z_;
            done;
            for i=0 to nTria - 1 do
                let (_,a_,b_,c_) = bscanf input_file "%d %d %d %d\n" (fun n x y z -> (n,x,y,z)) in
                    triangles.(i).(0) <- a_;
                    triangles.(i).(1) <- b_;
                    triangles.(i).(2) <- c_;
            done;
            (nVert,nTria,positions,triangles);;

(* Computes the gravity center of a triangle,
this is use to sort triangles by their distance from the camera *)
let centerOfFace (nVert,nTria,positions,triangles) i =
    let (ind1,ind2,ind3) = (triangles.(i).(0),triangles.(i).(1),triangles.(i).(2)) in
        let (x1,y1,z1) = (positions.(ind1).(0),positions.(ind1).(1),positions.(ind1).(2))
        and (x2,y2,z2) = (positions.(ind2).(0),positions.(ind2).(1),positions.(ind2).(2))
        and (x3,y3,z3) = (positions.(ind3).(0),positions.(ind3).(1),positions.(ind3).(2)) in
            ((x1+.x2+.x3)/.3.0,(y1+.y2+.y3)/.3.0,(z1+.z2+.z3)/.3.0);;

(* Sorts triangles in order to plot print the background faces first *)
let distanceSort (nVert,nTria,positions,triangles) x y z =
    let result = Array.make nTria (0.0,0) in
        for i=0 to nTria - 1 do
            let (xf,yf,zf) = centerOfFace (nVert,nTria,positions,triangles) i in
                result.(i) <- ( -. norm (xf +. x) (yf +. y) (zf +. z), i);
        done;
        Array.sort compare result;
        result;;

(* Explicit name, second argument is lightDirection *)
let set_color_from_normal (vx1,vy1,vz1) (vx2,vy2,vz2) =
    let aux1 = norm vx2 vy2 vz2 in
        let aux2 = scal (vx1,vy1,vz1) (vx2/.aux1,vy2/.aux1,vz2/.aux1) in
            let scaled = int_of_float((aux2+.1.0)*.255.0/.2.0) in
                set_color (rgb (scaled/2) (255/4+3*scaled/4) (255/3+2*scaled/3));;

(* Plots a face corresponding to a triangle of the mesh *)
let plotTriangle (nVert,nTria,positions,triangles) ind x y z cameraParameters lightDirection style=
    let arr = triangles.(ind)
    and poly_array = Array.make 3 (0,0) in
        for i=0 to 2 do
            let xv = positions.(arr.(i)).(0)
            and yv = positions.(arr.(i)).(1)
            and zv = positions.(arr.(i)).(2) in
                poly_array.(i) <- screenProjectionFast (xv+.x) (yv+.y) (zv+.z) cameraParameters;
        done;
        let diff i1 i2 j = positions.(arr.(i2)).(j) -. positions.(arr.(i1)).(j) in
            let normal = normalOfFace (diff 0 1 0,diff 0 1 1,diff 0 1 2) (diff 0 2 0,diff 0 2 1,diff 0 2 2) in
                set_color_from_normal normal lightDirection;
                match style with
                | Full -> fill_poly poly_array
                | Edge -> draw_poly_line poly_array;;

(* Plots a mesh using its position,
the camera parameters
and the light direction *)
let plotMesh (nVert,nTria,positions,triangles) (x,y,z,p,t,g) lightDirection style =
    let cameraParameters = computeCameraTrigo p t g
    and dSort = distanceSort (nVert,nTria,positions,triangles) x y z in
        for i=0 to nTria - 1 do
            let ind = snd (dSort.(i)) in
                plotTriangle (nVert,nTria,positions,triangles) ind x y z cameraParameters lightDirection style;
                if ((i mod print_step) = 1) then synchronize();
        done;
        synchronize();;

(***************************************************)
(******************** MAIN *************************)
(***************************************************)
print_endline ("Loading "^filePath^"...");;
let myMesh = loadMesh filePath;;
print_endline (filePath^" loaded.");;
let (n,m,_,_) = myMesh in
print_int n; print_string " vertices, ";
print_int m; print_endline " triangles.";;

let myPlotParameters = (meshPosition_x,meshPosition_y,meshPosition_z,sphericalPhi,sphericalTheta,zoom);;

print_string "Plot starts... ";;
open_graph windowSize;;
auto_synchronize false;;
plotMesh myMesh myPlotParameters  lightDirection myStyle;;
print_endline "done.";;

(******* Just to keep the graphics window open ******)
let last = Scanf.scanf "%d" (fun x->x);;
(****************************************************)S
