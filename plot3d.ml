(*************************************************
==================================================
        Simple mesh ploter written in OCaml
        2016
        Etienne JACOB
=================================================*)
#load "graphics.cma";;

(******************* types ***********************)
type plotStyle = Full | Edge;;
type point3D = float*float*float;;
type mesh = { nVert : int; nTria : int; positions : float array array; triangles : int array array; };;
type cameraView = { phi : float; theta : float; zoomFactor : float;  pos_back : point3D; };;
type precomputation = float*float*float*float*float*float*float*float*int*int;;

(*************************************************)
(***             Default  Arguments            ***)
(*************************************************)
let filePath = "C:/Users/Etienne/Desktop/toscahires-asci/cat1.off";;


let lightDirection = ((-.1.0,0.5,0.0) : point3D);;
let backwardCameraPosition = ((-.4.,15.,-7.0) : point3D);;
let myCameraView = { phi = 2.0; theta = 1.40; zoomFactor = 10.0;  pos_back = backwardCameraPosition; }

let printStep = 10000;;
let windowSize = "750x750";;
let myStyle = Full;;

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
let computeConstants cameraview =
    let (p,t,g) = (cameraview.phi,cameraview.theta,cameraview.zoomFactor) in
        let (u,v,w) = (g*.sin(t)*.cos(p),g*.sin(t)*.sin(p),g*.cos(t)) in
            ((u,v,w,cos(p),cos(t),sin(p),sin(t),scal (u,v,w) (u,v,w),Graphics.size_x()/2,Graphics.size_y()/2)  : precomputation);;


(* Projection from space to the camera plane,
it finds coordinates on the camera plane *)
(* Then it computes the pixel position of a point
using the previous function*)
(* Uses precomputed things,
that are constant when the camera doesn't move *)
let screenProjectionFast ((x,y,z) : point3D) ((u,v,w,cp,ct,sp,st,sc,sz_x,sz_y) : precomputation) =
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
let normalOfFace ((x1,y1,z1) : point3D) ((x2,y2,z2) : point3D) =
    let (a,b,c) = cross (x1,y1,z1) (x2,y2,z2) in
        let aux = norm a b c in
            (a/.aux,b/.aux,c/.aux);;

(* Loads a triangle based OFF format mesh in arrays *)
let loadOffMesh filePath =
    let input_file = Scanf.Scanning.from_file filePath in
    Scanf.bscanf input_file "%s\n" (fun x -> ());
        let (nVert,nTria,_) = Scanf.bscanf input_file "%d %d %d " (fun x y z -> (x,y,z)) in
        let positions = Array.make_matrix nVert 3 0.0
        and triangles = Array.make_matrix nTria 3 0 in
            for i=0 to nVert - 1 do
                let (x_,y_,z_) = Scanf.bscanf input_file "%f %f %f " (fun x y z -> (x,y,z)) in
                    positions.(i).(0) <- x_;
                    positions.(i).(1) <- y_;
                    positions.(i).(2) <- z_;
            done;
            for i=0 to nTria - 1 do
                let (_,a_,b_,c_) = Scanf.bscanf input_file "%d %d %d %d " (fun n x y z -> (n,x,y,z)) in
                    triangles.(i).(0) <- a_;
                    triangles.(i).(1) <- b_;
                    triangles.(i).(2) <- c_;
            done;
            {   nVert = nVert;
                nTria = nTria;
                positions = positions;
                triangles = triangles; };;


(* Computes the gravity center of a triangle,
this is use to sort triangles by their distance from the camera *)
let centerOfFace mesh i =
    let (ind1,ind2,ind3) = (mesh.triangles.(i).(0),mesh.triangles.(i).(1),mesh.triangles.(i).(2))
    and vert k = (mesh.positions.(k).(0),mesh.positions.(k).(1),mesh.positions.(k).(2)) in
        let (x1,y1,z1) = vert ind1
        and (x2,y2,z2) = vert ind2
        and (x3,y3,z3) = vert ind3 in
            ((x1+.x2+.x3)/.3.0,(y1+.y2+.y3)/.3.0,(z1+.z2+.z3)/.3.0);;

(* Sorts triangles in order to plot print the background faces first *)
let distanceSort mesh ((x,y,z) : point3D) =
    let result = Array.make mesh.nTria (0.0,0) in
        for i=0 to mesh.nTria - 1 do
            let (xf,yf,zf) = centerOfFace mesh i in
                result.(i) <- ( -. norm (xf +. x) (yf +. y) (zf +. z), i);
        done;
        Array.sort compare result;
        result;;

(* Explicit name, second argument will be lightDirection *)
let set_color_from_normal ((vx1,vy1,vz1) : point3D) ((vx2,vy2,vz2) : point3D) ((cam_x,cam_y,cam_z) : point3D) =
    let aux1 = norm vx2 vy2 vz2 in
        let sgn = if (scal (cam_x,cam_y,cam_z) (vx1,vy1,vz1) > 0.0) then 1.0 else -.1.0 in
        let aux2 = scal (vx1,vy1,vz1) (vx2/.aux1,vy2/.aux1,vz2/.aux1)*.sgn in
            let scaled = int_of_float((aux2+.1.0)*.255.0/.2.0) in
                Graphics.set_color (Graphics.rgb (scaled/2) (255/4+3*scaled/4) (255/3+2*scaled/3));;

(* Plots a face corresponding to a triangle of the mesh *)
let plotTriangle mesh ind ((x,y,z) : point3D) cameraParameters (lightDirection : point3D) style=
    let arr = mesh.triangles.(ind)
    and poly_array = Array.make 3 (0,0) in
        for i=0 to 2 do
            let xv = mesh.positions.(arr.(i)).(0)
            and yv = mesh.positions.(arr.(i)).(1)
            and zv = mesh.positions.(arr.(i)).(2) in
                poly_array.(i) <- screenProjectionFast (xv+.x,yv+.y,zv+.z) cameraParameters;
        done;
        let diff i1 i2 j = mesh.positions.(arr.(i2)).(j) -. mesh.positions.(arr.(i1)).(j) in
            let normal = normalOfFace (diff 0 1 0,diff 0 1 1,diff 0 1 2) (diff 0 2 0,diff 0 2 1,diff 0 2 2)
            and (cx,cy,cz) = centerOfFace mesh ind in
                set_color_from_normal normal lightDirection (x+.cx,y+.cy,z+.cz);
                match style with
                | Full -> Graphics.fill_poly poly_array
                | Edge -> Graphics.draw_poly_line poly_array;;

(* Plots a mesh using its position,
the camera parameters
and the light direction *)
let plotMesh mesh cameraview (lightDirection : point3D) style =
    let cameraParameters = computeConstants cameraview
    and dSort = distanceSort mesh cameraview.pos_back in
        for i=0 to mesh.nTria - 1 do
            let ind = snd (dSort.(i)) in
                plotTriangle mesh ind cameraview.pos_back cameraParameters lightDirection style;
                if ((i mod printStep) = 1) then Graphics.synchronize();
        done;
        Graphics.synchronize();;

(*** creates a mesh from a R^2 -> R function,
on a rectangle domain with borders parallel to x or y axis ***)
let meshOfHeightMapRect f ((xmin,xmax) : float*float) ((ymin,ymax) : float*float) (step : float) =
    let n = int_of_float(ceil((xmax-.xmin)/.step))
    and m = int_of_float(ceil((ymax-.ymin)/.step)) in
        print_int m;
        let positions = Array.make_matrix ((n+1)*(m+1)) 3 0.0
        and triangles = Array.make_matrix (n*m*2) 3 0
        and indexOfPos = Array.make_matrix (n+1) (m+1) 0 in
            let curx = ref xmin
            and index = ref 0
            and curi = ref 0
            and curj = ref 0 in
            while (!curx <= xmax) do
                let cury = ref ymin in
                curj := 0;
                while (!cury <= ymax) do
                    positions.(!index).(0) <- !curx;
                    positions.(!index).(1) <- !cury;
                    positions.(!index).(2) <- f (!curx) (!cury);
                    indexOfPos.(!curi).(!curj) <- !index;
                    cury := step +. !cury;
                    curj := 1 + !curj;
                    index := !index + 1;
                done;
                curx := step +. !curx;
                curi := 1 + !curi;
            done;
            let nVert = !index in
            index := 0;
            for i=0 to !curi - 2 do
                for j=0 to !curj - 2 do
                    for t=0 to 1 do
                        triangles.(!index*2 + t).(0) <- indexOfPos.(i+t).(j+t);
                        triangles.(!index*2 + t).(1) <-  indexOfPos.(i+1-t).(j+t);
                        triangles.(!index*2 + t).(2) <-  indexOfPos.(i+t).(j+1-t);
                    done;
                    index := !index + 1;
                done
            done;
            let nTria = !index*2 in
            {nVert = nVert; nTria = nTria; positions = positions; triangles = triangles; };;