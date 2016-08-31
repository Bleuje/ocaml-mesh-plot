(*************************************************
==================================================
        Mesh ploter written in OCaml
        2016
        Etienne JACOB
=================================================*)
#use "mesh3dtools.ml";;
#load "graphics.cma";;

(******************* types ***********************)
type plotStyle = Full | Edge;;

type cameraView =
{ mutable phi : float;
mutable theta : float;
mutable zoomfactor : float;
mutable cameraposition : point3D; };;

type precomputation = float*float*float*float*float*float*float*float*int*int;;

type hsv_color = float*float*float;;
type rgb_color = float*float*float;;

type gradientStyle =
| Hue of float*float
| Value of float*float
| Saturation of float*float
| HueCycles of int*float*float
| LinearRGB of rgb_color*rgb_color
| LinearHSV of hsv_color*hsv_color
| LinearCycleRGB of int*rgb_color*rgb_color
| LinearCycleHSV of int*hsv_color*hsv_color
| QuadraticRGB of rgb_color*rgb_color
| QuadraticHSV of hsv_color*hsv_color
| QuadraticCycleRGB of int*rgb_color*rgb_color
| QuadraticCycleHSV of int*hsv_color*hsv_color

type colorChoice = HSV of hsv_color | RGB of rgb_color | SpaceColor of (point3D -> rgb_color);;

type plotSettings =
{ mutable cameraview : cameraView;
mutable colorchoice : colorChoice;
mutable style : plotStyle;
mutable lightdirection : point3D;
mutable windowsize : string;
mutable printstep : int;
mutable shaderRGB : float -> rgb_color -> int*int*int; }

type values =
| VertexValues of float array
| PolygonValues of float array;;

(*************************************************)
(***             Default  Settings             ***)
(*************************************************)
let cameraPositionOld = ((4.,-.110.,80.0) : point3D);;
let cameraPosition = ((4.,-.200.,8.0) : point3D);;
let myCameraView = { phi = 1.5; theta = 1.1; zoomfactor = 18.0;  cameraposition = cameraPosition; }

let myLightDirection = ((-.1.0,0.5,0.0) : point3D);;
let myColor = ((0.5,0.1,1.0) : hsv_color) ;;

let myPrintStep = 10000;;
let myWindowSize = "750x750";;
let myStyle = Full;;
let myShaderColorFunction sc ((r,g,b) : rgb_color) =
    let coeff = 0.3 in
        let aux x =
        int_of_float(min (max ((1.0 -. coeff)*.255.*.((1.0+.coeff*.sc*.x)*.x) +. coeff*.122.) 0.) 255.) in
            (aux r,aux g,aux b);;

let defaultSettings = {cameraview = myCameraView;
colorchoice = HSV myColor;
style = myStyle;
lightdirection = myLightDirection;
windowsize = myWindowSize;
printstep = myPrintStep;
shaderRGB = myShaderColorFunction; };;

(*************************************************)
(********* 3D --> 2D projection functions ********)
(*************************************************)
(* Euclidean norm *)
let norm x y z =
    sqrt((x*.x)+.(y*.y)+.(z*.z));;
    
let norm3 (x,y,z) =
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
    let (p,t,g) = (cameraview.phi,cameraview.theta,cameraview.zoomfactor) in
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
                and b = j*.cp -. i*.sp
                and sz = min sz_x sz_y in
                    (sz_x - int_of_float(float_of_int(sz)*.b)/10,sz_y - int_of_float(float_of_int(sz)*.a)/10)
            end
            else
                (0,0);;

let hsv_to_rgb (h_,s_,v_) =
    let debug x = min (max x 0.0) 1.0 in
    let (h,s,v) = (h_,debug s_,debug v_) in
    if (s=0.0) then (v,v,v) else begin
        let i1 = int_of_float(h*.6.) in
        let i = i1 mod 6 in
        let f = (h*.6.)-. float_of_int(i1) in
        let (p,q,t) = v*.(1.-.s), v*.(1.-.s*.f), v*.(1.-.s*.(1.-.f)) in
        match i with
            | 0 -> (v, t, p)
            | 1 -> (q, v, p)
            | 2 -> (p, v, t)
            | 3 -> (p, q, v)
            | 4 -> (t, p, v)
            | _ -> (v, p, q)
    end;;

let average3 (r1,g1,b1) (r2,g2,b2) (r3,g3,b3) = ((r1+.r2+.r3)/.3.,(g1+.g2+.g3)/.3.,(b1+.b2+.b3)/.3.);;

(* Computes normal of a face,
given two vectors from the edges *)
let normalOfFace ((x1,y1,z1) : point3D) ((x2,y2,z2) : point3D) =
    let (a,b,c) = cross (x1,y1,z1) (x2,y2,z2) in
        let aux = norm a b c in
            (a/.aux,b/.aux,c/.aux);;

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
                result.(i) <- ( -. norm (xf -. x) (yf -. y) (zf -. z), i);
        done;
        Array.sort compare result;
        result;;

(* Explicit name, second argument will be lightDirection *)
let set_color_from_normal ((vx1,vy1,vz1) : point3D) ((vx2,vy2,vz2) : point3D) ((cam_x,cam_y,cam_z) : point3D) ((r_,g_,b_) : rgb_color) shader =
    let aux1 = norm vx2 vy2 vz2 in
        let sgn = if (scal (cam_x,cam_y,cam_z) (vx1,vy1,vz1) > 0.0) then 1.0 else -.1.0 in
        let sc = scal (vx1,vy1,vz1) (vx2/.aux1,vy2/.aux1,vz2/.aux1)*.sgn in
            let (r,g,b) = shader sc (r_,g_,b_) in
                Graphics.set_color (Graphics.rgb r g b);;

(* Plots a face corresponding to a triangle of the mesh *)
let plotTriangle mesh ind ((x,y,z) : point3D) cameraParameters (lightDirection : point3D) style colorChoice shadow=
    let debug x = min (max x 0.0) 1.0
    and arr = mesh.triangles.(ind)
    and poly_array = Array.make 3 (0,0) in
        for i=0 to 2 do
            let xv = mesh.positions.(arr.(i)).(0)
            and yv = mesh.positions.(arr.(i)).(1)
            and zv = mesh.positions.(arr.(i)).(2) in
                poly_array.(i) <- screenProjectionFast (xv-.x,yv-.y,zv-.z) cameraParameters;
        done;
        let diff i1 i2 j = mesh.positions.(arr.(i2)).(j) -. mesh.positions.(arr.(i1)).(j) in
            let normal = normalOfFace (diff 0 1 0,diff 0 1 1,diff 0 1 2) (diff 0 2 0,diff 0 2 1,diff 0 2 2)
            and (cx,cy,cz) = centerOfFace mesh ind in 
                let chosenColor =
                match mesh.colorstyle with
                | Outside ->
                    (match colorChoice with
                    | HSV c -> ((hsv_to_rgb c) : rgb_color)
                    | RGB (r,g,b) -> ((debug r,debug g,debug b) : rgb_color)
                    | SpaceColor field -> let (r_,g_,b_) = field (cx,cy,cz) in (debug r_,debug g_, debug b_))
                | VertexColor varr -> ((average3 varr.(arr.(0)) varr.(arr.(1)) varr.(arr.(2))) : rgb_color)
                | PolygonColor varr -> (varr.(ind) : rgb_color)
                in
                    set_color_from_normal normal lightDirection (cx-.x,cy-.y,cz-.z) chosenColor shadow;
                    match style with
                    | Full -> Graphics.fill_poly poly_array
                    | Edge -> Graphics.draw_poly_line [|poly_array.(0);poly_array.(1);poly_array.(2);poly_array.(0);|];;

(* Plots a mesh using its position,
the camera parameters
and the light direction *)
let plotMesh options mesh =
    let cameraParameters = computeConstants options.cameraview
    and dSort = distanceSort mesh options.cameraview.cameraposition in
        for i=0 to mesh.nTria - 1 do
            let ind = snd (dSort.(i)) in
                plotTriangle mesh ind options.cameraview.cameraposition cameraParameters options.lightdirection options.style options.colorchoice options.shaderRGB;
                if ((i mod options.printstep) = 1) then Graphics.synchronize();
        done;
        Graphics.synchronize();;

let triangle_cycle f = let i = int_of_float(f) in
    if (i mod 2 = 0) then f -. float_of_int(i)
    else 1.0 -. (f -. float_of_int(i));;

(* changes the color style according to values on vertices *)
let setColorFromValues gstyle f mesh =
    let varray_ = f mesh in
    let (n,varray) = match varray_ with
    | VertexValues a -> (mesh.nVert,a)
    | PolygonValues a -> (mesh.nTria,a) in
    let debug x = min (max x 0.0) 1.0
    and result = Array.make (n) (0.0,0.0,0.0)
    and epsilon = 0.00000001
    and min_ = ref 1000000000.0
    and max_ = ref (-.1000000000.0) in
    for i = 0 to n - 1 do
        min_ := min (!min_) varray.(i);
        max_ := max (!max_) varray.(i);
    done;
    for i = 0 to n - 1 do
        let current = (varray.(i) +. !min_)/.(!max_ -. !min_ +. epsilon) in
            match gstyle with
            | Hue (s,v) -> result.(i) <- (hsv_to_rgb (current,debug s,debug v) : rgb_color)
            
            | Value (h,s) -> result.(i) <- (hsv_to_rgb (debug h,debug s,current) : rgb_color)
            
            | Saturation (h,v) -> result.(i) <- (hsv_to_rgb (debug h,current,debug v) : rgb_color)
            
            | HueCycles (p,s,v) -> result.(i) <- (hsv_to_rgb (float_of_int(p)*.current,debug s,debug v) : rgb_color)
            
            | LinearRGB ((r1,g1,b1),(r2,g2,b2)) ->  result.(i) <- let pos y1 y2 = (1.0-.current)*.y1 +. current*.y2 in
                                                        (pos r1 r2, pos g1 g2, pos b1 b2)
                                                        
            | LinearHSV ((h1,s1,v1),(h2,s2,v2)) ->  result.(i) <- let pos y1 y2 = (1.0-.current)*.y1 +. current*.y2 in
                                                        hsv_to_rgb (pos h1 h2, pos s1 s2, pos v1 v2)
                                                        
            | LinearCycleRGB (p,(r1,g1,b1),(r2,g2,b2)) -> let current2 = triangle_cycle (float_of_int(p)*.current) in
                                                         result.(i) <- let pos y1 y2 = (1.0-.current2)*.y1 +. current2*.y2 in
                                                        (pos r1 r2, pos g1 g2, pos b1 b2)
                                                        
            | LinearCycleHSV (p,(h1,s1,v1),(h2,s2,v2)) ->  let current2 = triangle_cycle (float_of_int(p)*.current) in
                                                        result.(i) <- let pos y1 y2 = (1.0-.current2)*.y1 +. current2*.y2 in
                                                        hsv_to_rgb (pos h1 h2, pos s1 s2, pos v1 v2)
                                                        
            | QuadraticRGB ((r1,g1,b1),(r2,g2,b2)) ->  result.(i) <- let pos y1 y2 = (1.0-.current*.current)*.y1 +. current*.current*.y2 in
                                                        (pos r1 r2, pos g1 g2, pos b1 b2)
                                                        
            | QuadraticHSV ((h1,s1,v1),(h2,s2,v2)) ->  result.(i) <- let pos y1 y2 = (1.0-.current*.current)*.y1 +. current*.current*.y2 in
                                                        hsv_to_rgb (pos h1 h2, pos s1 s2, pos v1 v2)
                                                        
            | QuadraticCycleRGB (p,(r1,g1,b1),(r2,g2,b2)) -> let current2 = triangle_cycle (float_of_int(p)*.current) in
                                                         result.(i) <- let pos y1 y2 = (1.0-.current2)*.y1 +. current2*.y2 in
                                                        (pos r1 r2, pos g1 g2, pos b1 b2)
                                                        
            | QuadraticCycleHSV (p,(h1,s1,v1),(h2,s2,v2)) ->  let current2 = triangle_cycle (float_of_int(p)*.current) in
                                                        result.(i) <- let pos y1 y2 = (1.0-.current2*.current2)*.y1 +. current2*.current2*.y2 in
                                                        hsv_to_rgb (pos h1 h2, pos s1 s2, pos v1 v2)
    done;
    match varray_ with
    | VertexValues a -> mesh.colorstyle <- VertexColor result
    | PolygonValues a -> mesh.colorstyle <- PolygonColor result
;;