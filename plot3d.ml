(*************************************************
==================================================
        Mesh plotter written in OCaml
        2016
        Etienne JACOB
=================================================*)
#use "mesh3dtools.ml";;
#load "graphics.cma";;

module Plot3D = struct

    include Mesh3D

    (******************* types ***********************)
    type plotStyle = Full | Edge
    
    type cameraView =
    { mutable phi : float;
    mutable theta : float;
    mutable zoomfactor : float;
    mutable cameraposition : vec3D; }
    
    type precomputation = float*float*float*float*float*float*float*float*int*int
    
    type hsv_color = float*float*float
    
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
    | Personal of (float -> rgb_color)
    
    type colorChoice = HSV of hsv_color | RGB of rgb_color | SpaceColor of (vec3D -> rgb_color)
    
    type plotSettings =
    { mutable cameraview : cameraView;
    mutable colorchoice : colorChoice;
    mutable style : plotStyle;
    mutable lightdirection : vec3D;
    mutable windowsize : string;
    mutable printstep : int;
    mutable shaderRGB : float -> rgb_color -> int*int*int; }
    
    type pointCloud =
    { n : int;
    cloud : vec3D array; }
    
    type values =
    | VertexValues of float array
    | TriangleValues of float array
    
    (*************************************************)
    (***             Default  Settings             ***)
    (*************************************************)
    let cameraPositionOld = ((4.,-.110.,80.0) : vec3D)
    let cameraPosition = ((4.,-.200.,8.0) : vec3D)
    let myCameraView =
    { phi = 1.5;
    theta = 1.1;
    zoomfactor = 18.0;
    cameraposition = cameraPosition; }
    
    let myLightDirection = ((-.1.0,0.5,0.0) : vec3D)
    let myColor = ((0.5,0.1,1.0) : hsv_color)
    
    let myPrintStep = 10000
    let myWindowSize = "750x750"
    let myStyle = Full
    let myShaderColorFunction sc ((r,g,b) : rgb_color) =
        let darkness = 0.5
        and contrast = 1.8 in
            let aux x =
            int_of_float(min (max ((1.0 -. darkness)*.255.*.((1.0+.contrast*.darkness*.sc)*.x) +. darkness*.100.) 0.) 255.) in
                (aux r,aux g,aux b)
    
    let defaultSettings = {cameraview = myCameraView;
    colorchoice = HSV myColor;
    style = myStyle;
    lightdirection = myLightDirection;
    windowsize = myWindowSize;
    printstep = myPrintStep;
    shaderRGB = myShaderColorFunction; }
    
    (*************************************************)
    (********* 3D --> 2D projection functions ********)
    (*************************************************)
    
    (* some trigonometric functions an other things
    can be called only once if precomputed *)
    let computeConstants cameraview =
        let (p,t,g) = (cameraview.phi,cameraview.theta,cameraview.zoomfactor) in
            let (u,v,w) = (g*.sin(t)*.cos(p),g*.sin(t)*.sin(p),g*.cos(t)) in
                ((u,v,w,cos(p),cos(t),sin(p),sin(t),scal3 (u,v,w) (u,v,w),Graphics.size_x()/2,Graphics.size_y()/2)  : precomputation)
    
    
    (* Projection from space to the camera plane,
    it finds coordinates on the camera plane *)
    (* Then it computes the pixel position of a point
    using the previous function*)
    (* Uses precomputed things,
    that are constant when the camera doesn't move *)
    let screenProjectionFast ((x,y,z) : vec3D) ((u,v,w,cp,ct,sp,st,sc,sz_x,sz_y) : precomputation) =
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
                    (0,0)
    
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
        end
    
    (* Computes normal of a face,
    given two vectors from the edges *)
    let normalOfFace ((x1,y1,z1) : vec3D) ((x2,y2,z2) : vec3D) =
        let (a,b,c) = cross (x1,y1,z1) (x2,y2,z2) in
            let aux = norm a b c in
                (a/.aux,b/.aux,c/.aux)
    
    (* Computes the gravity center of a triangle,
    this is use to sort triangles by their distance from the camera *)
    let centerOfFace mesh i =
        let (ind1,ind2,ind3) = (mesh.triangles.(i).(0),mesh.triangles.(i).(1),mesh.triangles.(i).(2)) in
            let (x1,y1,z1) = mesh.positions.(ind1)
            and (x2,y2,z2) = mesh.positions.(ind2)
            and (x3,y3,z3) = mesh.positions.(ind3) in
                ((x1+.x2+.x3)/.3.0,(y1+.y2+.y3)/.3.0,(z1+.z2+.z3)/.3.0)
    
    (* Sorts triangles in order to plot print the background faces first *)
    let distanceSort mesh ((x,y,z) : vec3D) =
        let result = Array.make mesh.nTria (0.0,0) in
            for i=0 to mesh.nTria - 1 do
                let (xf,yf,zf) = centerOfFace mesh i in
                    result.(i) <- ( -. norm (xf -. x) (yf -. y) (zf -. z), i);
            done;
            Array.sort compare result;
            result
            
    let distanceSortPointCloud pointCloud ((x,y,z) : vec3D) =
        let result = Array.make pointCloud.n (0.0,0) in
            for i=0 to pointCloud.n - 1 do
                let (xf,yf,zf) = pointCloud.cloud.(i) in
                    result.(i) <- ( -. norm (xf -. x) (yf -. y) (zf -. z), i);
            done;
            Array.sort compare result;
            result
    
    (* Explicit name, second argument will be lightDirection *)
    let set_color_from_normal ((vx1,vy1,vz1) : vec3D) ((vx2,vy2,vz2) : vec3D) ((cam_x,cam_y,cam_z) : vec3D) ((r_,g_,b_) : rgb_color) shader =
        let aux1 = norm vx2 vy2 vz2 in
            let sgn = if (scal3 (cam_x,cam_y,cam_z) (vx1,vy1,vz1) > 0.0) then 1.0 else -.1.0 in
            let sc = scal3 (vx1,vy1,vz1) (vx2/.aux1,vy2/.aux1,vz2/.aux1)*.sgn in
                let (r,g,b) = shader sc (r_,g_,b_) in
                    Graphics.set_color (Graphics.rgb r g b)
    
    let diff3 (x1,y1,z1) (x2,y2,z2) = (x2-.x1,y2-.y1,z2-.z1)
    
    (* Plots a face corresponding to a triangle of the mesh *)
    let plotTriangle mesh ind ((x,y,z) : vec3D) cameraParameters (lightDirection : vec3D) style colorChoice shadow=
        let debug x = min (max x 0.0) 1.0
        and arr = mesh.triangles.(ind)
        and poly_array = Array.make 3 (0,0) in
            for i=0 to 2 do
                let (xv,yv,zv) = mesh.positions.(arr.(i)) in
                    poly_array.(i) <- screenProjectionFast (xv-.x,yv-.y,zv-.z) cameraParameters;
            done;
            let diff i1 i2= diff3 mesh.positions.(arr.(i1)) mesh.positions.(arr.(i2)) in
                let normal = normalOfFace (diff 0 1) (diff 0 2)
                and (cx,cy,cz) = centerOfFace mesh ind in 
                    let chosenColor =
                    match mesh.colorstyle with
                    | Outside ->
                        (match colorChoice with
                        | HSV c -> ((hsv_to_rgb c) : rgb_color)
                        | RGB (r,g,b) -> ((debug r,debug g,debug b) : rgb_color)
                        | SpaceColor field -> let (r_,g_,b_) = field (cx,cy,cz) in (debug r_,debug g_, debug b_))
                    | VertexColor varr -> ((average3 varr.(arr.(0)) varr.(arr.(1)) varr.(arr.(2))) : rgb_color)
                    | TriangleColor varr -> (varr.(ind) : rgb_color)
                    in
                        set_color_from_normal normal lightDirection (cx-.x,cy-.y,cz-.z) chosenColor shadow;
                        match style with
                        | Full -> Graphics.fill_poly poly_array
                        | Edge -> Graphics.draw_poly_line [|poly_array.(0);poly_array.(1);poly_array.(2);poly_array.(0);|]
    
    (* Plots a face corresponding to a triangle of the mesh *)
    let plotPoint ((xv,yv,zv) : vec3D) ((x,y,z) : vec3D) cameraParameters psize =
        let point = (xv-.x,yv-.y,zv-.z) in
        let (i,j) = (screenProjectionFast point cameraParameters) in
            Graphics.fill_circle i j (max 1 (int_of_float(psize/.(norm3 point))))
    
    (* Plots a mesh using its position,
    the camera parameters
    and the light direction *)
    let plotMesh settings mesh =
        let cameraParameters = computeConstants settings.cameraview
        and dSort = distanceSort mesh settings.cameraview.cameraposition in
            for i=0 to mesh.nTria - 1 do
                let ind = snd (dSort.(i)) in
                    plotTriangle mesh ind settings.cameraview.cameraposition cameraParameters settings.lightdirection settings.style settings.colorchoice settings.shaderRGB;
                    if ((i mod settings.printstep) = 1) then Graphics.synchronize();
            done;
            Graphics.synchronize()
    
    (* chooses random points to create a point cloud from a mesh,
    part is the part of points that will be selected *)
    let createCloud part mesh =
        let nres = int_of_float(_debug(part)*.float_of_int(mesh.nVert)) in
            let cloud = Array.make nres (0.0,0.0,0.0)
            and t = Array.copy mesh.positions in
                for i = mesh.nVert - 1 downto 0 do
                    let k = Random.int (i+1) in
                        let aux = t.(k) in
                            t.(k) <- t.(i);
                            t.(i) <- aux;
                done;
                for i = 0 to nres - 1 do
                    cloud.(i) <- t.(i);
                done;
                { n = nres; cloud = cloud; }
    
    (* plots the point cloud *)
    let plotPointCould settings (pointCloud : pointCloud) psize=
    Graphics.clear_graph();
    Graphics.set_color Graphics.black;
        let cameraParameters = computeConstants settings.cameraview
        and dSort = distanceSortPointCloud pointCloud settings.cameraview.cameraposition in
            for i=0 to pointCloud.n - 1 do
                let ind = snd (dSort.(i)) in
                    plotPoint pointCloud.cloud.(ind) settings.cameraview.cameraposition cameraParameters psize;
            done;
            Graphics.synchronize()
    
    (* sets a default camera view to settings from mesh *)
    let setDefaultCameraView settings mesh =
            let mins = Array.make 3 _float_inf
            and maxs = Array.make 3 (-._float_inf) in
                for i = 0 to mesh.nVert - 1 do
                    let (x,y,z) = mesh.positions.(i) in
                        mins.(0) <- min mins.(0) x;
                        mins.(1) <- min mins.(1) y;
                        mins.(2) <- min mins.(2) z;
                        maxs.(0) <- max maxs.(0) x;
                        maxs.(1) <- max maxs.(1) y;
                        maxs.(2) <- max maxs.(2) z;
                done;
                let cameraPosition =    (((maxs.(0)+.mins.(0))/.2.,
                                        (maxs.(1)+.mins.(1))/.2. -. 3.*.(maxs.(1)-.mins.(1))/.2.,
                                        (maxs.(2)+.mins.(2))/.2.) : vec3D) in
                    let myCameraView =
                    { phi = _pi/.2.;
                    theta = _pi/.2.;
                    zoomfactor = 15.;
                    cameraposition = cameraPosition; } in
                        settings.cameraview <- myCameraView;
                        max (max (maxs.(0)-.mins.(0)) (maxs.(1)-.mins.(1))) (maxs.(2)-.mins.(2))
                
    (* graphical user interface to change the camera view *)
    let changeCameraViewGUI part mesh settings =
    Graphics.auto_synchronize false;
        let speed = ref ((setDefaultCameraView settings mesh)/.10.)
        and psize = ref 500.
        and astep = ref 0.05 in
        let myCloud = createCloud (_debug(part)) mesh in
            plotPointCould settings myCloud (!psize);
            let continue = ref true in
            while (!continue) do (*
                let status = Graphics.wait_next_event [Graphics.Key_pressed] in
                    if (status.keypressed) then begin *)
                    let c = Graphics.read_key() in
                        if (c='g' || c='+') then settings.cameraview.zoomfactor <- settings.cameraview.zoomfactor*.1.1
                        else begin if (c='h' || c='-') then settings.cameraview.zoomfactor <- settings.cameraview.zoomfactor/.1.1
                        else begin if (c='o' || c='l' || c='f') then continue := false
                        else begin if (c='w') then speed := !speed*.2.
                        else begin if (c='x') then speed := !speed/.2.
                        else begin if (c='b') then psize := !psize*.1.2
                        else begin if (c='n') then psize := !psize/.1.2
                        else begin if (c='c') then astep := !astep*.1.2
                        else begin if (c='v') then astep := !astep/.1.2
                        else begin if (c='8') then settings.cameraview.theta <- settings.cameraview.theta -. !astep
                        else begin if (c='2') then settings.cameraview.theta <- settings.cameraview.theta +. !astep
                        else begin if (c='4') then settings.cameraview.phi <- settings.cameraview.phi +. !astep
                        else begin if (c='6') then settings.cameraview.phi <- settings.cameraview.phi -. !astep
                        else begin if (c='5' || c='z') then let (theta,phi) = (settings.cameraview.theta,settings.cameraview.phi) in
                            settings.cameraview.cameraposition <-
                            add3 settings.cameraview.cameraposition (lambda3 (!speed) (sin(theta)*.cos(phi),sin(theta)*.sin(phi),cos(theta)))
                        else begin if (c='0' || c='s') then let (theta,phi) = (settings.cameraview.theta,settings.cameraview.phi) in
                            settings.cameraview.cameraposition <-
                            add3 settings.cameraview.cameraposition (lambda3 (-.(!speed)) (sin(theta)*.cos(phi),sin(theta)*.sin(phi),cos(theta)))
                        else begin if (c='1' || c='q') then let (theta,phi) = (settings.cameraview.theta,settings.cameraview.phi +. _pi/.2.) in
                            settings.cameraview.cameraposition <-
                            add3 settings.cameraview.cameraposition (lambda3 (!speed) (sin(theta)*.cos(phi),sin(theta)*.sin(phi),cos(theta)))
                        else begin if (c='3' || c='d') then let (theta,phi) = (settings.cameraview.theta,settings.cameraview.phi -. _pi/.2.) in
                            settings.cameraview.cameraposition <-
                            add3 settings.cameraview.cameraposition (lambda3 (!speed) (sin(theta)*.cos(phi),sin(theta)*.sin(phi),cos(theta)))
                        else begin if (c='7') then let (theta,phi) = (settings.cameraview.theta -. _pi/.2.,settings.cameraview.phi) in
                            settings.cameraview.cameraposition <-
                            add3 settings.cameraview.cameraposition (lambda3 (!speed) (sin(theta)*.cos(phi),sin(theta)*.sin(phi),cos(theta)))
                        else begin if (c='9') then let (theta,phi) = (settings.cameraview.theta +. _pi/.2.,settings.cameraview.phi) in
                            settings.cameraview.cameraposition <-
                            add3 settings.cameraview.cameraposition (lambda3 (!speed) (sin(theta)*.cos(phi),sin(theta)*.sin(phi),cos(theta)))
                        else begin if (c='r') then let _ = setDefaultCameraView settings mesh in ()
                        end end end end end end
                        end end end end end end end end end;
                        end end end end;
                        plotPointCould settings myCloud (!psize); 
            done;
            Graphics.clear_graph()
    
    (* perdiodical continuous function with values between 0.  and 1. *)
    let triangle_cycle f = let i = int_of_float(f) in
        if (i mod 2 = 0) then f -. float_of_int(i)
        else 1.0 -. (f -. float_of_int(i))
    
    (* changes the color style according to values on vertices *)
    let setColorFromValues gstyle f mesh =
        let varray_ = f mesh in
        let (n,varray) = match varray_ with
        | VertexValues a -> (mesh.nVert,a)
        | TriangleValues a -> (mesh.nTria,a) in
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
                result.(i) <-  match gstyle with
                | Hue (s,v) -> (hsv_to_rgb (current,debug s,debug v) : rgb_color)
                
                | Value (h,s) -> (hsv_to_rgb (debug h,debug s,current) : rgb_color)
                
                | Saturation (h,v) -> (hsv_to_rgb (debug h,current,debug v) : rgb_color)
                
                | HueCycles (p,s,v) -> (hsv_to_rgb (float_of_int(p)*.current,debug s,debug v) : rgb_color)
                
                | LinearRGB ((r1,g1,b1),(r2,g2,b2)) ->  let pos y1 y2 = (1.0-.current)*.y1 +. current*.y2 in
                                                            (pos r1 r2, pos g1 g2, pos b1 b2)
                                                            
                | LinearHSV ((h1,s1,v1),(h2,s2,v2)) ->  let pos y1 y2 = (1.0-.current)*.y1 +. current*.y2 in
                                                            hsv_to_rgb (pos h1 h2, pos s1 s2, pos v1 v2)
                                                            
                | LinearCycleRGB (p,(r1,g1,b1),(r2,g2,b2)) -> let current2 = triangle_cycle (float_of_int(p)*.current) in
                                                             let pos y1 y2 = (1.0-.current2)*.y1 +. current2*.y2 in
                                                            (pos r1 r2, pos g1 g2, pos b1 b2)
                                                            
                | LinearCycleHSV (p,(h1,s1,v1),(h2,s2,v2)) ->  let current2 = triangle_cycle (float_of_int(p)*.current) in
                                                            let pos y1 y2 = (1.0-.current2)*.y1 +. current2*.y2 in
                                                            hsv_to_rgb (pos h1 h2, pos s1 s2, pos v1 v2)
                                                            
                | QuadraticRGB ((r1,g1,b1),(r2,g2,b2)) ->  let pos y1 y2 = (1.0-.current*.current)*.y1 +. current*.current*.y2 in
                                                            (pos r1 r2, pos g1 g2, pos b1 b2)
                                                            
                | QuadraticHSV ((h1,s1,v1),(h2,s2,v2)) ->  let pos y1 y2 = (1.0-.current*.current)*.y1 +. current*.current*.y2 in
                                                            hsv_to_rgb (pos h1 h2, pos s1 s2, pos v1 v2)
                                                            
                | QuadraticCycleRGB (p,(r1,g1,b1),(r2,g2,b2)) -> let current2 = triangle_cycle (float_of_int(p)*.current) in
                                                             let pos y1 y2 = (1.0-.current2)*.y1 +. current2*.y2 in
                                                            (pos r1 r2, pos g1 g2, pos b1 b2)
                                                            
                | QuadraticCycleHSV (p,(h1,s1,v1),(h2,s2,v2)) ->  let current2 = triangle_cycle (float_of_int(p)*.current) in
                                                            let pos y1 y2 = (1.0-.current2*.current2)*.y1 +. current2*.current2*.y2 in
                                                            hsv_to_rgb (pos h1 h2, pos s1 s2, pos v1 v2)
                
                | Personal f -> f current
        done;
        match varray_ with
        | VertexValues _ -> mesh.colorstyle <- VertexColor result
        | TriangleValues _ -> mesh.colorstyle <- TriangleColor result

end;;