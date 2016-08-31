(*************************************************
==================================================
        Scalar mesh functions in OCaml
        2016
        Etienne JACOB
=================================================*)

(*===============================================*)

let diff3 (x1,y1,z1) (x2,y2,z2) = (x2-.x1,y2-.y1,z2-.z1);;

let fabs x = max (-.x) x;;

(* Same value on all vertices *)
let constant_value (constant : float) mesh =
    VertexValues (Array.make (mesh.nVert) constant);;

(* Height *)
let height_value mesh =
    let result = Array.make (mesh.nVert) 0.0 in
        for i = 0 to mesh.nVert - 1 do
            result.(i) <- mesh.positions.(i).(2);
        done;
        VertexValues result;;

(* DFS from vertex number k, puts the visit time on the vertex *)
let bfsCount_value (k : int) mesh =
    let graph = graphOfMesh mesh
    and result = Array.make (mesh.nVert) 0.0
    and visited = Array.make (mesh.nVert) false
    and frontier = Queue.create()
    and curNb = ref 0.0 in
        Queue.push k frontier;
        result.(k) <- !curNb;
        visited.(k) <- true;
        while(Queue.length frontier > 0) do
            let ind = Queue.pop frontier in
                List.iter (fun x -> if not visited.(x) then
                                    (Queue.add x frontier;
                                    visited.(x) <- true;)) (graph.adj.(ind));
                result.(ind) <- !curNb;
                curNb := !curNb +. 1.0;
        done;
        VertexValues result;;
let bfsCount_value mesh = bfsCount_value (Random.int mesh.nVert) mesh;;

(* BFS from vertex number k, puts the visit time on the vertex *)
let dfsCount_value (k : int) mesh =
    let graph = graphOfMesh mesh
    and result = Array.make (mesh.nVert) 0.0
    and visited = Array.make (mesh.nVert) false
    and frontier = Stack.create()
    and curNb = ref 0.0 in
        Stack.push k frontier;
        result.(k) <- !curNb;
        visited.(k) <- true;
        while(Stack.length frontier > 0) do
            let ind = Stack.pop frontier in
                List.iter (fun x -> if not visited.(x) then
                                    (Stack.push x frontier;
                                    visited.(x) <- true;)) (graph.adj.(ind));
                result.(ind) <- !curNb;
                curNb := !curNb +. 1.0;
        done;
        VertexValues result;;
let dfsCount_value mesh = dfsCount_value (Random.int mesh.nVert) mesh;;

(* BFS from vertex number k, puts the tree depth on each vertex *)
let bfsDepth_value (k : int) mesh =
    let graph = graphOfMesh mesh
    and result = Array.make (mesh.nVert) 0.0
    and visited = Array.make (mesh.nVert) false
    and frontier = Queue.create() in
        Queue.push (k,0) frontier;
        visited.(k) <- true;
        while(Queue.length frontier > 0) do
            let (ind,cur_depth) = Queue.pop frontier in
                List.iter (fun x -> if not visited.(x) then
                                    (Queue.add (x,cur_depth+1) frontier;
                                    visited.(x) <- true;)) (graph.adj.(ind));
                result.(ind) <- float_of_int(cur_depth);
        done;
        VertexValues result;;
let bfsDepth_value mesh = bfsDepth_value (Random.int mesh.nVert) mesh;;

(* DFS from vertex number k, puts the tree depth on each vertex *)
let dfsDepth_value (k : int) mesh =
    let graph = graphOfMesh mesh
    and result = Array.make (mesh.nVert) 0.0
    and visited = Array.make (mesh.nVert) false
    and frontier = Stack.create() in
        Stack.push (k,0) frontier;
        visited.(k) <- true;
        while(Stack.length frontier > 0) do
            let (ind,cur_depth) = Stack.pop frontier in
                List.iter (fun x -> if not visited.(x) then
                                    (Stack.push (x,cur_depth+1) frontier;
                                    visited.(x) <- true;)) (graph.adj.(ind));
                result.(ind) <- float_of_int(cur_depth);
        done;
        VertexValues result;;
let dfsDepth_value mesh = dfsDepth_value (Random.int mesh.nVert) mesh;;

let _pi = 4.*.atan(1.0);;

(* Triangle area *)
let areaOfTriangle mesh ind =
    let point3dOfPositionArray i =
        ((mesh.positions.(i).(0),mesh.positions.(i).(1),mesh.positions.(i).(2)) : point3D) in
        let (a,b,c) = (point3dOfPositionArray mesh.triangles.(ind).(0),
        point3dOfPositionArray mesh.triangles.(ind).(1),
        point3dOfPositionArray mesh.triangles.(ind).(2)) in
            let (ab,ac) = (diff3 a b,diff3 a c) in
                0.5*.norm3 (cross ab ac);;

let triangleArea_value mesh =
    PolygonValues (Array.init (mesh.nTria) (areaOfTriangle mesh));;

(* Discrete gaussian curvature *)
let angleOfTriangle mesh i j =
    let point3dOfPositionArray i =
        ((mesh.positions.(i).(0),mesh.positions.(i).(1),mesh.positions.(i).(2)) : point3D) in
        let (a,b,c) = (point3dOfPositionArray mesh.triangles.(i).((0 + j) mod 3),
        point3dOfPositionArray mesh.triangles.(i).((1 + j) mod 3),
        point3dOfPositionArray mesh.triangles.(i).((2 + j) mod 3)) in
            let (ab,ac) = (diff3 a b,diff3 a c) in
                let cosine = (scal ab ac)/.(norm3 (ab)*.norm3 (ac)) in
                    acos(cosine);;

(* Discrete gaussian curvature : abs_max controls the maximum absolute value of values,
to prevent from bugs coming from  incoherent high or low values that
ruin the color gradient later on. *)
let discreteGaussianCurvature_value abs_max mesh =
    let result = Array.make (mesh.nVert) (2.0*._pi) in
        for i = 0 to mesh.nTria - 1 do
            for j = 0 to 2 do
                let ind = mesh.triangles.(i).(j) in
                    result.(ind) <- result.(ind) -. angleOfTriangle mesh i j;
            done
        done;(*
        let min_ = ref 100.
        and max_ = ref (-.100.) in*)
        for i = 0 to mesh.nVert - 1 do(*
            min_ := min (!min_) result.(i);
            max_ := max (!max_) result.(i);*)
            result.(i) <- min (max result.(i) (-.abs_max)) (abs_max);
            (*if (((Random.int 12345) mod 123) = 0) then (print_float result.(i); print_newline();)*)
        done;(*
        print_float (!min_);
        print_newline();
        print_float (!max_);
        print_newline();*)
        VertexValues result;;
            
            