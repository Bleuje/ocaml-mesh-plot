(*************************************************
==================================================
        Scalar functions on meshes in OCaml
        2016
        Etienne JACOB
=================================================*)

(*===============================================*)

(* Same value on all vertices *)
let constant_value (constant : float) mesh =
    VertexValues (Array.make (mesh.nVert) constant);;

(* Height *)
let height_value mesh =
    let result = Array.make (mesh.nVert) 0.0 in
        for i = 0 to mesh.nVert - 1 do
            result.(i) <- let (_,_,z) = mesh.positions.(i) in z;
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

(* Triangle area *)
let areaOfTriangle mesh ind =
        let (a,b,c) = (mesh.positions.(mesh.triangles.(ind).(0)),
        mesh.positions.(mesh.triangles.(ind).(1)),
        mesh.positions.(mesh.triangles.(ind).(2))) in
            area a b c;;

let triangleArea_value mesh =
    TriangleValues (Array.init (mesh.nTria) (areaOfTriangle mesh));;

(* Discrete gaussian curvature *)
let angleOfTriangle mesh i j =
    let (a,b,c) = (mesh.positions.(mesh.triangles.(i).((0 + j) mod 3)),
    mesh.positions.(mesh.triangles.(i).((1 + j) mod 3)),
    mesh.positions.(mesh.triangles.(i).((2 + j) mod 3))) in
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
        done;(* debugging?
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

(********************)
(*** SMOOTHING    ***)
(********************)
let smoothVertex max_iter v mesh =
    let graph = graphOfMesh mesh in
    for it = 0 to max_iter - 1 do
        let v2 = Array.make (mesh.nVert) 0. in
        for i = 0 to mesh.nVert - 1 do
            let meanList = i::(graph.adj.(i)) in
            List.iter (fun j -> (v2.(i) <- v2.(i) +. v.(j);)) meanList;
            v2.(i) <- v2.(i)/.float_of_int(List.length meanList);
        done;
        Array.iteri (fun i _ -> (v.(i) <- v2.(i))) v;
    done;
    VertexValues v;;

let smoothTriangle max_iter v mesh =
    smoothVertex max_iter (vertexValuesFromTriangleValues v mesh) mesh;;

(* Iterative smoothing using the mean value with neighbors,
in the mesh graph *)
let smoothenValues max_iter f mesh = 
    let res_ = f mesh in
        match res_ with
            | VertexValues v -> smoothVertex max_iter v mesh
            | TriangleValues v -> smoothTriangle max_iter v mesh;;
let smoothenValuesStep f mesh =
    smoothenValues 1 f mesh;;

(* Future plans : use cotangent weights and Laplace-Beltrami basis *)
            
            