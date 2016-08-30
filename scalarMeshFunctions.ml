(*************************************************
==================================================
        Scalar mesh functions in OCaml
        2016
        Etienne JACOB
=================================================*)

(*===============================================*)
(* Same value on all vertices *)
let valueConstant (constant : float) mesh =
    Array.make (mesh.nVert) constant;;

(* Height *)
let valueHeight mesh =
    let result = Array.make (mesh.nVert) 0.0 in
        for i = 0 to mesh.nVert - 1 do
            result.(i) <- mesh.positions.(i).(2);
        done;
        result;;

(* DFS from vertex number k, puts the visit time on the vertex *)
let valueBFScount (k : int) mesh =
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
        result;;
let valueBFScount mesh = valueBFScount (Random.int mesh.nVert) mesh;;

(* BFS from vertex number k, puts the visit time on the vertex *)
let valueDFScount (k : int) mesh =
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
        result;;
let valueDFScount mesh = valueDFScount (Random.int mesh.nVert) mesh;;

(* BFS from vertex number k, puts the tree depth on each vertex *)
let valueBFSdepth (k : int) mesh =
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
        result;;
let valueBFSdepth mesh = valueBFSdepth (Random.int mesh.nVert) mesh;;

(* DFS from vertex number k, puts the tree depth on each vertex *)
let valueDFSdepth (k : int) mesh =
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
        result;;
let valueDFSdepth mesh = valueDFSdepth (Random.int mesh.nVert) mesh;;