(*************************************************
==================================================
        Mesh processing/plot in OCaml
        2016
        Etienne JACOB
=================================================*)
#use "mathtools.ml"

type rgb_color = float*float*float;;

type colorStyle =
| Outside
| VertexColor of rgb_color array
| TriangleColor of rgb_color array;;

type mesh =
{ nVert : int;
nTria : int;
positions : vec3D array;
triangles : int array array;
mutable colorstyle : colorStyle; };;

type meshGraph =
{ n : int;
adj : int list array; };;

(*************************************************************)
(*** SIMPLE FUNCTIONS *****************************************)

(*** copy ***)
let copyMesh mesh =
    let (n,m) = (mesh.nVert,mesh.nTria) in
        let (positions,triangles) = (Array.make n (0.0,0.0,0.0),Array.make_matrix m 3 0) in
            for i=0 to n-1 do
                positions.(i) <- mesh.positions.(i);
            done;
            for i=0 to m-1 do
                for j=0 to 2 do
                    triangles.(i).(j) <- mesh.triangles.(i).(j);
                done
            done;
            let newcolorstyle = match mesh.colorstyle with
            | Outside -> Outside
            | VertexColor t -> VertexColor (Array.copy t)
            | TriangleColor t -> TriangleColor (Array.copy t) in
            {nVert = n; nTria = m; positions = positions; triangles = triangles; colorstyle = newcolorstyle; } ;;
            

(*** mesh concatenation ***)
let concatMeshList (l : mesh list) =
    let rec len l = match l with
    | [] -> (0,0,0);
    | t::q -> let (n,m,k) = len q in (n + t.nVert,m + t.nTria,k+1) in
    let (nVert,nTria,number) = len l in
        let positions = Array.make nVert (0.0,0.0,0.0)
        and triangles = Array.make_matrix nTria 3 0
        and previousLength = Array.make number 0
        and curl = ref l
        and sum = ref 0
        and i = ref 0 in
        for id = 0 to number - 1 do
            let mesh = List.hd (!curl) in
                for k=0 to mesh.nVert - 1 do
                    positions.(!i) <- mesh.positions.(k);
                    i := !i + 1;
                done;
                previousLength.(id) <- !sum;
                sum := !sum + mesh.nVert;
                curl := List.tl (!curl); 
        done;
        sum := 0;
        i := 0;
        curl := l;
        for id = 0 to number - 1 do
            let mesh = List.hd (!curl) in
                for k=0 to mesh.nTria - 1 do
                    triangles.(!i).(0) <- previousLength.(id) + mesh.triangles.(k).(0);
                    triangles.(!i).(1) <- previousLength.(id) + mesh.triangles.(k).(1);
                    triangles.(!i).(2) <- previousLength.(id) + mesh.triangles.(k).(2);
                    i := !i + 1;
                done;
                curl := List.tl (!curl); 
        done;
        {nVert = nVert; nTria = nTria; positions = positions; triangles = triangles; colorstyle = Outside; } ;;

let add3 ((x,y,z) : vec3D) ((a,b,c) : vec3D) =
    (x+.a,y+.b,z+.c);;

(*** Same mesh translated by a 3d vector ***)
let movedMesh mesh ((x,y,z) : vec3D) =
    let positions = Array.make mesh.nVert (0.0,0.0,0.0)
    and triangles = Array.make_matrix mesh.nTria 3 0 in
        for i=0 to mesh.nVert-1 do
            positions.(i) <- add3 mesh.positions.(i) (x,y,z);
        done;
        for i=0 to mesh.nTria-1 do
            triangles.(i) <- mesh.triangles.(i);
        done;
        {nVert = mesh.nVert; nTria = mesh.nTria; positions = positions; triangles = triangles; colorstyle = Outside; } ;;

(*** new mesh with a function applied to vertex positions ***)
let deformedMesh mesh f =
    let positions = Array.make mesh.nVert (0.0,0.0,0.0)
    and triangles = Array.make_matrix mesh.nTria 3 0 in
        for i=0 to mesh.nVert-1 do
            positions.(i) <- f mesh.positions.(i)
        done;
        for i=0 to mesh.nTria-1 do
            triangles.(i) <- mesh.triangles.(i);
        done;
        {nVert = mesh.nVert; nTria = mesh.nTria; positions = positions; triangles = triangles; colorstyle = Outside; } ;;

(*************************************************************)
(*** MESH GENERATION *****************************************)

(*** creates a mesh from a R^2 -> R function,
on a rectangle domain with borders parallel to x or y axis ***)
let meshOfHeightMapRect f ((xmin,xmax) : float*float) ((ymin,ymax) : float*float) (step : float) =
    let n = int_of_float(ceil((xmax-.xmin)/.step))
    and m = int_of_float(ceil((ymax-.ymin)/.step)) in
        let positions = Array.make ((n+1)*(m+1)) (0.0,0.0,0.0)
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
                    positions.(!index) <- (!curx,!cury,f (!curx) (!cury));
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
            {nVert = nVert; nTria = nTria; positions = positions; triangles = triangles; colorstyle = Outside; };;

(*** Graph from mesh with adjacency lists ***)
let graphOfMesh mesh =
    let try_add x l = if (List.mem x l) then l else x::l
    and result = Array.make (mesh.nVert) [] in
        for i=0 to mesh.nTria - 1 do
            let v j = mesh.triangles.(i).(j) in
            let (a,b,c) = (v 0,v 1,v 2) in
                result.(b) <- try_add a (result.(b));
                result.(c) <- try_add a (result.(c));
                result.(c) <- try_add b (result.(c));
                result.(a) <- try_add b (result.(a));
                result.(a) <- try_add c (result.(a));
                result.(b) <- try_add c (result.(b));
        done;
        { n = mesh.nVert; adj = result; };;

let vertexValuesFromTriangleValues v mesh =
    let result = Array.make (mesh.nVert) 0.
    and count = Array.make (mesh.nVert) 0 in
        let f i j = mesh.triangles.(i).(j) in
        for i=0 to mesh.nTria - 1 do
            for j = 0 to 2 do
                result.(f i j) <- result.(f i j) +. v.(i);
                count.(f i j) <- count.(f i j) + 1;
            done
        done;
        for i=0 to mesh.nVert - 1 do
            result.(i) <- result.(i)/.float_of_int(count.(i));
        done;
        result;;
        
let triangleValuesFromVertexValues v mesh =
    let result = Array.make (mesh.nTria) (0.,0.,0.) in
        let f i j = v.(mesh.triangles.(i).(j)) in
        for i=0 to mesh.nTria - 1 do
            result.(i) <- ((average3 (f i 0) (f i 1) (f i 2)) : rgb_color);
        done;
        result;;

(*************************************************************)
(*** MESH INPUT/OUTPUT ***************************************)

(* Loads a triangle based OFF format mesh in arrays *)
let loadOffMesh filePath =
    let input_file = Scanf.Scanning.from_file filePath in
    Scanf.bscanf input_file "%s\n" (fun x -> ());
        let (nVert,nTria,_) = Scanf.bscanf input_file "%d %d %d " (fun x y z -> (x,y,z)) in
        let positions = Array.make nVert (0.0,0.0,0.0)
        and triangles = Array.make_matrix nTria 3 0 in
            for i=0 to nVert - 1 do
                positions.(i) <- Scanf.bscanf input_file "%f %f %f " (fun x y z -> (x,y,z))
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
                triangles = triangles;
                colorstyle = Outside; };;

(* Writes a triangle based OFF format mesh in arrays *)
let writeOffMesh mesh_ filePath =
    let mesh = copyMesh mesh_ in
    (match mesh.colorstyle with
    | VertexColor t -> mesh.colorstyle <- TriangleColor (triangleValuesFromVertexValues t mesh);
    | _ -> (););
    let output_file = open_out filePath in
    Printf.fprintf output_file "%s\n" "OFF";
    Printf.fprintf output_file "%d %d 0\n" mesh.nVert mesh.nTria;
    for i=0 to mesh.nVert - 1 do
        let (x,y,z) = mesh.positions.(i) in
        Printf.fprintf output_file "%f %f %f\n" x y z;
    done;
    for i=0 to mesh.nTria - 1 do
        match mesh.colorstyle with
        | TriangleColor t -> let (r,g,b) = t.(i) in Printf.fprintf output_file "3 %d %d %d %f %f %f %f\n" mesh.triangles.(i).(0) mesh.triangles.(i).(1) mesh.triangles.(i).(2) r g b (1.0)
        | _ -> Printf.fprintf output_file "3 %d %d %d\n" mesh.triangles.(i).(0) mesh.triangles.(i).(1) mesh.triangles.(i).(2)
    done;
    close_out output_file;;