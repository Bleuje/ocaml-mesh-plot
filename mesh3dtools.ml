(*************************************************
==================================================
        Mesh processing in OCaml
        2016
        Etienne JACOB
=================================================*)
type point3D = float*float*float;;

type rgb_color = float*float*float;;

type colorStyle =
| Outside
| VertexColor of rgb_color array
| PolygonColor of rgb_color array;;

type mesh =
{ nVert : int;
nTria : int;
positions : float array array;
triangles : int array array;
mutable colorstyle : colorStyle; };;

type meshGraph =
{ n : int;
adj : int list array; }

(*************************************************************)
(*** MESH INPUT/OUTPUT ***************************************)

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
                triangles = triangles;
                colorstyle = Outside; };;

(* Loads a triangle based OFF format mesh in arrays *)
let writeOffMesh mesh filePath =
    let output_file = open_out filePath in
    Printf.fprintf output_file "%s\n" "OFF";
    Printf.fprintf output_file "%d %d 0\n" mesh.nVert mesh.nTria;
    for i=0 to mesh.nVert - 1 do
        Printf.fprintf output_file "%f %f %f\n" mesh.positions.(i).(0) mesh.positions.(i).(1) mesh.positions.(i).(2);
    done;
    for i=0 to mesh.nTria - 1 do
        Printf.fprintf output_file "3 %d %d %d\n" mesh.triangles.(i).(0) mesh.triangles.(i).(1) mesh.triangles.(i).(2);
    done;
    close_out output_file;;

(*************************************************************)
(*** SIMPLE FUNCTIONS *****************************************)

(*** copy ***)
let copyMesh mesh =
    let (n,m) = (mesh.nVert,mesh.nTria) in
        let (positions,triangles) = (Array.make_matrix n 3 0.0,Array.make_matrix m 3 0) in
            for i=0 to n-1 do
                for j=0 to 2 do
                    positions.(i).(j) <- mesh.positions.(i).(j);
                done
            done;
            for i=0 to m-1 do
                for j=0 to 2 do
                    triangles.(i).(j) <- mesh.triangles.(i).(j);
                done
            done;
            {nVert = n; nTria = m; positions = positions; triangles = triangles; colorstyle = Outside; } ;;
            

(*** mesh concatenation ***)
let concatMeshList (l : mesh list) =
    let rec len l = match l with
    | [] -> (0,0,0);
    | t::q -> let (n,m,k) = len q in (n + t.nVert,m + t.nTria,k+1) in
    let (nVert,nTria,number) = len l in
        let positions = Array.make_matrix nVert 3 0.0
        and triangles = Array.make_matrix nTria 3 0
        and previousLength = Array.make number 0
        and curl = ref l
        and sum = ref 0
        and i = ref 0 in
        for id = 0 to number - 1 do
            let mesh = List.hd (!curl) in
                for k=0 to mesh.nVert - 1 do
                    positions.(!i).(0) <- mesh.positions.(k).(0);
                    positions.(!i).(1) <- mesh.positions.(k).(1);
                    positions.(!i).(2) <- mesh.positions.(k).(2);
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

(*** Same mesh translated by a 3d vector ***)
let movedMesh mesh ((x,y,z) : point3D) =
    let positions = Array.make_matrix mesh.nVert 3 0.0
    and triangles = Array.make_matrix mesh.nTria 3 0 in
        for i=0 to mesh.nVert-1 do
            positions.(i).(0) <- x +. mesh.positions.(i).(0);
            positions.(i).(1) <- y +. mesh.positions.(i).(1);
            positions.(i).(2) <- z +. mesh.positions.(i).(2);
        done;
        for i=0 to mesh.nTria-1 do
            triangles.(i) <- mesh.triangles.(i);
        done;
        {nVert = mesh.nVert; nTria = mesh.nTria; positions = positions; triangles = triangles; colorstyle = Outside; } ;;

(*** new mesh with a function applied to vertex positions ***)
let deformedMesh mesh f =
    let positions = Array.make_matrix mesh.nVert 3 0.0
    and triangles = Array.make_matrix mesh.nTria 3 0 in
        for i=0 to mesh.nVert-1 do
            let (x_,y_,z_) = f mesh.positions.(i).(0) mesh.positions.(i).(1) mesh.positions.(i).(2) in
                positions.(i).(0) <- x_;
                positions.(i).(1) <- y_;
                positions.(i).(2) <- z_;
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
