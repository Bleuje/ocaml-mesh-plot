(*************************************************
==================================================
        Mesh processing in OCaml
        2016
        Etienne JACOB
=================================================*)
#use "plot3d.ml";;

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
        {nVert = nVert; nTria = nTria; positions = positions; triangles = triangles; } ;;

let move mesh ((x,y,z) : point3D) =
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
        {nVert = mesh.nVert; nTria = mesh.nTria; positions = positions; triangles = triangles; } ;;

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
        {nVert = mesh.nVert; nTria = mesh.nTria; positions = positions; triangles = triangles; } ;;