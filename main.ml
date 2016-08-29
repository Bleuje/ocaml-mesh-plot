#use "plot3d.ml";;
#use "mesh3dtools.ml";;
(***************************************************)
(******************** MAIN *************************)
(***************************************************)

let filePath = "C:/Users/Etienne/Desktop/toscahires-asci/cat1.off";;

let lightDirection = ((-.1.0,0.5,0.0) : point3D);;
let backwardCameraPosition = ((-.4.,110.,-.80.0) : point3D);;
let myCameraView = { phi = 1.7; theta = 1.50; zoomFactor = 10.0;  pos_back = backwardCameraPosition; }
let printStep = 10000;;
let windowSize = "750x750";;
let myStyle = Full;;

(*** paraboloid example ***)
let f x y = 0.01*.(x*.x +. y*.y) +. 50.0;;

(*** generating a mesh from the previous function ***)
let myMesh = meshOfHeightMapRect f (-.50.0,100.0) (-20.0,155.0) 1.0;;
let (n,m) = (myMesh.nVert,myMesh.nTria) in
print_int n; print_string " vertices, ";
print_int m; print_endline " triangles.";;

(*** generating a mesh from the OFF file ***)
let myMesh2 = loadOffMesh filePath;;
let (n,m) = (myMesh2.nVert,myMesh2.nTria) in
print_int n; print_string " vertices, ";
print_int m; print_endline " triangles.";;

(*** deformation function ***)
let g x y z = (1.5*.x,y,2.0*.z);;

(*** putting meshes together ***)
let myFullMesh = concatMeshList [myMesh; myMesh2; move myMesh2 (50.0,0.0,0.0); deformedMesh (move myMesh2 (25.0,50.0,0.0)) g];;

(*** plot ***)
print_string "Plot starts... ";;
Graphics.open_graph windowSize;;
Graphics.auto_synchronize false;;
plotMesh myFullMesh myCameraView lightDirection myStyle;;
print_endline "done.";;

(******* Just to keep the graphics window open ******)
let last = Scanf.scanf "%d" (fun x->x);;
(****************************************************)
