#use "3dplot.ml"
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
let myMesh = meshOfHeightMapRect f (-.50.0,100.0) (-20.0,55.0) 1.0;;
let (n,m) = (myMesh.nVert,myMesh.nTria) in
print_int n; print_string " vertices, ";
print_int m; print_endline " triangles.";;

(*** generating a mesh from the OFF file ***)
let myMesh2 = loadMesh filePath;;
let (n,m) = (myMesh2.nVert,myMesh2.nTria) in
print_int n; print_string " vertices, ";
print_int m; print_endline " triangles.";;

(*** putting the two meshes together ***)
let mySecondMesh = concat myMesh myMesh2;;

(*** plot ***)
print_string "Plot starts... ";;
Graphics.open_graph windowSize;;
Graphics.auto_synchronize false;;
plotMesh mySecondMesh myCameraView lightDirection myStyle;;
print_endline "done.";;

(******* Just to keep the graphics window open ******)
let last = Scanf.scanf "%d" (fun x->x);;
(****************************************************)
