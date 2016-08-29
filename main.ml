#use "mesh3dtools.ml";;
#use "plot3d.ml";;
(***************************************************)
(********* Settings example*************************)
(***************************************************)

(*** Define your own settings ***)
let cameraPositionOld = ((4.,-.110.,80.0) : point3D);;
let cameraPosition = ((4.,-.200.,8.0) : point3D);;
let myCameraView = { phi = 1.5; theta = 1.1; zoomFactor = 18.0;  cameraposition = cameraPosition; }

let myLightDirection = ((-.1.0,0.5,0.0) : point3D);;
let myColor = ((0.5,0.1,1.0) : hsv_color) ;;

let myPrintStep = 10000;;
let myWindowSize = "750x750";;
let myStyle = Full;;

let mySettings = {cameraview = myCameraView;
colorchoice = HSV myColor;
style = myStyle;
lightdirection = myLightDirection;
windowsize = myWindowSize;
printstep = myPrintStep; };;

(*** Or use the default settings from plot3d.ml ***)
defaultSettings.windowsize <- "1000x1000";;

let mySettings2 = defaultSettings;;
mySettings2.colorchoice <- RGB (0.6,0.9,1.2);;

(****************************************************)
(*** Demo *******************************************)
(****************************************************)
(*** paraboloid example ***)
let f x y = 0.01*.(x*.x +. y*.y) +. 50.0;;

(*** generating a mesh from the previous function ***)
let myMesh = meshOfHeightMapRect f (-.50.0,100.0) (-20.0,155.0) 10.0;;
let (n,m) = (myMesh.nVert,myMesh.nTria) in
print_int n; print_string " vertices, ";
print_int m; print_endline " triangles.";;

(*** generating a mesh from the OFF file ***)
let filePath = "C:/Users/Etienne/Desktop/toscahires-asci/cat1.off";;

let myMesh2 = loadOffMesh filePath;;
let (n,m) = (myMesh2.nVert,myMesh2.nTria) in
print_int n; print_string " vertices, ";
print_int m; print_endline " triangles.";;

(*** deformation function ***)
let g x y z = (1.5*.x,y,2.0*.z);;
let noise x y z = (x +. (-. 0.5 +. Random.float 1.),y +. (-. 0.5 +. Random.float 1.),z +. (-. 0.5 +. Random.float 1.));;

(*** putting meshes together ***)
let myFullMesh = concatMeshList [copyMesh myMesh; myMesh2;
deformedMesh (movedMesh myMesh2 (50.0,0.0,0.0)) noise;
deformedMesh (movedMesh myMesh2 (25.0,50.0,0.0)) g];;

(*** final plot ********************************)
print_string "Plot starts... ";
Graphics.open_graph mySettings2.windowsize;
Graphics.auto_synchronize false;
plotMesh myFullMesh mySettings2;
print_endline "done.";;

(*** Saving result *****************************)
print_endline "Starting to write...";
writeOffMesh myFullMesh "cats_paraboloid.off";
print_endline "Finished writing.";;

(******* Just to keep the graphics window open ******)
let last = Scanf.scanf "%d" (fun x->x);;
(****************************************************)
