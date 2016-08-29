#use "mesh3dtools.ml";;
#use "plot3d.ml";;

(***************************************************)
(********* Example of settings *********************)
(***************************************************)

(*** A / Define your own settings from scratch ***)
let cameraPosition = ((4.,-.200.,8.0) : point3D);;
let myCameraView = { phi = 1.5; theta = 1.2; zoomfactor = 18.0;  cameraposition = cameraPosition; }

let myLightDirection = ((-.1.0,0.5,0.0) : point3D);;
let myColor = ((0.5,0.1,1.0) : hsv_color) ;;

let myPrintStep = 10000;;
let myWindowSize = "750x750";;
let myStyle = Full;;
let myColorStyle = Outside;;

let mySettings = {cameraview = myCameraView;
colorchoice = HSV myColor;
style = myStyle;
lightdirection = myLightDirection;
windowsize = myWindowSize;
printstep = myPrintStep; };;

(*** B / Or use the default settings from plot3d.ml ***)
let mySettings2 = defaultSettings;;

let myField ((x,y,z) : point3D) =
    (x/.50.,y/.50.,100.-.z/.50.);;

mySettings2.colorchoice <- SpaceColor myField;
mySettings2.windowsize <- "1000x1000";
mySettings2.cameraview.theta <- 1.0;
mySettings2.cameraview.zoomfactor <- 10.0;
mySettings2.cameraview.cameraposition <- (-.20.,-.60.,8.0);
mySettings2.style <- Full;;



(****************************************************)
(*** Demo *******************************************)
(****************************************************)

(*** paraboloid example ***)
let f x y = 0.01*.(x*.x +. y*.y) +. 50.0;;

(*** generating a mesh from the previous function ***)
let myMesh = meshOfHeightMapRect f (-.50.0,100.0) (-20.0,155.0) 2.0;;
let (n,m) = (myMesh.nVert,myMesh.nTria) in
print_int n; print_string " vertices, ";
print_int m; print_endline " triangles.";;

(*** generating a mesh from the OFF file ***)
let filePath = "C:/Users/Etienne/Desktop/toscahires-asci/cat1.off";;

let myCat = loadOffMesh filePath;;
let (n,m) = (myCat.nVert,myCat.nTria) in
print_int n; print_string " vertices, ";
print_int m; print_endline " triangles.";;

let filePath2 = "C:/Users/Etienne/Desktop/toscahires-asci/centaur1.off";;

let myMesh2 = loadOffMesh filePath2;;
applyValueArrayWithColors myMesh2 (Saturation (0.1,0.5)) (valueHeight myMesh2);;
let (n,m) = (myMesh2.nVert,myMesh2.nTria) in
print_int n; print_string " vertices, ";
print_int m; print_endline " triangles.";;

(*** deformation function ***)
let g x y z = (1.5*.x,y,2.0*.z);;
let noise x y z = (x +. (-. 0.5 +. Random.float 1.),y +. (-. 0.5 +. Random.float 1.),z +. (-. 0.5 +. Random.float 1.));;

(*** putting meshes together ***)
let myFullMesh = concatMeshList [copyMesh myMesh; myCat;
deformedMesh (movedMesh myCat (50.0,0.0,0.0)) noise;
deformedMesh (movedMesh myCat (25.0,50.0,0.0)) g];;


(*** plot 1 ********************************)
print_string "Plot starts... ";
Graphics.open_graph mySettings2.windowsize;
Graphics.auto_synchronize false;
plotMesh myMesh2 mySettings2;
print_endline "done.";;

(****************************************************)

(*
(*** final plot ********************************)
Graphics.clear_graph();;
print_string "Plot starts... ";
plotMesh myFullMesh mySettings;
print_endline "done.";;

(*** Saving result *****************************)
print_endline "Starting to write...";
writeOffMesh myFullMesh "cats_paraboloid.off";
print_endline "Finished writing.";;
*)

(******* Just to keep the graphics window open ******)
let last = Scanf.scanf "%d " (fun x->x);;
(****************************************************)
