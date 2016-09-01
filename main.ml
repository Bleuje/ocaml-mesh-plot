#use "plot3d.ml";;
#use "scalarmeshfunctions.ml";;

open Plot3D;;
open MeshFunctions;;

(***************************************************)
(********* I / Example of settings *****************)
(***************************************************)

(*-----------------------------------------------*)
(*** A / Define your own settings from scratch ***)
(*-----------------------------------------------*)
let cameraPosition = ((-.20.,-.100.,20.0) : vec3D);;
let myCameraView =
{ phi = 1.3;
theta = 1.2;
zoomfactor = 13.5;
cameraposition = cameraPosition; }

let myLightDirection = ((-.1.0,0.5,-.0.5) : vec3D);;
let myColor = ((0.5,0.1,1.0) : hsv_color) ;;

let myPrintStep = 10000;;
let myWindowSize = "750x750";;
let myStyle = Full;;
let myColorStyle = Outside;;

let myShaderColorFunction sc ((r,g,b) : rgb_color) =
    let darkness = 0.5
    and contrast = 2.8 in
        let aux x =
        int_of_float(min (max ((1.0 -. darkness)*.255.*.((1.0+.contrast*.darkness*.sc)*.x) +. darkness*.100.) 0.) 255.) in
            (aux r,aux g,aux b);;

let myEmptyShaderColorFunction sc ((r,g,b) : rgb_color) =
        let aux x =
        int_of_float(255.*.x) in
            (aux r,aux g,aux b);;

let mySettings = {cameraview = myCameraView;
colorchoice = HSV myColor;
style = myStyle;
lightdirection = myLightDirection;
windowsize = myWindowSize;
printstep = myPrintStep;
shaderRGB = myShaderColorFunction; };;

(*----------------------------------------------------*)
(*** B / Or use the default settings from plot3d.ml ***)
(*----------------------------------------------------*)
let mySettings2 = defaultSettings;;

let myField ((x,y,z) : vec3D) =
    (x/.50.,y/.50.,100.-.z/.50.);;

mySettings2.colorchoice <- SpaceColor myField;
mySettings2.windowsize <- "1000x1000";
mySettings2.cameraview.theta <- 1.0;
mySettings2.cameraview.zoomfactor <- 10.0;
mySettings2.style <- Full;;


(****************************************************)
(*** II / Demonstration *****************************)
(****************************************************)

(*** paraboloid example ***)
let f x y = 0.01*.(x*.x +. y*.y) +. 50.0;;

(*** generating a mesh from the previous function ***)
let myMesh = meshOfHeightMapRect f (-.50.0,100.0) (-20.0,155.0) 2.0;;
let (n,m) = (myMesh.nVert,myMesh.nTria) in
print_int n; print_string " vertices, ";
print_int m; print_endline " triangles.";;

(*** generating a mesh from the OFF file ***)
let filePath = "examples/cat1.off";;

let myCat = loadOffMesh filePath;;
let (n,m) = (myCat.nVert,myCat.nTria) in
print_int n; print_string " vertices, ";
print_int m; print_endline " triangles.";;

(*** adapt to your path ***)
let filePath2 = "examples/cat2.off";;

let myMesh2 = loadOffMesh filePath2;;
setColorFromValues (LinearCycleHSV (1,(1.0,1.0,1.0),(0.5,1.0,0.6))) (smoothenValues 3 bfsDepth_value) myMesh2;;
let (n,m) = (myMesh2.nVert,myMesh2.nTria) in
print_int n; print_string " vertices, ";
print_int m; print_endline " triangles.";;

(*** deformation function ***)
let g (x,y,z) = (1.5*.x,y,2.0*.z);;
let noise (x,y,z) = (x +. (-. 0.5 +. Random.float 1.),y +. (-. 0.5 +. Random.float 1.),z +. (-. 0.5 +. Random.float 1.));;

(*** putting meshes together ***)
let myFullMesh = concatMeshList [copyMesh myMesh; myCat;
deformedMesh (movedMesh myCat (50.0,0.0,0.0)) noise;
deformedMesh (movedMesh myCat (25.0,50.0,0.0)) g];;



(*** plot 1 ********************************)
let _ = setDefaultCameraView mySettings2 myMesh2;;

print_string "Plot starts... ";
Graphics.open_graph mySettings2.windowsize;
Graphics.auto_synchronize true;
changeCameraViewGUI 0.05 myMesh2 mySettings2;
plotMesh mySettings2 myMesh2;
print_endline "done.";;

(****************************************************)

(* Demo with concatenation of different meshes *) 

(*** final plot ********************************)
let _ = setDefaultCameraView mySettings myFullMesh;;

Graphics.clear_graph();
Graphics.auto_synchronize true;
print_string "Plot starts... ";
setColorFromValues (LinearHSV ((0.0,1.0,1.0),(0.5,1.0,0.6))) (smoothenValues 8 triangleArea_value) myFullMesh;
changeCameraViewGUI 0.02 myFullMesh mySettings;
plotMesh mySettings myFullMesh ;
print_endline "done.";;

(*** Saving result *****************************)
print_endline "Starting to write...";
writeOffMesh myFullMesh "cats_paraboloid.off";
print_endline "Finished writing.";;



(******* Just to keep the graphics window opened ****)
let last = Scanf.scanf "%d " (fun x->x);;
(****************************************************)
