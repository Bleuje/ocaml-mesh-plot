#use "plot3d.ml";;
open Plot3D;;

let mySettings = Plot3D.defaultSettings;;
let myMesh = loadOffMesh "examples/centaur1.off";;

Graphics.open_graph mySettings.windowsize;
changeCameraViewGUI 0.05 myMesh mySettings;
plotMesh mySettings myMesh;;

(******* Just to keep the graphics window opened ****)
let last = Scanf.scanf "%d " (fun x->x);;
(****************************************************)