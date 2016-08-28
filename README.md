# Basic mesh plot with Objective Caml
Shows meshes in OFF format, using the standard OCaml graphics library.
The code is contained in the file **ocaml_mesh_plot.ml**

## Use
 - The camera is located at position (0,0,0) and you can define the position of the mesh. The camera has 3 parameters : angles Phi and Theta, and a "zoom" factor.

- *loadMesh* loads an uncolored triangle-based OFF format mesh given the file path.

- *plotMesh* plots a mesh given its array description, the camera parameters and the light direction.
There are also two drawing styles : filled triangles or not.

- Comments in the code contain additional information to use the plot function.

## Examples of results
Examples of rendered meshes from the TOSCA dataset :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/cat1_ocaml.jpg)
![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/centaur1_ocaml.jpg)

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/centaur1_edge_ocaml.jpg)

## Possible future improvement
- Increase rendering speed with another library
- Colors defined by user
- Use more types or object oriented programming
- Compilation
