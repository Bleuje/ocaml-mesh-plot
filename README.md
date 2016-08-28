# Basic mesh plot with Objective Caml
Shows meshes in OFF format, and R^2 -> R functions on a rectangular domain.
It uses the standard OCaml graphics library (it's slow).
The code is contained in the file **3dplot.ml** and used in **main.ml**.

## How to use the code
- to use the functions in *3dplot.ml*, use *"#use "3dplot.ml";;"*

- The camera is located at position (0,0,0) and you can define the position of the mesh. The camera has 3 parameters : angles Phi and Theta, and a "zoom" factor.

- *loadMesh* loads an uncolored triangle-based OFF format mesh given the file path.

- *plotMesh* plots a mesh given its array description, the camera parameters and the light direction.
There are also two drawing styles : filled triangles or not.

- Comments in the code contain additional information to use the plot function.

## Examples of results
Examples of rendered meshes from the [Non-rigid world dataset here](http://tosca.cs.technion.ac.il/book/resources_data.html) :

Full style :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/cat1_ocaml.jpg)
![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/centaur1_ocaml.jpg)

Edge style :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/centaur1_edge_ocaml.jpg)

Cat + paraboloid :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/catparaboloid.jpg)

## Possible future improvement
- Increase rendering speed with another library
- Colors defined by user
- Compilation
- Function to write OFF files
- Mesh processing
