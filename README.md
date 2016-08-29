# Basic mesh plot with Objective Caml
Shows meshes in OFF format (uncolored triangle-based with float values for vertices positions), and R^2 -> R functions on a rectangular domain.
It uses the standard OCaml graphics library (it's slow).
The code is contained in the file **3dplot.ml** and **mesh3dtools.ml**, and used in **main.ml** on examples.

## How to use the code
- to use the functions in *3dplot.ml*, use *"#use "3dplot.ml";;"* (same for *mesh3dtools.ml*).

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

## Some tools
**mesh3dtools.ml** has some functions to modify meshes or put them together :
- *move mesh (x,y,z)* gives a new mesh after a translation (given by the vector (x,y,z)) to mesh.
- *deformedMesh mesh g* applies function g to vertices positions and returns a new mesh.
- *concatMeshList l* returns a new mesh that contains the meshes in the mesh list l.

(Cats + deformed cat + paraboloid) concatenation:

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/catfamily.jpg)

## Possible future improvement
- Colors defined by user
- Function to write OFF files
- More mesh processing
- Increase rendering speed with another library
