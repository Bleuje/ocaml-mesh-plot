# Basic mesh plot with Objective Caml
Shows meshes in OFF format (uncolored triangle-based with float values for vertices positions), and R^2 -> R functions on a rectangular domain.
It uses the standard OCaml graphics library (it's slow).
The code is contained in the file **3dplot.ml** and **mesh3dtools.ml**, and used in **main.ml** on examples.

## How to use the code
- To use the plot functions in *3dplot.ml*, use *"#use "3dplot.ml";;"*.

- *mesh3dtools.ml* contains all the mesh related things independant from plot.

- You can choose the positions of the camera. The camera has 3 more parameters : angles *Phi* and *Theta*, and a *"zoom" factor*.

- *loadMesh filePath* loads an uncolored triangle-based OFF format mesh from the string *filePath*.

- *loadMesh mesh filePath* writes *mesh* in OFF format to *filePath*.

- *plotMesh mesh settings* plots *mesh* given its plot *settings*. The settings consist in color (RGB or HSV), drawing style (Full or Edge), window size, light direction, camera settings, and the number of printed triangles per flush.  

- Comments in the code contain additional information on how to use the plot function.

## Examples of results
Examples of rendered meshes from the [Non-rigid world dataset here](http://tosca.cs.technion.ac.il/book/resources_data.html) :

Full style :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/cat1_ocaml.jpg)
![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/centaur1_ocaml.jpg)

Edge style :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/centaur1_edge_ocaml.jpg)

## Some tools
**mesh3dtools.ml** has some functions to create meshes, modify meshes or put them together :
- *copyMesh mesh* returns a copy of *mesh*.
- *movedMesh mesh (x,y,z)* returns a new mesh after a translation (given by the vector (x,y,z)) to mesh.
- *deformedMesh mesh g* applies function *g* to vertices positions of *mesh* to return a new mesh.
- *concatMeshList l* returns a new mesh that contains the meshes in the mesh list *l*.
- *meshOfHeightMapRect f (xmin,xmax) (ymin,ymax) step* creates a mesh from a R^2 -> R function (*f*),
on a rectangle domain with borders parallel to x or y axis (boundaries are given by *xmin*, *xmax*, *ymin*, *ymax*, and the discretization is done with the parameter step).

(Cats + deformed cat + paraboloid) concatenation:

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/catfamily.jpg)

## Possible future improvement
- More mesh processing
- Increase rendering speed with another library
