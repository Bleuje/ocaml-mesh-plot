# Basic mesh plot with Objective Caml
Shows meshes in OFF format (uncolored triangle-based with float values for vertices positions), and R^2 -> R functions on a rectangular domain.
It uses the standard OCaml graphics library (it's slow).
The code is contained in the file **3dplot.ml** and **mesh3dtools.ml**, and used in **main.ml** on examples.

## How to use the code
- To use the plot functions in *3dplot.ml*, use *"#use "3dplot.ml";;"*.

- *mesh3dtools.ml* contains all the mesh related things independant from plot.

- You can choose the positions of the camera. The camera has 3 more parameters : angles *Phi* and *Theta* (spherical coordinates), and a *"zoom" factor*.

- *loadMesh filePath* loads an uncolored triangle-based OFF format mesh from the string *filePath*.

- *loadMesh mesh filePath* writes *mesh* in OFF format to *filePath*.

- *plotMesh mesh settings* plots *mesh* given its plot *settings*. The settings consist in color (RGB or HSV), drawing style (Full or Edge), window size, light direction, camera settings, and the number of printed triangles per flush.

- meshes have two colorstyle : if set to *Outside*, the mesh doesn't define its colors itself, if set to VertexValue, an array of float*float*float defines the color in rgb format for each vertex.

- *applyValueArrayWithColors mesh gstyle varray* uses the array varray that defines a float for each vertex of the mesh. It then uses the gradient style gstyle to define the color array of the mesh. There are 3 types of gradients ; Hue, Saturation and Value. Each has 2 parameters : the constant other values in the HSV color format.

- Comments in the code contain additional information on how to use functions.

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

Same with noise on a cat, different angle and color :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/catfamily2.jpg)

Centaur with a saturation gradient based on vertex height :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/ccentaurgradientheight.jpg.jpg)

## Known flaws
- Slow (but I think that the fact it uses the standard library is nice, and the colors are nice and worth additional computation time)

## Possible future improvement
- More mesh processing
- Make it more user friendly
- Increase rendering speed with another library
- Optimize speed
