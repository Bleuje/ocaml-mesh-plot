![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/header.jpg)

# Basic mesh plot with Objective Caml
Shows meshes in OFF format (uncolored triangle-based with float values for vertices positions), and R^2 -> R functions on a rectangular domain.
It uses the standard OCaml graphics library (it's slow).
The code is contained in the file **3dplot.ml** and **mesh3dtools.ml**, some more functions are in **meshScalarFunctions.ml**.

Some examples can be computed by runnning **main.ml**.

## How to use the code
- To use the plot functions in *3dplot.ml*, use *"#use "3dplot.ml";;"*.

- **mesh3dtools.ml** contains all the mesh related things independant from plot.

- You can choose the positions of the camera with `.cameraposition`. The camera has 3 more parameters : angles `.phi` and `.theta` (spherical coordinates), and a *"zoom" factor* `.zoomfactor`.

- **`loadMesh filePath`** loads an uncolored triangle-based OFF format mesh from the string *filePath*.

- **`writeMesh mesh filePath`** writes *mesh* in OFF format to *filePath*.

- **`plotMesh settings mesh`** plots *mesh* given its plot *settings*. The *settings* consist in many attributes : color (RGB or HSV) `.colorchoice`, drawing style (Full or Edge) `.style`, window size `.windowsize`, light direction `.lightdirection`, camera settings `.cameraview`, and the number of printed triangles per flush `.printstep`, the shader function `.shaderRGB`.

- meshes have two **`colorstyle`** : if set to *Outside*, the mesh doesn't define its colors itself, if set to *VertexValue*, an array of float\*float\*float defines the color in rgb format for each vertex.

- **`setColorArrayFromValues gstyle f mesh`** uses the *f* function *mesh -> float array* that generates an array from the *mesh* to define a float value for each vertex of the *mesh*. It then uses the color gradient style *gstyle* (see part about color gradients below) to define the color array of the mesh.

- Comments in the code contain additional information on how to use functions.

## Examples of results
Examples of rendered meshes from the [Non-rigid world dataset here](http://tosca.cs.technion.ac.il/book/resources_data.html) :

Full style vs Edge style :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/centaur1_ocaml.jpg)
![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/centaur1_edge_ocaml.jpg)

## Some tools
**mesh3dtools.ml** has some functions to create meshes, modify meshes or put them together :
- **`copyMesh mesh`** returns a copy of *mesh*.
- **`movedMesh mesh (x,y,z)`** returns a new mesh after a translation (given by the vector (x,y,z)) to mesh.
- **`deformedMesh mesh g`** applies function *g* to vertices positions of *mesh* to return a new mesh.
- **`concatMeshList l`** returns a new mesh that contains the meshes in the mesh list *l*.
- **`meshOfHeightMapRect f (xmin,xmax) (ymin,ymax) step`** creates a mesh from a R^2 -> R function *f*,
on a rectangle domain with borders parallel to x or y axis (boundaries are given by *xmin*, *xmax*, *ymin*, *ymax*, and the discretization is done with the parameter *step*).

(Cats + deformed cat + paraboloid) concatenation :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/catfamily2.jpg)

Centaur with a saturation gradient based on vertex height :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/centaurgradientheight.jpg)

## Shaders
The plot settings have an attribute `.shaderRGB`. It contains a function `shader sc c` that takes as arguments the scalar product (between the normalized light direction and face normal) sc, and the color sc (triplet of floats between 0.0 and 1.0 in RGB format). It has to return a triplet of integers beween 0 and 255 representing the color in RGB format.

It is therefore easy for the user to define its own shader function.

Here is a picture that shows the difference without/with shader :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/shadertest.jpg)

## Scalar Functions
If we can draw colors when given values on vertices, let's see which values we can give. **meshScalarFunctions.ml** contains many examples of functions that can be applied to meshes to can scalar values on the whole mesh.

- `valueConstant c`and `valueHeight`are quite explicit.
- Some other functions perform a BFS or DFS on the mesh, you can put the index of the starting vertex as argument (without it takes a random start). Those functions either return the time when the vertex was visited, or the depth from start, on each vertex. Therefore the function names are `valueBFScount`, `valueDFScount`, `valueBFSdepth`, `valueDFSdepth`.

## Gradient styles
There are many gradient styles, they are all color gradients from the vertices achieving minimum and maximum values of the value array.

- **Hue**, **Saturation** and **Value** gradients have one parameter of the HSV color format changing while the others are fixed.

- The *linear* and *quadratic* color gradients use an interpolation between to colors in the RGB or HSV spaces.

- Those that have the keyword **Cycle** are some previously defined color gradients with a new interger as parameter to define how many times the gradient will be repeated (triangular periodicity).

## Known flaws
- Slow (but I think that the fact it uses the standard library is nice, and the colors are nice and worth additional computation time)

## Possible future improvement
- More mesh processing
- Make it more user friendly
- Increase rendering speed with another library
- Optimize speed
