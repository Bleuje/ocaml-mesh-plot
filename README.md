![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/header.jpg)

# Basic mesh plot with Objective Caml
Shows meshes in OFF format (uncolored triangle-based with float values for vertices positions), and R^2 -> R functions on a rectangular domain.
It uses the standard OCaml graphics library (it's slow).
The code is contained in the file **3dplot.ml** and **mesh3dtools.ml**, some more functions are in **meshScalarFunctions.ml**.

Some examples can be computed by runnning **main.ml**.

## How to use the code
- To use the plot functions in *3dplot.ml*, use **`#use "3dplot.ml";;`**.

- **mesh3dtools.ml** contains all the main mesh related things independant from plot, **mathtools.ml** contains some mathematical purely tools.

- **`loadMesh filePath`** loads an uncolored uncommented triangle-based OFF format mesh from the string *filePath*.

- **`writeMesh mesh filePath`** writes *mesh* in OFF format to *filePath*. It also writes the mesh colors if there are any.

- **`plotMesh settings mesh`** plots *mesh* given its plot *settings*.

### More detailed information :

- I advice to read the types defined at the top of *mesh3dtools.ml* and *3dplot.ml* to understand how to use the code better, but I'll try to explain a little here without the definitions of the new types.

- the **`mesh`**structure consists in the following attributes : `.nVert`: number of vertices, `.nTria` : number of triangles, `.positions` : positions.(i) is an array representing the i-th vertex position (cartesian coordinates),  `.triangles` : triangles.(i) is an array of size 3 that indicates the indices of vertices from the triangle (order gives orientation), `.colorstyle` : see below for further explaination.

- The **`plotSettings`** structure consist in many attributes : `.colorchoice` : color if not specified by mesh (RGB or HSV), `.style` : drawing style (Full or Edge), `.windowsize` : window size, `.lightdirection` : light direction, `.cameraview` : camera settings, `.printstep` : the number of printed triangles per flush, `.shaderRGB` : the shader function (explained below).

- An instance of the structure **`cameraView`** can define the camera view. One can choose the position of the camera with `.cameraposition`. The camera has 3 more parameters : angles `.phi` and `.theta` (spherical coordinates), and a *"zoom" factor* `.zoomfactor`. `cameraView` instances are used in the structure `plotSettings`.

- meshes have two colorstyles (**`colorstyle`**) : if set to `Outside`, the mesh doesn't define its colors itself, if set to `VertexValue`, an array of float\*float\*float defines the color in rgb format for each vertex.

- **`setColorArrayFromValues gstyle f mesh`** uses the *f* function `mesh -> values` that generates an array from the *mesh* to define a float value for each vertex or for each triangle of the *mesh*. It then uses the color gradient style *gstyle* (see part about color gradients below) to define the color array of the mesh.

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

## Shaders
The plot settings have an attribute `.shaderRGB`. It contains a function `shader sc c` that takes as arguments the scalar product (between the normalized light direction and face normal) sc, and the color sc (triplet of floats between 0.0 and 1.0 in RGB format). It has to return a triplet of integers beween 0 and 255 representing the color in RGB format.

It is therefore easy for the user to define its own shader function.

Here is a picture that shows the difference without/with shader :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/shadertest.jpg)

## Scalar functions
If we can draw colors when given values on vertices, let's see which values we can give. **meshScalarFunctions.ml** contains many examples of functions (they end with *_value*) that can be applied to meshes to can scalar values on the whole mesh. They either return `VertexValues` of a float array (values for each vertex), or `PolygonValues` of a float array (values for each triangle). `setColorArrayFromValues gstyle f mesh` will work on both cases.

- `constant_value c`and `height_value`are quite explicit.
- Some other functions perform a BFS or DFS on the mesh, you can put the index of the starting vertex as argument (without it takes a random start). Those functions either return the time when the vertex was visited, or the depth from start, on each vertex. Therefore the function names are `bfsCount_value`, `dfsCount_Value`, `bfsDepth_value`, `dfsDepth_value`.
- `discreteGaussianCurvature_value abs_max` computes the discrete gaussian curvature. I used 2*PI - (sum of the angles around the vertex) as formula. I think it should actually use the area around. So far, there are some unexpected high and low values, and it's possible to get rid of them using the `abs_max` parameter (for example set to 0.05). Here's a picture with a gradient applied to the discrete gaussian curvature :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/discretegaussiancurvature.jpg)

- `smoothenValues max_iter f` adapts the function f (`mesh -> values`) by computing `max_iter` iterations of mean values with neighbors in the mesh graph. `smoothenValues max_iter f` will have type `mesh -> values`. Example of use : `setColorFromValues (LinearCycleHSV (1,(1.0,1.0,1.0),(0.5,1.0,0.6))) (smoothenValues 50 (discreteGaussianCurvature_value 0.09)) myMesh2`, result picture below :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/smoothcurvature.jpg)

## Gradient styles
There are many gradient styles, they are all color gradients between the vertices achieving minimum and maximum values of an array of scalar values.

- `Hue`, `Saturation` and `Value` gradients have one parameter of the HSV color format changing while the others are fixed.

- The `Linear` and `Quadratic` color gradients use an interpolation between two colors in the RGB or HSV spaces.

- Those that have the keyword `Cycle` are some previously defined color gradients with a new interger as parameter to define how many times the gradient will be repeated (triangular periodicity).

- `Personal f` is a gradient that uses the `float -> rgb_color` f function to define the gradient.

## Opening results with MeshLab
After writing the mesh with `writeOffMesh` it is possible to load and see it in MeshLab as shown in the picture below (OCaml plot on the left, MeshLab plot on the right) :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/ocamlplotcomparison1.jpg)
![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/ocamlplotcomparison2.jpg)

## Known flaws
- Slow (but I think that the fact it uses the standard library is nice, and the colors are nice and worth additional computation time).
- Absence of anti-aliasing.

## Possible future improvement
- More mesh processing
- Make it more user friendly
- Increase rendering speed with another library
- Optimize speed
- More functionnal style
- Move the camera on a simplified point cloud
- Fix bugs?
