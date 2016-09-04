<p align="center">
  <img src="https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/header.jpg">
</p>

# Basic mesh plot with Objective Caml
So far, this is mainly a module (`Plot3D`) to show meshes from OFF format (uncolored triangle-based with float values as vertices positions), and R^2 -> R functions on a rectangular domain.
It uses the standard OCaml graphics library (it is slow).

Some examples are shown by runnning **main.ml**.

## Quick example
**quick_main.ml** is a quick a example of how a user could use the Plot3D module.
``` ocaml
#use "plot3d.ml";;
open Plot3D;;

let mySettings = Plot3D.defaultSettings;;
let myMesh = loadOffMesh "examples/centaur1.off";;

Graphics.open_graph mySettings.windowsize;
changeCameraViewGUI 0.05 myMesh mySettings;
plotMesh mySettings myMesh;;
```
Here the user loads the mesh `centaur1.off`, uses his keyboard to choose a camera angle from a point cloud.
Then it plots the mesh with some default settings contained in the module.

Pictures (camera view choice, then rendered result) :
<p align="center">
  <img src="https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/quickmain.jpg">
</p>

You should be able to obtain this by running `quick_main.ml` after downloading the master branch here.
Keyboard shorcuts of `changeCameraViewGUI` are explained below.

## How to use the code
- To use the plot functions in *3dplot.ml*, use **`#use "3dplot.ml"`** and then `open Plot3D`. To load functions to have some interesting (or not) textures, **`#use "scalarMeshFunctions.ml"`** and `open MeshFunctions`.

- **`loadMesh filePath`** loads an uncolored uncommented triangle-based OFF format mesh from the string *filePath*.

- **`writeMesh mesh filePath`** writes *mesh* in OFF format to *filePath*. It also writes the mesh colors if there are any.

- **`plotMesh settings mesh`** plots *mesh* given its plot *settings*.

- **`changeCameraViewGUI part mesh settings`** uses a point cloud generated with a proportion `part`of the mesh vertices to change the `cameraview` attribute of `settings`, in the current window (shortcuts explained [below](https://github.com/Bleuje/ocaml-mesh-plot#keyboard-shortcuts)).

- **mesh3dtools.ml** contains all the main mesh related things independant from plot, **mathtools.ml** contains some purely mathematical tools.

### More detailed information :

- The **`mesh`**structure consists in the following attributes : `nVert`: number of vertices, `nTria` : number of triangles, `positions` : positions.(i) is an array representing the i-th vertex position (cartesian coordinates),  `triangles` : triangles.(i) is an array of size 3 that indicates the indices of vertices from the triangle (order gives orientation), `colorstyle` : see below for further explaination.

``` ocaml
type rgb_color = float*float*float (* color in RGB format with valeus between 0.0 and 1.0 *)
type hsv_color = float*float*float  (* same with HSV format *)
type vec3D = float*float*float (* vector in 3D space *)

type mesh =
{ nVert : int;
nTria : int;
positions : vec3D array;
triangles : int array array;
mutable colorstyle : colorStyle; }

type colorStyle =
| Outside
| VertexColor of rgb_color array
| TriangleColor of rgb_color array
```

- The **`plotSettings`** structure consist in many attributes : `colorchoice` : color if not specified by mesh (RGB or HSV), `style` : drawing style (Full or Edge), `windowsize` : window size, `lightdirection` : light direction, `cameraview` : camera settings, `printstep` : the number of printed triangles per flush, `shaderRGB` : the shader function (explained below).

``` ocaml
type plotStyle = Full | Edge

type plotSettings =
    { mutable cameraview : cameraView;
    mutable colorchoice : colorChoice;
    mutable style : plotStyle;
    mutable lightdirection : vec3D;
    mutable windowsize : string;
    mutable printstep : int;
    mutable shaderRGB : float -> rgb_color -> int*int*int; }
```

- An instance of the structure **`cameraView`** can define the camera view. One can choose the position of the camera with `cameraposition`. The camera has 3 more parameters : angles `phi` and `theta` (spherical coordinates), and a *"zoom" factor* `zoomfactor`. `cameraView` instances are used in the structure `plotSettings`.

``` ocaml
type cameraView =
    { mutable phi : float;
    mutable theta : float;
    mutable zoomfactor : float;
    mutable cameraposition : vec3D; }
```
- meshes have three colorstyles (**`colorstyle`**) : if set to `Outside`, the mesh doesn't define its colors itself, if set to `VertexValue` (respectively to `TriangleColor`), an array of float\*float\*float defines the color in rgb format for each vertex (respectively for each triangle).

- **`setColorArrayFromValues gstyle f mesh`** uses the *f* function `mesh -> values` that generates an array from the *mesh* to define a float value for each vertex or for each triangle of the *mesh*. It then uses the color gradient style *gstyle* (see part about color gradients [below](https://github.com/Bleuje/ocaml-mesh-plot/blob/master/README.md#color-gradient-styles)) to define the color array of the mesh.

``` ocaml
type values =
    | VertexValues of float array
    | TriangleValues of float array
```

- **`setDefaultCameraView settings mesh`** modifies the plot settings (`settings`here) using the `mesh` : the camera angle is scaled from the mesh automatically.

- Comments in the code may contain additional information about how to use functions.

## Mesh data
Examples of rendered meshes presented here use the [Non-rigid world dataset](http://tosca.cs.technion.ac.il/book/resources_data.html).

## Some tools
The module **`Mesh3D`** (contained in `Plot3D`) has some functions to create meshes, modify meshes or put them together :
- **`copyMesh mesh`** returns a copy of *mesh*.
- **`movedMesh mesh (x,y,z)`** returns a new mesh after a translation (given by the vector (x,y,z)) to mesh.
- **`deformedMesh mesh g`** applies function *g* to vertices positions of *mesh* to return a new mesh.
- **`concatMeshList l`** returns a new mesh that contains the meshes in the mesh list *l*.
- **`meshOfHeightMapRect f (xmin,xmax) (ymin,ymax) step`** creates a mesh from a R^2 -> R function *f*,
on a rectangle domain with borders parallel to x or y axis (boundaries are given by *xmin*, *xmax*, *ymin*, *ymax*, and the discretization is done with the parameter *step*).

(Cats + deformed cat + paraboloid) concatenation :

<p align="center">
  <img src="https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/catfamily2.jpg">
</p>

## Shaders
The plot settings have an attribute `shaderRGB`. It contains a function `shader sc c` that takes as arguments the scalar product (between the normalized light direction and face normal) sc, and the color sc (triplet of floats between 0.0 and 1.0 in RGB format). It has to return a triplet of integers beween 0 and 255 representing the color in RGB format.

It is therefore easy for the user to define its own shader function.

Here is a picture that shows the difference without/with shader :

<p align="center">
  <img src="https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/shadertest.jpg">
</p>

## Scalar functions
If we can draw colors when given values on vertices, let's see which values we can give. The module **`MeshFunctions`** from *meshScalarFunctions.ml* contains many examples of functions (they end with *_value*) that can be applied to meshes to can scalar values on the whole mesh. They either return `VertexValues` of a float array (values for each vertex), or `PolygonValues` of a float array (values for each triangle). `setColorArrayFromValues gstyle f mesh` will work on both cases.

- `constant_value c`and `height_value`are quite explicit.
- Some other functions perform a BFS or DFS on the mesh, you can put the index of the starting vertex as argument (without it takes a random start). Those functions either return the time when the vertex was visited, or the depth from start, on each vertex. Therefore the function names are `bfsCount_value`, `dfsCount_Value`, `bfsDepth_value`, `dfsDepth_value`.
- `discreteGaussianCurvature_value abs_max` computes the discrete gaussian curvature. I used 2*PI - (sum of the angles around the vertex) as formula. I think it should actually use the area around. So far, there are some unexpected high and low values, and it's possible to get rid of them using the `abs_max` parameter (for example set to 0.05). Here's a picture with a gradient applied to the discrete gaussian curvature :

<p align="center">
  <img src="https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/discretegaussiancurvature.jpg">
</p>

- `smoothenValues max_iter f` adapts the function f (`mesh -> values`) by computing `max_iter` iterations of mean values with neighbors in the mesh graph. `smoothenValues max_iter f` will have type `mesh -> values`. Example of use : `setColorFromValues (LinearCycleHSV (1,(1.0,1.0,1.0),(0.5,1.0,0.6))) (smoothenValues 50 (discreteGaussianCurvature_value 0.09)) myMesh2`, result picture below :

<p align="center">
  <img src="https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/smoothcurvature.jpg">
</p>

## Color gradient styles
There are many gradient styles, they are all color gradients between the vertices achieving minimum and maximum values of an array of scalar values.

- `Hue`, `Saturation` and `Value` gradients have one parameter of the HSV color format changing while the others are fixed.

- The `Linear` and `Quadratic` color gradients use an interpolation between two colors in the RGB or HSV spaces.

- Those that have the keyword `Cycle` are some previously defined color gradients with a new interger as parameter to define how many times the gradient will be repeated (triangular periodicity).

- `Personal f` is a gradient that uses the `float -> rgb_color` f function to define the gradient.

``` ocaml
    type gradientStyle =
    | Hue of float*float
    | Value of float*float
    | Saturation of float*float
    | HueCycles of int*float*float
    | LinearRGB of rgb_color*rgb_color
    | LinearHSV of hsv_color*hsv_color
    | LinearCycleRGB of int*rgb_color*rgb_color
    | LinearCycleHSV of int*hsv_color*hsv_color
    | QuadraticRGB of rgb_color*rgb_color
    | QuadraticHSV of hsv_color*hsv_color
    | QuadraticCycleRGB of int*rgb_color*rgb_color
    | QuadraticCycleHSV of int*hsv_color*hsv_color
    | Personal of (float -> rgb_color)
```

## Opening results with MeshLab
After writing the mesh with `writeOffMesh` it is possible to load and see it in MeshLab as shown in the picture below (OCaml plot on the left, MeshLab plot on the right) :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/ocamlplotcomparison1.jpg)
![image](https://raw.githubusercontent.com/Bleuje/ocaml-mesh-plot/master/pictures/ocamlplotcomparison2.jpg)

## Keyboard shortcuts

In the camera view GUI, here are the keyboard shortcuts (designed to be convenient with AZERTY keyboard that have a numpad) :

| Key | Description |
| ------ | ----------- |
| O,L,F | Finish, stop |
| R | Go (back) to default camera view |
| 5,Z   | Move forward |
| 0,S | Move backward |
| 4,6 | Look left and right respectively |
| 8,2 | Look up and down respectively |
| 1,3 | Move to left or right respectively |
| 7,9 | Move up or down respectively |
| G,H | Respectively increases or decreases the zoom factor | 
| W | Increase movement speed |
| X | Decrease movement speed |
| C | Increase rotation speed |
| V | Decrease rotation speed |
| B | Increase point size |
| N | Decrease point size |

## Known flaws
- Slow (but I think that the fact it uses the standard library is nice, and the colors are nice and worth additional computation time).
- Absence of anti-aliasing.

## Possible future improvement
- More mesh processing
- Make it more user friendly
- Increase rendering speed
- More functionnal coding style
- Move the camera with a simplified point cloud representing the mesh, add a default camera position based on the mesh
- Generalize to more mesh formats
- Fix bugs?
