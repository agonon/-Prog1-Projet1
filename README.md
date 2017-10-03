This folder contains 5 files :

-penrose_base.ml;
-penrose_animation.ml;
-penrose_optimization.ml
-penrose_idth_division.ml;
-READ ME.

The files with the ".ml" extension need to be open in an a code editor/IDE in order to execute each instruction from top to bottom of the code. You can also launch ocaml in a terminal before writing  " #use "file.ml";; ". 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

THE PAVING OF PENROSE


Overall, each file is organized as follows : the main source code solving the problem separated from the instructions and functions which allows the reader to test easier the code. The separation between these two parts is indicated by a comment as " (*/////////////*) ".
The following content is a more detailed description of the files related to the Paving of Penrose problem.

PENROSE_BASE.ML

Base.ml contains the basic and unsophisticated functions to pave a triangle with the paving of Penrose. The function "divide" takes as first argument a natural different from 0.

PENROSE_ANIMATION.ML 

The new function "wait" wich takes a float "dt" as argument makes the interpreter wait "dt" seconds when he reads this instruction. This function is used inside the function "draw" to animate the drawing.

PENROSE_OPTIMIZATION.ML

The source code is more optimized than in the two previous files : each triangle edge is drawn only once thanks to changes operated inside the functions "divide" and "draw". We chose to keep the animation so as to verify visually that each edge seems indeed drawn only once.

PENROSE_WIDTH_DIVISION.ML

Up to now the code was calculating the triangles to draw in depth. The function "width_division" has the same role than the function until now known as "divide" but it calculates the triangles to draw generation by generation ("in width"). We chose to keep the animation and the optimization developed in the previous files in order to observate how the algorithm works by looking at the graphic window.
