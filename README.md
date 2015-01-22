interpolateme
=============

Voronoi tessellation of a sphere, by successive truncations using planes tangent to inscribed sphere.

Maze generation from the voronoi cells.

[Web demo](http://www.jollycyb.org/haskell_maze/)

[Video demo 1 (tessellation demo)](https://www.youtube.com/watch?v=SFVgltKpxSA)

[Video demo 2 (tessellation result)](https://www.youtube.com/watch?v=_JYSKB4cUnA)

Usage
-----

"cabal configure" then "cabal run" or "cabal install"

Run with:

* "--f" for fullscreen.
* "--c xxx" to specify the number of cells to create at start. Default is 1000, recommended between 1000 and 10000.
* "--s someString" to specify the seed to use to randomize the cells.
* "--g xx" to specify the maze overlapping factor. 100 for no overlapping, 25 for medium overlapping, >10 for extreme overlapping.
* "--p" to compute and draw the maze path. Warning: *very* memory heavy for models with ~3000 cells or more.

Features
--------

* View control: drag the mouse to rotate the view, use the mouse wheel to zoom.
* D: toggle maze depth.
* I: reverse maze depth.
* +/-: depth exaggeration control.
* Page Up / Page Down: explosion control.
* L: show/hide maze path (must have been started with --p).
* S: show/hide model.
* N: show/hide face normals (originating from face centers).
* ESC or Q to exit.
* Web demo (partial port, slow, unstable at least on Firefox): open web/index.html after compiling to JS with:

``hastec '--start=$HASTE_MAIN();' --with-js=web/js/app.js MainJs.hs -o web/js/appHaste.js``

In the works
------------

* Optimisations for maze generation.
* Optimisations so that the JS port performs decently.
* Nicer maze visualisation.

Credits
-------

Much code was read and tinkered with to have this tiny demo working.

So thanks to them:

* Anthony Cowley, [modern OpenGL in Haskell blog post](http://www.arcadianvisions.com/blog/?p=224)
* Viktor Devecseri, [Bloxorz port to Haskell](https://hackage.haskell.org/package/bloxorz-0.1.2)
* Yves Parès, [OpenGL 3 tutorials](https://github.com/YPares/Haskell-OpenGL3.1-Tutos)