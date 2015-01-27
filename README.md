interpolateme
=============

Voronoi tessellation of a sphere, by successive truncations using planes tangent to inscribed sphere.

Maze generation from the voronoi cells.

[Web demo](http://www.jollycyb.org/haskell_maze/)

[Video demo 1 (tessellation demo)](https://www.youtube.com/watch?v=SFVgltKpxSA)

[Video demo 2 (tessellation result)](https://www.youtube.com/watch?v=_JYSKB4cUnA)

[Video demo 3 (egg shell world)](http://youtu.be/QiNU-jJ_odc)

Usage
-----

"cabal configure" then "cabal run" or "cabal install"

Run with:

* "--f" for fullscreen.
* "--s someString" (global random seed): the seed to use for all random processes.
* "--c xxx" (tessellation control): the number of cells to create at start. Default is 8000, slow from 20000 onward.
* "--m xxx" (maze control): the maximum length the maze can grow to. Default to the number of cells parameter.
* "--g xxx" (maze control): the minimum depth gap for the maze to re-use a cell. Default to the maximum length parameter.
* "--r" (maze control): allow the maze branches to randomly go up instead of always down.

See the Cool mazes section for examples of parameters usage.

Features
--------

* View control: drag the mouse to rotate the view, use the mouse wheel to zoom.
* D: toggle maze depth.
* I: reverse maze depth.
* +/-: depth exaggeration control.
* Page Up / Page Down: explosion factor control.
* L: show/hide maze path.
* S: show/hide model.
* space: toggle random cell highlighting.
* N: show/hide face normals (originating from Voronoi cells seeds).
* ESC or Q to exit.
* Web demo (partial port, slow, unstable at least on Firefox): open web/index.html after compiling to JS with:

``hastec '--start=$HASTE_MAIN();' --with-js=web/js/app.js MainJs.hs -o web/js/appHaste.js``

In the works
------------

* Optimisations for maze generation (quadtree to store cell seeds and find closest seed faster).
* Optimisations so that the JS port performs decently.
* More maze visualisations (path -> walls).

Cool mazes
----------
* simple eggshell: ``--c 2000``
* high cell count eggshell: ``--s imullinaty --c 10000``
* multilayer eggshell: ``--s trustme --c 8000 --m 1000 --g 400``
* rough eggshell: ``--c 2000 --m 400 --r``
* high cell count rough eggshell: ``--s toomanytocount --c 15000 --r``
* rough multilayer eggshell: ``--s sexyseed --c 1000 --m 250 --g 25 --r``
* 1 layer and a half: ``--s stairwaytoheaven --c 8000 --m 550 --g 400``
* severely multilayered (smooth): ``--s meetchaos --c 6000 --m 2000 --g 500``
* severely multilayered (rough): ``--s meetchaos --c 6000 --g 50 --r``
* wider and wider layers: ``--s somewhatuniform --c 1000 --m 50 --g 5``
* partial, multiple layers: ``--s glibglob --c 8000 --m 300 --g 5 --r``
* partial coverage: ``--s shish --c 8000 --m 300``
* a funny hat: ``--s glibglob --c 8000 --m 30 --g 5``
* weird geom: ``--c 0 --g 1 --m 4``

Credits
-------

Much code was read and tinkered with to have this tiny demo working.

So thanks to them:

* Anthony Cowley, [modern OpenGL in Haskell blog post](http://www.arcadianvisions.com/blog/?p=224)
* Viktor Devecseri, [Bloxorz port to Haskell](https://hackage.haskell.org/package/bloxorz-0.1.2)
* Yves Parès, [OpenGL 3 tutorials](https://github.com/YPares/Haskell-OpenGL3.1-Tutos)