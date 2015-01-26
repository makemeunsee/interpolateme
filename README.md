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
* "--c xxx" to specify the number of cells to create at start. Default is 8000, slow from 20000 onward.
* "--s someString" to specify the seed to use to randomize the cells.
* "--g xx" to specify the maze overlapping factor. 100 for no overlapping, 25 for medium overlapping, >10 for extreme overlapping.

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