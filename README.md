interpolateme
=============

Voronoi tessellation of a sphere <=> truncating regular polyhedras with planes tangent to inscribed sphere.

[Web demo](http://www.jollycyb.org/haskell_truncate/)

[Video demo 1](https://www.youtube.com/watch?v=SFVgltKpxSA)

[Video demo 2](https://www.youtube.com/watch?v=_JYSKB4cUnA)

Usage
-----

"cabal configure" then "cabal run" or "cabal install"

Run with:

* "--f" for fullscreen
* "--b" to draw both faces, back and front
* "--c xxx" to specify the number of cuts to apply on the model. Warning: very memory heavy for values >= 2000.
* "--s someString" to specify the seed to use to randomize the cuts

Features
--------

* View control: drag the mouse to rotate the view, use the mouse wheel to zoom.
* SPACE: truncate the model, keeping what's beyond the cutting plane.
* TAB: show/hide the cutting plane.
* C: apply 250 random cuts in a row. Warning: CPU heavy on already high poly objects.
* N: show/hide face normals (originating from face centers).
* L: show/hide maze path.
* W: show/hide maze walls.
* S: show/hide model.
* ESC or Q to exit.
* Web demo (no maze support yet): open web/index.html after compiling to JS with:

``hastec '--start=$HASTE_MAIN(); appMain();' --with-js=web/js/app.js MainJs.hs -o web/js/appHaste.js``

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