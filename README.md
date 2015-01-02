interpolateme
=============

Voronoi tessellation of a sphere <=> truncating regular polyhedras with planes tangent to inscribed sphere.

[Web demo](http://www.jollycyb.org/haskell_truncate/)

Usage
-----

"cabal configure" then "cabal run" or "cabal install"

Run with:

* "--f" for fullscreen
* "--b" to draw both faces, back and front
* "--c xxx" to specify the number of cuts to apply on the model
* "--s someString" to specify the seed to use to randomize the cuts

Features
--------

* View control: drag the mouse to rotate the view, use the mouse wheel to zoom.
* Show/hide cutting plane: tab key.
* Cut: hit space to truncate the model, keeping what's beyond the cutting plane.
* Misc: esc or q to exit.
* Web demo: open web/index.html after compiling to JS with:

``hastec '--start=$HASTE_MAIN(); appMain();' --with-js=web/js/app.js MainJs.hs -o web/js/appHaste.js``

In the works
------------

Turning random truncations / tessellations into spherical labyrinths.

Credits
-------

Much code was read and tinkered with to have this tiny demo working.

So thanks to them:

* Anthony Cowley, [modern OpenGL in Haskell blog post](http://www.arcadianvisions.com/blog/?p=224)
* Viktor Devecseri, [Bloxorz port to Haskell](https://hackage.haskell.org/package/bloxorz-0.1.2)
* Yves Parès, [OpenGL 3 tutorials](https://github.com/YPares/Haskell-OpenGL3.1-Tutos)