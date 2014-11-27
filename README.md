interpolateme
=============

Haskell demo using openGL shaders.

![Renders](http://i.imgur.com/kEdyM7Q.png)

Usage
-----

"cabal configure" then "cabal run" or "cabal install"

Run with:

* "--f" for fullscreen
* "--s" for static mode, with no deconstruction/reconstruction
* "--i" to draw back faces, cull front faces (inverse of default rendering)
* "--b" to draw both faces, back and front
* "--json file(,i)*" to render the ASSIMP formatted model from the json file. Optional indice allow to select which meshes to draw. By default, all meshes are combined into one.

Features
--------

* View control: drag the mouse to rotate the view, use the mouse wheel to zoom.
* Reconstruction: upon mouse / key release, the model reconstructs itself to appear intact to the viewer (in default mode, in static mode, the model is loaded and displayed verbatim).
* Light control: with the left control key pressed, drag the mouse to orient the light source and use the mouse wheel to adjust the light intensity.
* Model adjustment: use arrow keys to rotate the model (useful when model is facing downward or upward).
* Misc: esc or q to exit.


In the works
------------

More exotic transition animations.

Credits
-------

Much code was read and tinkered with to have this tiny demo working.

So thanks to them:

* Anthony Cowley, [modern OpenGL in Haskell blog post](http://www.arcadianvisions.com/blog/?p=224)
* Viktor Devecseri, [Bloxorz port to Haskell](https://hackage.haskell.org/package/bloxorz-0.1.2)
* Yves Parès, [OpenGL 3 tutorials](https://github.com/YPares/Haskell-OpenGL3.1-Tutos)