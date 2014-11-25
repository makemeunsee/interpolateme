interpolateme
=============

Haskell demo using openGL shaders.

![Renders](http://i.imgur.com/kEdyM7Q.png)

Usage
-----

"cabal configure" then "cabal run" or "cabal install"

Run with:

* "--f" for fullscreen
* "--s" for static mode, with no animation / randomization
* "--i" to draw back faces, cull front faces (inverse of default rendering)
* "--b" to draw both faces, back and front
* "--json file(,i)*" to render the ASSIMP formatted model from the json file. Optional indice allow to select which meshes to draw. By default, all meshes are combined into one.

Features
--------

Drag the mouse to rotate the view.

Use arrow keys to rotate the model (useful when model is facing downward or upward).

Upon mouse / key release, the model reconstructs itself to appear intact to the viewer (in default mode, in static mode, the model is loaded and displayed verbatim).

Misc: esc or q to exit, mouse wheel to zoom in/out.

In the works
------------

Shader lighting.

Credits
-------

Much code was read and tinkered with to have this tiny demo working.

So thanks to them:

* Anthony Cowley, [modern OpenGL in Haskell blog post](http://www.arcadianvisions.com/blog/?p=224)
* Viktor Devecseri, [Bloxorz port to Haskell](https://hackage.haskell.org/package/bloxorz-0.1.2)
* Yves Parès, [OpenGL 3 tutorials](https://github.com/YPares/Haskell-OpenGL3.1-Tutos)