function arrToMat( arr ) {
    var mat = new THREE.Matrix4();
    mat.set( arr[0], arr[1], arr[2], arr[3],
             arr[4], arr[5], arr[6], arr[7],
             arr[8], arr[9], arr[10], arr[11],
             arr[12], arr[13], arr[14], arr[15] );
    return mat;
}

function makeMesh( model ) {
    var customUniforms = {
       u_time: { type: "1f", value: 0 },
       u_allowDepth: { type: "1f", value: 0 },
       u_borderWidth: { type: "1f", value: 0 },
       u_mvpMat: { type: "m4", value: new THREE.Matrix4() },
       u_color: { type: "v4", value: new THREE.Vector4( 1, 1, 1, 1 ) },
       u_borderColor: { type: "v4", value: new THREE.Vector4( 0, 0, 0, 1 ) }
    };
    
    var geometry = new THREE.BufferGeometry();
    
    // create attributes for each vertex
    // currently 2 colors are given as vertex attributes
    var attributes = {
        a_normal: {
             type: 'v3',
             value: []
        },
        a_centerFlag: {
            type: 'f',
            value: []
        },
        a_mazeDepth: {
            type: 'f',
            value: []
        }
    };
//    var shaderMaterial = new THREE.RawShaderMaterial({
    var shaderMaterial = new THREE.ShaderMaterial({
        attributes:     attributes,
        uniforms:       customUniforms,
        vertexShader:   document.getElementById('shader-vs').innerHTML,
        fragmentShader: document.getElementById('shader-fs').innerHTML,
        side: THREE.DoubleSide
    });

    var positions = new Float32Array( model.vertice.length );
    var normals = new Float32Array( model.vertice.length );
    var centerFlag = new Float32Array( model.vertice.length / 3 );
    var mazeDepth = new Float32Array( model.vertice.length / 3 );
    if ( model.centers !== undefined ) {
        for (var i = 0; i < model.vertice.length / 3; i++) {
            positions [ 3*i ] = model.vertice[ 3*i ];
            normals [ 3*i ] = model.normals[ 3*i ];
            positions [ 3*i+1 ] = model.vertice[ 3*i+1 ];
            normals [ 3*i+1 ] = model.normals[ 3*i+1 ];
            positions [ 3*i+2 ] = model.vertice[ 3*i+2 ];
            normals [ 3*i+2 ] = model.normals[ 3*i+2 ];
//            geometry.vertices.push( new THREE.Vector3( model.vertice[ 3*i ], model.vertice[ 3*i+1 ], model.vertice[ 3*i+2 ] ) ) ;
//            attributes.a_normal.value.push( new THREE.Vector3( model.normals[ 3*i ], model.normals[ 3*i+1 ], model.normals[ 3*i+2 ] ) );
            centerFlag[ i ] = model.centers[ i ];
//            attributes.a_centerFlag.value.push( model.centers[ i ] );
            mazeDepth[ i ] = model.mazeData[ i ];
//            attributes.a_mazeDepth.value.push( model.mazeData[ i ] );
        }
    } else {
        for (var i = 0; i < model.vertice.length / 3; i++) {
            positions [ 3*i ] = model.vertice[ 3*i ];
            positions [ 3*i+1 ] = model.vertice[ 3*i+1 ];
            positions [ 3*i+2 ] = model.vertice[ 3*i+2 ];
//            geometry.vertices.push( new THREE.Vector3( model.vertice[ 3*i ], model.vertice[ 3*i+1 ], model.vertice[ 3*i+2 ] ) ) ;
        }
    }
    var indices = new Uint16Array( model.indice.length );
    for (var i = 0; i < model.indice.length ; i++) {
        indices[ i ] = model.indice[ i ];
//        geometry.faces.push( new THREE.Face3( model.indice[3*i], model.indice[3*i+1], model.indice[3*i+2] ) );
    }

    geometry.addAttribute( 'index', new THREE.BufferAttribute( indices, 1 ) );
    geometry.addAttribute( 'a_mazeDepth', new THREE.BufferAttribute( mazeDepth, 1 ) );
    geometry.addAttribute( 'a_centerFlag', new THREE.BufferAttribute( centerFlag, 1 ) );
    geometry.addAttribute( 'a_normal', new THREE.BufferAttribute( normals, 3 ) );
    geometry.addAttribute( 'position', new THREE.BufferAttribute( positions, 3 ) );

    if ( model.centers !== undefined ) {
        return new THREE.Mesh( geometry, shaderMaterial );
    } else {
        return new THREE.Line( geometry, shaderMaterial, THREE.LinePieces );
    }
}

function modelFromRaw( model, mazeData ) {
    return {
        "vertice": model[0],
        "indice": model[1],
        "normals": model[2],
        "centers": model[3],
        "mazeData": mazeData
    };
}


DummyCamera = function () {
    THREE.Camera.call( this );
    this.type = 'DummyCamera';
};

DummyCamera.prototype = Object.create( THREE.Camera.prototype );

function appMain() {

    var seed = Math.random().toString().slice(2);
    console.log("seed0: ", seed);
    var prng;
    
    function nextSeed() {
        prng = new Math.seedrandom(seed);
        seed = prng().toString().slice(2);
        console.log("next seed: ", seed);
    }

    nextSeed();

    function rndSpherePosition() {
        var u = Math.random();
        var v = Math.random();
        return { "theta": 2*Math.PI*u, "phi": Math.acos( 2*v - 1 ) };
    }

    // help dialog
    $(function() {
        $( "#dialog" ).dialog({
            width: 600,
            height: 200
        });
    });
    $( "#dialog" ).dialog( "close" );

//    function toggleUI() {
//        console.log("toggling ui");
//        if ( $( "#gui" ).css( "display" ) === "none" )
//            $( "#gui" ).show();
//        else
//            $( "#gui" ).hide();
//    }

    // jqueryui widgets
    $(function() {
      $( "button" ).button();
    });

    var depthMaze = false;
    function depthMazeFct() {
        depthMaze = !depthMaze;
    }
    $("#depthMaze").unbind("click");
    $("#depthMaze").click(depthMazeFct);
    $('#depthMaze').attr('checked', false);

    var showMazePath = false;
    function showMazePathFct() {
        showMazePath = !showMazePath;
    }
    $("#showMazePath").unbind("click");
    $("#showMazePath").click(showMazePathFct);
    $('#showMazePath').attr('checked', false);

    var showSolid = true;
    function showSolidFct() {
        showSolid = !showSolid;
        if (!showSolid) {
            $('#depthMaze').attr('disabled', true);
        } else {
            $('#depthMaze').attr('disabled', false);
        }
    }
    $("#showSolid").unbind("click");
    $("#showSolid").click(showSolidFct);
    $('#showSolid').attr('checked', true);

    function showHelp() {
        if ( $( "#dialog" ).dialog( "isOpen" ) )
            $( "#dialog" ).dialog( "close" );
        else
            $( "#dialog" ).dialog( "open" );
    }
    $("#showHelp").unbind("click");
    $("#showHelp").click(showHelp);

    function screenshot() {
        var w = window.open('', '');
        w.document.title = "Screenshot";
        w.document.body.style.backgroundColor = "black";
        var img = new Image();
        img.src = canvas.toDataURL("image/png");
        w.document.body.appendChild(img);
    }
    $("#screenshot").unbind("click");
    $("#screenshot").click(screenshot);

    var toggleFullscreen = THREEx.FullScreen.toggleFct();
    $("#fullscreen").unbind("click");
    $("#fullscreen").click(toggleFullscreen);

    function rndCutsFct() {
        var now = Date.now();

        var oldMesh = currentMesh;
        var oldPathMesh = currentPathMesh;
        scene.remove( oldMesh );
        scene.remove( oldPathMesh );

        console.log("Killing old mesh:", Date.now() - now);

        var max = 0;
        var acc = 0;
        for (var index = 0; index < 64; index++) {
            var now1 = Date.now();
            var cutSeed = rndSpherePosition();
            baseModel = Haste.truncateAtPoint ( baseModel
                                              , cutSeed.theta
                                              , cutSeed.phi);

            var duration = Date.now() - now1;
            if (duration > max) {
                max = duration;
            }
            acc = acc + duration;
        }
        console.log("Max cut duration:", max);
        console.log("Mean cut duration:", acc / 2000);

        var now1 = Date.now();
        maze = Haste.toMaze ( baseModel );
        var now2 = Date.now();
        console.log("To maze:", now2 - now1);

        mazeData = Haste.toMazeData ( baseModel, maze );
        flatModel = modelFromRaw ( Haste.toFlatModel ( baseModel ), mazeData );
        mazeModel = modelFromRaw ( Haste.mazeVerticeAndIds ( baseModel, maze ) );
        currentMesh = makeMesh ( flatModel );
        currentPathMesh = makeMesh( mazeModel );
        console.log("To mesh:", Date.now() - now2);

        oldMesh.geometry.dispose();
        oldPathMesh.geometry.dispose();
        oldMesh.material.dispose();
        oldPathMesh.material.dispose();

        var now3 = Date.now();
        scene.add( currentMesh );
        scene.add( currentPathMesh );
        console.log("To scene:", Date.now() - now3);

        console.log("64 cuts:", Date.now() - now);
    }
    $("#rndCuts").unbind("click");
    $("#rndCuts").click(rndCutsFct);

    var modelId = 2;
    var baseModel = Haste.loadModel ( modelId );
    var maze = Haste.toMaze ( baseModel );
    var mazeData = Haste.toMazeData ( baseModel, maze );
    var flatModel = modelFromRaw ( Haste.toFlatModel ( baseModel ), mazeData );
    var mazeModel = modelFromRaw ( Haste.mazeVerticeAndIds ( baseModel, maze ) );

    var zoomMax = 16;
    var zoomMin = 0.0625;
    var zoomSpeed = 1.01;
    var zoom = 1;
    
    var dummyCam = new DummyCamera();
    var camTheta = Math.PI / 2;
    var camPhi = Math.PI / 2;
    var camDist = 1;

    var projMat = new THREE.Matrix4();
    var viewMat = arrToMat( Haste.updateViewMat( camTheta, camPhi, camDist ) );
    var modelMat = new THREE.Matrix4();
    modelMat.elements[0] = Math.sqrt(2) / 2;
    modelMat.elements[2] = Math.sqrt(2) / 2;
    modelMat.elements[5] = 1;
    modelMat.elements[8] = -Math.sqrt(2) / 2;
    modelMat.elements[10] = Math.sqrt(2) / 2;
    var scaledModelMat = modelMat.clone();

    var mvp = new THREE.Matrix4();
    function updateMVPs() {
        var p = projMat.clone();
        var vp = p.multiply( viewMat );
        scaledModelMat = new THREE.Matrix4();
        scaledModelMat.multiplyScalar( zoom ).multiply( modelMat );
        scaledModelMat.elements[15] = 1;
        mvp = vp.multiply( scaledModelMat );
    }

    var updateProjection = function(screenWidth, screenHeight) {
        projMat = arrToMat( Haste.orthoMatrixFromScreen( screenWidth, screenHeight ) );
        updateMVPs();
    };
    
    updateProjection(window.innerWidth, window.innerHeight);

    var currentMesh =  makeMesh( flatModel );
    var currentPathMesh = makeMesh( mazeModel );

    var mainContainer = document.getElementById( 'main' );

    // pinch detection (and more)
    var mc = new Hammer(mainContainer);
    mc.get("pinch").set({ enable: true });

    mc.on("pinch", function(ev) {
        zoomFct(ev.scale < 1 ? -1 : 1, 1.04);
    });

    var scene = new THREE.Scene();
    var renderer = new THREE.WebGLRenderer( { preserveDrawingBuffer: true } );
    var canvas = renderer.domElement;
    renderer.setSize( window.innerWidth, window.innerHeight );
    mainContainer.appendChild( canvas );

    scene.add( currentMesh );
    scene.add( currentPathMesh );

    function leftButton(evt) {
        var button = evt.which || evt.button;
        return button == 1;
    }
    
    var mx, my = 0;

    var tapped = false; // double tap detection

    function onMouseDown(event) {
        if (leftButton(event)) {
            event.touches = [{clientX: event.clientX, clientY: event.clientY}];
            onTouchStart(event);
            clearTimeout(tapped);
            tapped = null; // double click is handled natively
        }
    }

    // only react to left clicks
    function onTouchStart(event) {
        // dont handle multi touch
        if (event.touches.length === 1) {
            mx = event.touches[0].clientX;
            my = event.touches[0].clientY;
            canvas.addEventListener( "mouseup", onMouseUp, false );
            canvas.addEventListener( "touchend", onTouchEnd, false );
            canvas.addEventListener( "mousemove", onMouseMove, false );
            canvas.addEventListener( "touchmove", onTouchMove, false );
            if(!tapped){ //if tap is not set, set up single tap
                tapped = setTimeout(function() {
                    tapped = null
                }, 300);   //wait 300ms then run single click code
            } else {    //tapped within 300ms of last tap. double tap
              clearTimeout(tapped); //stop single tap callback
              tapped = null;
              // nothing bound to double tap
            }
        }
    }

    function onMouseUp(event) {
        if (leftButton(event)) {
            onTouchEnd(event);
        }
    }

    function onTouchEnd(event) {
        canvas.removeEventListener( "mouseup", onMouseUp, false );
        canvas.removeEventListener( "touchend", onTouchEnd, false );
        canvas.removeEventListener( "mousemove", onMouseMove, false );
        canvas.removeEventListener( "touchmove", onTouchMove, false );
    }

    function onMouseMove(event) {
        event.touches = [{clientX: event.clientX, clientY: event.clientY}];
        onTouchMove(event);
    }

    // mouse drag -> move camera (adjusted to zoom)
    function onTouchMove(event) {
        // dont handle multi touch
        if (event.touches.length === 1) {
            event.preventDefault();
            var deltaX = event.touches[0].clientX - mx;
            var deltaY = event.touches[0].clientY - my;

            modelMat = arrToMat( Haste.naiveRotMat( deltaX * 0.005, deltaY * 0.005 ) ).multiply( modelMat );
            updateMVPs();

            mx = event.touches[0].clientX;
            my = event.touches[0].clientY;
            // no need to update cam, projection matrix is not changed by translation
        }
    }

    function zoomFct(delta, alpha) {
        zoom = Math.min( Math.max( zoom * ( Math.pow( alpha, delta ) ), zoomMin ), zoomMax );
        updateMVPs();
    }

    // mouse wheel -> zoom in / out
    function onMouseWheel(event) {
        zoomFct( Math.max( -1, Math.min( 1, ( event.wheelDelta || -event.detail ) ) ), 1.05 );
    }
    
    canvas.addEventListener( "mousedown", onMouseDown, false );
    canvas.addEventListener( "touchstart", onTouchStart, false );
    
    canvas.addEventListener( "mousewheel", onMouseWheel, false );
      // Firefox
    canvas.addEventListener( "DOMMouseScroll", onMouseWheel, false );

//    nothing bound to double click
//    canvas.addEventListener( "dblclick", cut, false );
    
    THREEx.WindowResize(renderer, updateProjection);
    THREEx.FullScreen.bindKey({ charCode : 'f'.charCodeAt(0) });

    // A cross-browser requestAnimationFrame
    // See https://hacks.mozilla.org/2011/08/animating-with-javascript-from-setinterval-to-requestanimationframe/
    var requestAnimFrame = (function() {
        return window.requestAnimationFrame    ||
            window.webkitRequestAnimationFrame ||
            window.mozRequestAnimationFrame    ||
            window.oRequestAnimationFrame      ||
            window.msRequestAnimationFrame     ||
            function(callback){
                window.setTimeout(callback, 1000 / 60);
            };
    })();

    // Don't run the game when the tab isn't visible
    window.addEventListener('focus', function() {
        unpause();
    });

    window.addEventListener('blur', function() {
        pause();
    });

    reset();
//    var then = Date.now();
    var running = true;
    main();


    // Functions ---

    // Reset game to original state
    function reset() {
    }

    // Pause and unpause
    function pause() {
        running = false;
    }

    function unpause() {
        running = true;
//        then = Date.now();
        main();
    }

    var simTime = 0.0;

    // The main game loop
    function main() {
        if(!running) {
            return;
        }

//        var now = Date.now();
//        var dt = now - then;

        if ( showSolid && depthMaze ) {
            currentMesh.material.uniforms.u_allowDepth.value = 1.0;
        } else {
            currentMesh.material.uniforms.u_allowDepth.value = 0.0;
        }
        currentMesh.material.uniforms.u_time.value = simTime;
        currentMesh.material.uniforms.u_borderWidth.value = 1.0;
        currentMesh.material.uniforms.u_mvpMat.value = mvp;
        if ( showSolid ) {
            currentMesh.material.uniforms.u_color.value = new THREE.Vector4(1,1,1,1);
            currentMesh.material.uniforms.u_borderColor.value = new THREE.Vector4(0.05,0.05,0.05,1);
        } else {
            currentMesh.material.uniforms.u_color.value = new THREE.Vector4(0,0,0,1);
            currentMesh.material.uniforms.u_borderColor.value = new THREE.Vector4(0,0,0,1);
        }

        if ( showMazePath ) {
            scene.add( currentPathMesh );
            currentPathMesh.material.uniforms.u_borderWidth.value = 0.0;
            currentPathMesh.material.uniforms.u_mvpMat.value = mvp;
            currentPathMesh.material.uniforms.u_color.value = new THREE.Vector4(1,0,0,1);
            currentPathMesh.material.uniforms.u_borderColor.value = new THREE.Vector4(1,0,0,1);
        } else {
            scene.remove( currentPathMesh );
        }

        renderer.render(scene, dummyCam);

//        then = now;
        requestAnimFrame(main);
    }


};

window.onload = appMain;