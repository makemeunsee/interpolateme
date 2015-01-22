function getURLParameter(sParam) {
    var sPageURL = window.location.search.substring(1);
    var sURLVariables = sPageURL.split('&');
    for (var i = 0; i < sURLVariables.length; i++) {
        var sParameterName = sURLVariables[i].split('=');
        if (sParameterName[0] == sParam) {
            return sParameterName[1];
        }
    }
}

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
        u_depthMode: { type: "1f", value: 0 },
        u_depthScale: { type: "1f", value: 0 },
        u_explodedFactor: { type: "1f", value: 0 },
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
    for (var i = 0; i < model.vertice.length / 3; i++) {
        positions [ 3*i ] = model.vertice[ 3*i ];
        normals [ 3*i ] = model.normals[ 3*i ];
        positions [ 3*i+1 ] = model.vertice[ 3*i+1 ];
        normals [ 3*i+1 ] = model.normals[ 3*i+1 ];
        positions [ 3*i+2 ] = model.vertice[ 3*i+2 ];
        normals [ 3*i+2 ] = model.normals[ 3*i+2 ];
        centerFlag[ i ] = model.centers[ i ];
        mazeDepth[ i ] = model.mazeData[ i ];
    }
    var indices = new Uint16Array( model.indice.length );
    for (var i = 0; i < model.indice.length ; i++) {
        indices[ i ] = model.indice[ i ];
    }

    geometry.addAttribute( 'index', new THREE.BufferAttribute( indices, 1 ) );
    geometry.addAttribute( 'a_mazeDepth', new THREE.BufferAttribute( mazeDepth, 1 ) );
    geometry.addAttribute( 'a_centerFlag', new THREE.BufferAttribute( centerFlag, 1 ) );
    geometry.addAttribute( 'a_normal', new THREE.BufferAttribute( normals, 3 ) );
    geometry.addAttribute( 'position', new THREE.BufferAttribute( positions, 3 ) );

    return new THREE.Mesh( geometry, shaderMaterial );
}

function modelFromRaw( model ) {
    return {
        "vertice": model[0],
        "normals": model[1],
        "centers": model[2],
        "mazeData": model[3],
        "indice": model[4],
    };
}


DummyCamera = function () {
    THREE.Camera.call( this );
    this.type = 'DummyCamera';
};

DummyCamera.prototype = Object.create( THREE.Camera.prototype );

function appMain() {

    var seed = getURLParameter("seed") || Math.random().toString().slice(2);
    console.log("seed:\t", seed);

    function rndSpherePosition() {
        var u = Math.random();
        var v = Math.random();
        return [ 2*Math.PI*u, Math.acos( 2*v - 1 ) ];
    }

    // help dialog
    $(function() {
        $( "#dialog" ).dialog({
            width: 600,
            height: 180
        });
    });
    $( "#dialog" ).dialog( "close" );

    function toggleUI() {
        console.log("toggling ui");
        if ( $( "#gui" ).css( "display" ) === "none" )
            $( "#gui" ).show();
        else
            $( "#gui" ).hide();
    }

    // jqueryui widgets
    $(function() {
      $( "button" ).button();
    });

    var depthMaze = true;
    function flatFct() {
        depthMaze = !depthMaze;
    }
    $("#flat").unbind("click");
    $("#flat").click(flatFct);
    $("#flat").attr("checked", false);

    var reverseDepth = false;
    function reverseDepthFct() {
        reverseDepth = !reverseDepth;
    }
    $("#reverseDepth").unbind("click");
    $("#reverseDepth").click(reverseDepthFct);
    $("#reverseDepth").attr("checked", false);

    function showHelp() {
        if ( $( "#dialog" ).dialog( "isOpen" ) )
            $( "#dialog" ).dialog( "close" );
        else
            $( "#dialog" ).dialog( "open" );
    }
    $("#showHelp").unbind("click");
    $("#showHelp").click(showHelp);

    var depthExagg = 0.5;
    var explosion = 0.0;

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

    var cuts = getURLParameter("cells") || 0;
    console.log( "cells:\t", cuts );
    var cutPositions = new Array(2*cuts);
    for ( var i = 0; i < cuts; i++ ) {
        var cut = rndSpherePosition();
        cutPositions[2*i] = cut[0];
        cutPositions[2*i+1] = cut[1];
    }

    var id = getURLParameter( "polyId" ) || 3;
    console.log( "polyId:\t", id );
    var overlapThreshold = getURLParameter( "overlapThreshold" ) || 100;
    console.log( "overlapThreshold:\t", overlapThreshold );
    var model = modelFromRaw ( Haste.createMazePlanet ( id, seed, cutPositions, overlapThreshold ) );

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

    var currentMesh =  makeMesh( model );

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
              toggleUI()
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

    canvas.addEventListener( "dblclick", toggleUI, false );
    
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

        if ( depthMaze && reverseDepth ) {
            currentMesh.material.uniforms.u_depthMode.value = -1.0;
        } else if ( depthMaze && !reverseDepth ) {
            currentMesh.material.uniforms.u_depthMode.value = 1.0;
        } else {
            currentMesh.material.uniforms.u_depthMode.value = 0.0;
        }
        currentMesh.material.uniforms.u_explodedFactor.value = explosion;
        currentMesh.material.uniforms.u_depthScale.value = depthExagg;
        currentMesh.material.uniforms.u_time.value = simTime;
        currentMesh.material.uniforms.u_borderWidth.value = 1.0;
        currentMesh.material.uniforms.u_mvpMat.value = mvp;
        currentMesh.material.uniforms.u_color.value = new THREE.Vector4(1,1,1,1);
        currentMesh.material.uniforms.u_borderColor.value = new THREE.Vector4(0.05,0.05,0.05,1);

        renderer.render(scene, dummyCam);

//        then = now;
        requestAnimFrame(main);
    }


};

window.onload = appMain;