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
       u_borderWidth: { type: "1f", value: 0 },
       u_mvpMat: { type: "m4", value: new THREE.Matrix4() },
       u_color: { type: "v4", value: new THREE.Vector4( 1, 1, 1, 1 ) },
       u_borderColor: { type: "v4", value: new THREE.Vector4( 0, 0, 0, 1 ) }
    };
    
    var geometry = new THREE.Geometry();
    
    // create attributes for each vertex
    // currently 2 colors are given as vertex attributes
    var attributes = {
        a_normal: {
             type: 'v3',
             value: []
        },
        a_barycentrics: {
            type: 'v3',
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

    for (var i = 0; i < model.vertice.length / 3; i++) {
        geometry.vertices.push( new THREE.Vector3( model.vertice[3*i], model.vertice[3*i+1], model.vertice[3*i+2] ) );
    }
    for (var i = 0; i < model.normals.length / 3; i++) {
        attributes.a_normal.value.push( new THREE.Vector3( model.normals[3*i], model.normals[3*i+1], model.normals[3*i+2] ) );
    }
    for (var i = 0; i < model.centers.length / 3; i++) {
        attributes.a_barycentrics.value.push( new THREE.Vector3( model.centers[3*i], model.centers[3*i+1], model.centers[3*i+2] ) );
    }
    for (var i = 0; i < model.indice.length / 3; i++) {
        geometry.faces.push( new THREE.Face3( model.indice[3*i], model.indice[3*i+1], model.indice[3*i+2] ) );
    }
    return new THREE.Mesh( geometry, shaderMaterial );
}

function modelFromRaw(model) {
    return {
        "vertice": model[0],
        "normals": model[1],
        "centers": model[2],
        "indice": model[3],
        "vpf": model[4],
        "span": model[5],
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
            height: 240
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

    var showPlane = true;
    function showPlaneFct() {
        showPlane = !showPlane;
        if ( showPlane ) {
            scene.add( planeMesh );
        } else {
            scene.remove( planeMesh );
        }
    }
    $("#showPlane").unbind("click");
    $("#showPlane").click(showPlaneFct);
    $('#showPlane').attr('checked', false);

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

    function cut() {
        oldMesh = currentMesh;
        scene.remove( oldMesh );

        baseModel = Haste.truncate ( baseModel
                                   , scaledModelMat.clone().transpose().elements);
        flatModel = modelFromRaw ( Haste.toFlatModel ( baseModel ) );
        currentMesh = makeMesh ( flatModel );

        oldMesh.geometry.dispose();
        oldMesh.material.dispose();

        // model span changes, need to recompute matrices
        updateMVPs();

        scene.add( currentMesh );
    }
    $("#cut").unbind("click");
    $("#cut").click(cut);

    function rndCutsFct() {
        oldMesh = currentMesh;
        scene.remove( oldMesh );

        for (var index = 0; index < 10; index++) {
            var cutSeed = rndSpherePosition();
            baseModel = Haste.truncateAtPoint ( baseModel
                                              , cutSeed.theta
                                              , cutSeed.phi);
        }
        flatModel = modelFromRaw ( Haste.toFlatModel ( baseModel ) );
        currentMesh = makeMesh ( flatModel );

        oldMesh.geometry.dispose();
        oldMesh.material.dispose();

        // model span changes, need to recompute matrices
        updateMVPs();

        scene.add( currentMesh );
    }
    $("#rndCuts").unbind("click");
    $("#rndCuts").click(rndCutsFct);

    var modelId = 2;
    var baseModel = Haste.loadModel ( modelId );
    var flatModel = modelFromRaw ( Haste.toFlatModel ( baseModel ) );

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
    var planeMvp = new THREE.Matrix4();
    function updateMVPs() {
        var p = projMat.clone();
        var vp = p.multiply( viewMat );
        planeMvp = vp.clone();
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

    var planeMesh = makeMesh ( {
        "vertice": [ 0, 0, 1.00001
                   , 2, -2, 1.00001
                   , 2, 2, 1.00001
                   , -2, 2, 1.00001
                   , -2, -2, 1.00001
                   ],
        "normals": [ 1, 0, 0
                   , 1, 0, 0
                   , 1, 0, 0
                   , 1, 0, 0
                   , 1, 0, 0
                   ],
        "indice": [ 0, 1, 2
                  , 0, 2, 3
                  , 0, 3, 4
                  , 0, 4, 1
                  ],
        "centers": [ 1, 1, 1
                   , 1, 0, 0
                   , 0, 1, 0
                   , 1, 0, 0
                   , 0, 1, 0]
    } );
    planeMesh.material.transparent = true;
    var currentMesh =  makeMesh( flatModel );

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
    scene.add( planeMesh );
    
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
              cut();
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

    canvas.addEventListener( "dblclick", cut, false );
    
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

        currentMesh.material.uniforms.u_time.value = simTime;
        currentMesh.material.uniforms.u_borderWidth.value = 1.2;
        currentMesh.material.uniforms.u_mvpMat.value = mvp;
        currentMesh.material.uniforms.u_color.value = new THREE.Vector4(1,1,1,1);
        currentMesh.material.uniforms.u_borderColor.value = new THREE.Vector4(0.4,0.4,0.4,1);

        if (showPlane) {
            planeMesh.material.uniforms.u_time.value = simTime;
            planeMesh.material.uniforms.u_borderWidth.value = 5;
            planeMesh.material.uniforms.u_mvpMat.value = planeMvp;
            planeMesh.material.uniforms.u_color.value = new THREE.Vector4(0.8, 0.3, 0.3, 0.5);
            planeMesh.material.uniforms.u_borderColor.value = new THREE.Vector4(1,0,0,1);
        }

        renderer.render(scene, dummyCam);

//        then = now;
        requestAnimFrame(main);
    }


};

window.onload = appMain;