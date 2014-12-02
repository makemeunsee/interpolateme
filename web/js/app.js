function arrToMat( arr ) {
    var mat = new THREE.Matrix4();
    mat.set( arr[0], arr[1], arr[2], arr[3],
             arr[4], arr[5], arr[6], arr[7],
             arr[8], arr[9], arr[10], arr[11],
             arr[12], arr[13], arr[14], arr[15] );
    return mat;
}

function makeMesh( vertice, normals, centers, indice ) {
    var customUniforms = {
       u_time: { type: "1f", value: 0 },
       u_mvpMat: { type: "m4", value: new THREE.Matrix4() },
       u_lightDirection: { type: "v4", value: new THREE.Vector4( 1, 1, 1, 1 ) },
       u_lightIntensity: { type: "1f", value: 1 },
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
        // alt_position: {
        //     type: '3fv',
        //     value: []
        // },
        a_barycentric: {
            type: '1i',
            value: []
        }
    };
    
    var shaderMaterial = new THREE.ShaderMaterial({
      attributes:     attributes,
      uniforms:       customUniforms,
      vertexShader:   document.getElementById('shader-vs').innerHTML,
      fragmentShader: document.getElementById('shader-fs').innerHTML
    });
     
    for (var i = 0; i < vertice.length / 3; i++) {
        geometry.vertices.push( new THREE.Vector3( vertice[3*i], vertice[3*i+1], vertice[3*i+2] ) );
    }
    for (var i = 0; i < normals.length / 3; i++) {
        attributes.a_normal.value.push( new THREE.Vector3( normals[3*i], normals[3*i+1], normals[3*i+2] ) );
    }
    for (var i = 0; i < centers.length; i++) {
        attributes.a_barycentric.value.push( centers[i] );
    }
    for (var i = 0; i < indice.length / 3; i++) {
        geometry.faces.push( new THREE.Face3( indice[3*i], indice[3*i+1], indice[3*i+2] ) );
    }
    return new THREE.Mesh( geometry, shaderMaterial );
}

function modelFor(id) {
    return {
        "id": id,
        "span": Haste.spanOf( id ),
        "mesh": makeMesh( Haste.verticeOf( id ),
                          Haste.normalsOf( id ), 
                          Haste.centersOf( id ), 
                          Haste.indiceOf( id ) )
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

    // help dialog
    $(function() {
        $( "#dialog" ).dialog({
            width: 475,
            height: 200
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
      $( "#grabLight" ).button();
    });

    var grabLight = false;
    $( "#grabLight" ).siblings('label').html("Grab light");
    $( "#grabLight" ).change(function() {
        if ( this.checked ) {
            grabLight = false;
            $(this).siblings('label').html("Grab light");
        } else {
            grabLight = true;
            $(this).siblings('label').html("Release light");
        }
    });

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
    
    var modelId = 8;

    function previousModel() {
        modelId = (modelId - 1 + Haste.modelsLength()) % Haste.modelsLength();
        switchModel();
    }
    $("#prevModel").unbind("click");
    $("#prevModel").click(previousModel);

    function nextModel() {
        modelId = (modelId + 1) % Haste.modelsLength();
        switchModel();
    }
    $("#nextModel").unbind("click");
    $("#nextModel").click(nextModel);

    var model = modelFor( modelId );

    function switchModel() {
        scene.remove( model.mesh );
        model.mesh.geometry.dispose();
        model.mesh.material.dispose();

        model = modelFor( modelId );
        scene.add( model.mesh );
    }

    var zoomMax = 8;
    var zoomMin = 0.125;
    var zoomSpeed = 1.1;
    var zoom = 1;
    
    var dummyCam = new DummyCamera();
    var camTheta = Math.PI / 2;
    var camPhi = Math.PI / 2;
    var camDist = model.span * 1.1;

    var lightTheta = 1.75;
    var lightPhi = 1.75;
    var lightDist = 1;

    var projMat = new THREE.Matrix4();
    var viewMat = arrToMat( Haste.updateViewMat( camTheta, camPhi, camDist ) );
    var modelMat = new THREE.Matrix4();
    
    var updateProjection = function(screenWidth, screenHeight) {
        projMat = arrToMat( Haste.orthoMatrixFromScreen( screenWidth, screenHeight ) );
    };
    
    updateProjection(window.innerWidth, window.innerHeight);
    
    var mainContainer = document.getElementById( 'main' );
    
    var scene = new THREE.Scene();
    var renderer = new THREE.WebGLRenderer( { preserveDrawingBuffer: true } );
    var canvas = renderer.domElement;
    renderer.setSize( window.innerWidth, window.innerHeight );
    mainContainer.appendChild( canvas );

    scene.add( model.mesh );
    
    function leftButton(evt) {
        var button = evt.which || evt.button;
        return button == 1;
    }
    
    var mx, my = 0;
    
    // only react to left clicks
    function onMouseDown(event) {
        if (leftButton(event)) {
            mx = event.clientX;
            my = event.clientY;
            canvas.addEventListener( 'mouseup', onMouseUp, false );
            canvas.addEventListener( 'mousemove', onMouseMove, false );
        }
    }
    
    function onMouseUp(event) {
        if (leftButton(event)) {
            canvas.removeEventListener( 'mouseup', onMouseUp, false );
            canvas.removeEventListener( 'mousemove', onMouseMove, false );
        }
    }
    
    // mouse drag -> move camera (adjusted to zoom)
    function onMouseMove(event) {
        var deltaX = event.clientX - mx;
        var deltaY = event.clientY - my;

        if (grabLight) {
            lightTheta = lightTheta + deltaX * 0.005;
            lightPhi = lightPhi - deltaY * 0.005;
        } else {
            camTheta = camTheta + deltaX * 0.005;
            camPhi = camPhi - deltaY * 0.005;
            viewMat = arrToMat( Haste.updateViewMat( camTheta, camPhi, camDist ) );
        }

        mx = event.clientX;
        my = event.clientY;
        // no need to update cam, projection matrix is not changed by translation
    }
    
    // mouse wheel -> zoom in / out
    function onMouseWheel(event) {
        var delta = Math.max(-1, Math.min(1, (event.wheelDelta || -event.detail)));
        if (grabLight) {
            lightDist = lightDist / Math.pow( 1.05, delta );
        } else {
            zoom = Math.min( Math.max( zoom * ( Math.pow( 1.1, delta ) ), zoomMin ), zoomMax );
        }
    }
    
    canvas.addEventListener( "mousedown", onMouseDown, false );
    
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
    var then = Date.now();
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
        then = Date.now();
        main();
    }

    // The main game loop
    function main() {
        if(!running) {
            return;
        }

        var scaledProj = projMat.clone().multiplyScalar( 1/model.span );
        scaledProj.elements[15] = 1;
        var zoomedModel = new THREE.Matrix4();
        zoomedModel.multiplyScalar( zoom ).multiply( modelMat );
        zoomedModel.elements[15] = 1;
        var mvp = scaledProj.multiply( viewMat ).multiply( zoomedModel );

        var lightDir = Haste.directionFromOrigin(lightTheta, lightPhi, lightDist);

        var now = Date.now();
        var dt = now - then;
        // shaders use the current time to animate properly
        model.mesh.material.uniforms.u_time.value += dt;
        model.mesh.material.uniforms.u_mvpMat.value = mvp;
        model.mesh.material.uniforms.u_lightDirection.value = new THREE.Vector4( lightDir[0]/lightDist,
                                                                                 lightDir[1]/lightDist,
                                                                                 lightDir[2]/lightDist);
        model.mesh.material.uniforms.u_lightIntensity.value = 1 / lightDist / lightDist;
        model.mesh.material.uniforms.u_color.value = new THREE.Vector4(1,1,1,1);
        model.mesh.material.uniforms.u_borderColor.value = new THREE.Vector4(0.4,0.4,0.4,1);

        renderer.render(scene, dummyCam);

        then = now;
        requestAnimFrame(main);
    }


};

window.onload = appMain;