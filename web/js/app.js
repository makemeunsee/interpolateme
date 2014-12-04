function arrToMat( arr ) {
    var mat = new THREE.Matrix4();
    mat.set( arr[0], arr[1], arr[2], arr[3],
             arr[4], arr[5], arr[6], arr[7],
             arr[8], arr[9], arr[10], arr[11],
             arr[12], arr[13], arr[14], arr[15] );
    return mat;
}

function interpolate(from, to, alpha) {
    var res = [];
    var a = 0.5 + 0.5 * Math.cos(alpha);
    for (var i = 0; i < from.length; i++) {
       res[i] = new THREE.Vector3( a * from[i].x + (1-a) * to[i].x,
                                   a * from[i].y + (1-a) * to[i].y,
                                   a * from[i].z + (1-a) * to[i].z)
    }
    return res;
}

function rndFaces(vertice, span, vpf, lookDir, prng ) {
    var rnds = [];
    for (var i = 0; i < vpf.length; i++) {
        rnds[i] = prng() * 2 * span - span;
    }
    return Haste.rndFacesAlongAxis(rnds, lookDir[0], lookDir[1], lookDir[2], vertice, vpf);
}

function makeMesh( model, lookDir, prng ) {
    var customUniforms = {
       u_alpha: { type: "1f", value: 0 },
       u_mvpMat: { type: "m4", value: new THREE.Matrix4() },
       u_vMat: { type: "m4", value: new THREE.Matrix4() },
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
        alt_position: {
             type: 'v3',
             value: []
        },
        a_barycentric: {
            type: '1i',
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

    var static = prng === true;
    var randomizedFaces = [];
    var interpolatedFaces = [];
    if (!static) {
        randomizedFaces = rndFaces( model.vertice, model.span, model.vpf, lookDir, prng);
    }

    geometry.dynamic = true;
    for (var i = 0; i < model.vertice.length / 3; i++) {
        if (static) {
            attributes.alt_position.value.push( new THREE.Vector3( model.vertice[3*i], model.vertice[3*i+1], model.vertice[3*i+2] ) );
            geometry.vertices.push( new THREE.Vector3( model.vertice[3*i], model.vertice[3*i+1], model.vertice[3*i+2] ) );
        } else {
            geometry.vertices.push( new THREE.Vector3( randomizedFaces[3*i], randomizedFaces[3*i+1], randomizedFaces[3*i+2] ) );
            attributes.alt_position.value.push( new THREE.Vector3( randomizedFaces[3*i], randomizedFaces[3*i+1], randomizedFaces[3*i+2] ) );
        }
    }
    for (var i = 0; i < model.normals.length / 3; i++) {
        attributes.a_normal.value.push( new THREE.Vector3( model.normals[3*i], model.normals[3*i+1], model.normals[3*i+2] ) );
    }
    for (var i = 0; i < model.centers.length; i++) {
        attributes.a_barycentric.value.push( model.centers[i] );
    }
    for (var i = 0; i < model.indice.length / 3; i++) {
        geometry.faces.push( new THREE.Face3( model.indice[3*i], model.indice[3*i+1], model.indice[3*i+2] ) );
    }
    return new THREE.Mesh( geometry, shaderMaterial );
}

function modelFor(id) {
    return {
        "id": id,
        "vpf": Haste.verticeCountPerFaceOf( id ),
        "span": Haste.spanOf( id ),
        "vertice": Haste.verticeOf( id ),
        "normals": Haste.normalsOf( id ),
        "centers": Haste.centersOf( id ),
        "indice": Haste.indiceOf( id )
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
    });

    var grabLight = false;
    $( "#grabLight" ).change(function() {
        if ( this.checked ) {
            grabLight = true;
        } else {
            grabLight = false;
        }
    });
    $('#grabLight').attr('checked', false);

    var static = false;
    $( "#static" ).change(function() {
        if ( this.checked ) {
            static = true;
            switchModel();
        } else {
            static = false;
        }
    });
    $('#static').attr('checked', false);

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

    function switchModel() {
        oldMesh = currentMesh;
        scene.remove( oldMesh );

        model = modelFor( modelId );
        currentMesh = makeMesh( model,
                                Haste.normedDirectionToOrigin(camTheta, camPhi),
                                static || prng );

        oldMesh.geometry.dispose();
        oldMesh.material.dispose();

        scene.add( currentMesh );
        simTime = 0;
        then = Date.now();
    }

    var modelId = 8;
    var model = modelFor( modelId );

    var zoomMax = 8;
    var zoomMin = 0.125;
    var zoomSpeed = 1.1;
    var zoom = 1;
    
    var dummyCam = new DummyCamera();
    var camTheta = Math.PI / 2;
    var camPhi = Math.PI / 2;
    var camDist = model.span * 1.1;

    var lightTheta = -1.2;
    var lightPhi = 1.8;
    var lightDist = 1;

    var projMat = new THREE.Matrix4();
    var viewMat = arrToMat( Haste.updateViewMat( camTheta, camPhi, camDist ) );
    var modelMat = new THREE.Matrix4();
    
    var updateProjection = function(screenWidth, screenHeight) {
        projMat = arrToMat( Haste.orthoMatrixFromScreen( screenWidth, screenHeight ) );
    };
    
    updateProjection(window.innerWidth, window.innerHeight);
    
    var currentMesh =  makeMesh( model,
                                 Haste.normedDirectionToOrigin(camTheta, camPhi),
                                 prng );

    var mainContainer = document.getElementById( 'main' );
    
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

    // only react to left clicks
    function onMouseDown(event) {
        if (leftButton(event)) {
            event.touches = [{clientX: event.clientX, clientY: event.clientY}];
            onTouchStart(event);
        }
    }

    function onTouchStart(event) {
        mx = event.touches[0].clientX;
        my = event.touches[0].clientY;
        canvas.addEventListener( "mouseup", onMouseUp, false );
        canvas.addEventListener( "touchend", onTouchEnd, false );
        canvas.addEventListener( "mousemove", onMouseMove, false );
        canvas.addEventListener( "touchmove", onTouchMove, false );
        timeFlowing = grabLight;
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
        if (!grabLight && !static) {
            var interpolated = interpolate( currentMesh.geometry.vertices,
                                            currentMesh.material.attributes.alt_position.value,
                                            simTime );
            var newRnd = rndFaces( model.vertice,
                                   model.span,
                                   model.vpf,
                                   Haste.normedDirectionToOrigin(camTheta, camPhi),
                                   prng);
            for (var i = 0; i < model.vertice.length / 3; i++) {
                currentMesh.geometry.vertices[i] = interpolated[i];
                currentMesh.material.attributes.alt_position.value[i] = new THREE.Vector3( newRnd[3*i], newRnd[3*i+1], newRnd[3*i+2] );
            }
            currentMesh.geometry.verticesNeedUpdate = true;
            currentMesh.material.attributes.alt_position.needsUpdate = true;
            simTime = 0;
        }
        timeFlowing =  true;
    }

    function onMouseMove(event) {
        event.touches = [{clientX: event.clientX, clientY: event.clientY}];
        onTouchMove(event);
    }

    // mouse drag -> move camera (adjusted to zoom)
    function onTouchMove(event) {
        event.preventDefault();
        var deltaX = event.touches[0].clientX - mx;
        var deltaY = event.touches[0].clientY - my;

        if (grabLight) {
            lightTheta = lightTheta - deltaX * 0.005;
            lightPhi = Math.min( Math.PI - 0.01, Math.max( 0.01, lightPhi - deltaY * 0.005 ) );
        } else {
            camTheta = camTheta + deltaX * 0.005;
            camPhi = Math.min( Math.PI - 0.01, Math.max( 0.01, camPhi - deltaY * 0.005 ) );
            viewMat = arrToMat( Haste.updateViewMat( camTheta, camPhi, camDist ) );
        }

        mx = event.touches[0].clientX;
        my = event.touches[0].clientY;
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

    var simTime = 0.0;
    var timeFlowing = true;

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

        if (timeFlowing) {
            simTime = Math.min(simTime+dt/1000, Math.PI);
        }

        // shaders use the current time to animate properly
        currentMesh.material.uniforms.u_alpha.value = 0.5 + 0.5*Math.cos(simTime);
        currentMesh.material.uniforms.u_mvpMat.value = mvp;
        currentMesh.material.uniforms.u_vMat.value = viewMat;
        currentMesh.material.uniforms.u_lightDirection.value = new THREE.Vector4( lightDir[0]/lightDist,
                                                                                  lightDir[1]/lightDist,
                                                                                  lightDir[2]/lightDist);
        currentMesh.material.uniforms.u_lightIntensity.value = 1 / lightDist / lightDist;
        currentMesh.material.uniforms.u_color.value = new THREE.Vector4(1,1,1,1);
        currentMesh.material.uniforms.u_borderColor.value = new THREE.Vector4(0.4,0.4,0.4,1);

        renderer.render(scene, dummyCam);

        then = now;
        requestAnimFrame(main);
    }


};

window.onload = appMain;