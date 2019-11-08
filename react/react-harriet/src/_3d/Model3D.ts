import { OrbitControls } from "three/examples/jsm/controls/OrbitControls";
import { CSS2DRenderer, CSS2DObject } from 'three/examples/jsm/renderers/CSS2DRenderer.js';
import { GLTFLoader, GLTF } from 'three/examples/jsm/loaders/GLTFLoader';

import * as THREE from "three";
import { Quaternion, ReinhardToneMapping, Vector3 } from "three";
import { SystemObject } from "../system/model";

interface Waypoint {
    position : THREE.Vector3 | null,
    lookAt   : THREE.Vector3,
    up       : THREE.Vector3,
    duration : number,
}

interface CachedModelTable {
    [key : string] : GLTF
}

var cachedModels : CachedModelTable = {}

  interface ObjectTable {
    [key : string] : SystemObject
  }
  
  export default class Model3D {

    readonly scene: THREE.Scene;
    readonly camera: THREE.Camera;
    readonly renderer: THREE.Renderer;
    readonly labelRenderer: CSS2DRenderer;
    readonly textureLoader: THREE.TextureLoader;
    readonly controls : OrbitControls;
    readonly labelDiv : HTMLDivElement;
    readonly light : THREE.Light;
    readonly modelLoader : GLTFLoader;

    timerId = 0;
    renderCount = 0;

    objects : ObjectTable = {};

    requestID : number = 0
  
    travel            : Waypoint[] = []
    travelIndex       : number = 0
    travelStart       : THREE.Vector3
    travelEnd         : THREE.Vector3
    travelLookEnd     : THREE.Vector3
    travelStartQuat   : THREE.Quaternion
    travelEndQuat     : THREE.Quaternion
    travelDuration    : number = 0
    travelProgress    : number = 0

    constructor(mount : any, cameraNear : number, cameraFar : number, orbitNear : number, orbitFar : number) {
        this.scene = new THREE.Scene();
        const itemElement = mount.closest(".concorde-dashboard-item");
  
        const width = itemElement.clientWidth; 
        const height = itemElement.clientHeight - 30;
  
        this.camera = new THREE.PerspectiveCamera( 60, width / height, cameraNear, cameraFar );

        this.renderer = new THREE.WebGLRenderer({ 
            antialias: true,
        });
        this.renderer.setSize(width, height);
        mount.appendChild( this.renderer.domElement );

        this.labelRenderer = new CSS2DRenderer();
        this.labelRenderer.setSize( width, height );
        this.labelRenderer.domElement.style.position = 'absolute';
        this.labelRenderer.domElement.style.top = '0';
        mount.appendChild( this.labelRenderer.domElement );
    
        this.labelDiv = document.createElement( 'div' );
        this.labelDiv.className = 'label';
        this.labelDiv.textContent = 'test label';

        this.controls = new OrbitControls( this.camera, this.labelRenderer.domElement );
        this.controls.enableDamping = true; // an animation loop is required when either damping or auto-rotation are enabled
        this.controls.dampingFactor = 0.05;

        this.controls.screenSpacePanning = false;

        this.controls.minDistance = orbitNear;
        this.controls.maxDistance = orbitFar;

        this.controls.maxPolarAngle = Math.PI;

        this.light = new THREE.DirectionalLight();

        this.camera.position.z = (cameraFar - cameraNear) / 2;
        
        this.travelStart = this.travelEnd = this.camera.position;
        this.travelStartQuat = this.travelEndQuat = new THREE.Quaternion(0, 0, 0, 0);
        this.travelLookEnd = new THREE.Vector3(0, 0, 0);

        this.textureLoader = new THREE.TextureLoader();
        this.modelLoader = new GLTFLoader();

        this.timerId = setInterval(() => { console.log("fps", this.renderCount / 10); this.renderCount = 0}, 10000);
      }

  addObject = (obj : SystemObject, mesh : THREE.Mesh, label : CSS2DObject) : void => {          
    this.objects[obj.id] = obj;
  }

   startTravel = () : void => {
        this.travelStart = this.camera.position.clone();
        this.travelStartQuat = this.camera.quaternion.clone();

        const { position, duration, lookAt } = this.travel[this.travelIndex];
        this.travelEnd = position || this.travelStart.clone();

        const lookObject = new THREE.Object3D();
        lookObject.position.copy(this.travelEnd);
        lookObject.up.set(0, 1, 0);
        lookObject.lookAt(-lookAt.x, -lookAt.y, -lookAt.z);
        console.log('startTravel', lookObject.position, lookObject.up, lookObject.quaternion);
        this.travelLookEnd = lookAt.clone();
        this.travelEndQuat = lookObject.quaternion.clone();
        this.travelDuration = duration * 60.0;
        this.travelProgress = 0;
        console.log('travel', this.travelStart, this.travelEnd, this.travelDuration, this.travelLookEnd);
    }

   updateTravel = () : void => {
    this.travelProgress += 1
    if (this.travelProgress >= this.travelDuration) {
        this.camera.position.set (this.travelEnd.x, this.travelEnd.y, this.travelEnd.z)   
        this.camera.quaternion.copy(this.travelEndQuat);
        console.log('endTravel', this.camera.position, this.camera.up, this.camera.quaternion);
        this.travelIndex += 1
        if (this.travelIndex >= this.travel.length) {
            this.travel = [];
            return;
        } else {
            this.startTravel();
        }
    } else {
        const v1 = this.travelStart;
        const v2 = this.travelEnd;
        const d = this.travelDuration;
        const unitProgress = this.travelProgress / d;
        const f = unitProgress < 0.5 ? 4 * unitProgress ** 3 : 1 - (4 * (1 - unitProgress) ** 3);
        let v : THREE.Vector3 = v2.clone();
        v.sub(v1);
        v.multiplyScalar(f);
        v.add(v1);
        this.camera.position.set(v.x, v.y, v.z);
        this.camera.quaternion.copy(this.travelStartQuat.clone().slerp(this.travelEndQuat, f));
    }
   }

   startAnimationLoop(beforeRender: () => void) {
        const animate = () => {
            this.requestID = requestAnimationFrame(animate);
            beforeRender();
            if (this.travel.length > 0) {
                this.updateTravel();
            }
            this.renderer!.render(this.scene!, this.camera!);
            this.labelRenderer!.render(this.scene!, this.camera!);
            ++this.renderCount;
        }
        animate();
    }

   addWaypoint = (position : THREE.Vector3 | null, lookAt : THREE.Vector3, duration : number) : void => {
       this.travel.push({
        position,
        lookAt,
        up       : this.camera.up,
        duration,
       });
       if (this.travel.length === 1) {
           this.travelIndex = 0;
           this.startTravel();
       }
   }

   stopAnimationLoop() {
    window.cancelAnimationFrame(this.requestID);
    clearInterval(this.timerId);
   }

   getModel = (name : string, callback : (model : GLTF) => void) : void => {
       if (!(name in cachedModels)) {
           const saveModel = (model : GLTF) => {
                cachedModels[name] = model;
                callback (model);
           }
           const loadError = (event : ErrorEvent) => {
               console.log(event)
               alert(event.message);
           }
           this.modelLoader.load('models/' + name + '.gltf', saveModel, undefined, loadError);
       } else {
           callback(cachedModels[name]);
       }
   }
      
}