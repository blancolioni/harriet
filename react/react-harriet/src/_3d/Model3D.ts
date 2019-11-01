import { OrbitControls } from "three/examples/jsm/controls/OrbitControls";
import { CSS2DRenderer } from 'three/examples/jsm/renderers/CSS2DRenderer.js';

import * as THREE from "three";

export default class Model3D {

    readonly scene: THREE.Scene;
    readonly camera: THREE.Camera;
    readonly renderer: THREE.Renderer;
    readonly labelRenderer: CSS2DRenderer;
    readonly textureLoader: THREE.TextureLoader;
    readonly controls : OrbitControls;
    requestID : number = 0
  
    constructor(mount : any) {
        this.scene = new THREE.Scene();
        const itemElement = mount.closest(".concorde-dashboard-item");
        console.log("model3d", itemElement.clientWidth, itemElement.clientHeight);
  
        const width = itemElement.clientWidth; 
        const height = itemElement.clientHeight - 30;
  
        this.camera = new THREE.PerspectiveCamera( 75, width / height, 0.1, 1000 );
  
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
    
        this.controls = new OrbitControls( this.camera, this.labelRenderer.domElement );
        this.controls.enableDamping = true; // an animation loop is required when either damping or auto-rotation are enabled
        this.controls.dampingFactor = 0.05;

        this.controls.screenSpacePanning = false;

        this.controls.minDistance = 5;
        this.controls.maxDistance = 50

        this.controls.maxPolarAngle = Math.PI / 2;

        this.camera.position.z = 2;

        this.textureLoader = new THREE.TextureLoader();
      }

   startAnimationLoop(beforeRender: () => void) {
        const animate = () => {
            this.requestID = requestAnimationFrame(animate);
            beforeRender();
            this.renderer!.render(this.scene!, this.camera!);
        }
        animate();
    }

   stopAnimationLoop() {
    window.cancelAnimationFrame(this.requestID);
   }
      
}