import * as THREE from "three";

export default class Model3D {

    readonly scene: THREE.Scene;
    readonly camera: THREE.Camera;
    readonly renderer: THREE.Renderer;
    readonly textureLoader: THREE.TextureLoader;
    requestID : number = 0
  
    constructor(mount : any) {
        this.scene = new THREE.Scene();
        const itemElement = mount.closest(".concorde-dashboard-item");
        console.log("model3d", itemElement.clientWidth, itemElement.clientHeight);
  
        const width = itemElement.clientWidth; 
        const height = itemElement.clientHeight - 30;
  
        this.camera = new THREE.PerspectiveCamera( 75, width / height, 0.1, 10 );
  
        this.renderer = new THREE.WebGLRenderer({ 
            antialias: true,
        });
        this.renderer.setSize(width, height);
        mount.appendChild( this.renderer.domElement );
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