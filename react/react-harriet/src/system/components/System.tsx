import React from "react";
import * as THREE from "three";
import { noise4d } from '../../_3d/Noise';
import { State, SystemObject, SystemObjectType, StarObject, WorldObject, ShipObject } from '../model';
import { ClientDispatch } from '../../clients/model';
import Model3D from '../../_3d/Model3D';
import { worldMesh } from "../../world/components/World";
import { shipMesh } from "../../ship/components/Ship";
import { CSS2DObject } from "three/examples/jsm/renderers/CSS2DRenderer";
import { Vector3 } from "three";

interface Dispatch extends ClientDispatch {
}

interface Props {
    clientState : State,
    clientDispatch : Dispatch
}

class Component extends React.Component<Props,State> {

  mount: any;
  vertexShader() {
    return `
      varying vec2 vUv;
      varying vec3 vNormal;

      void main() {
        vUv = uv;
        vNormal = normal;
        gl_Position = projectionMatrix *
                      modelViewMatrix *
                      vec4(position,1.0);
      }  
    `;
  }
 
  fragmentShader() {
    return `
      varying vec2 vUv;
      varying vec3 vNormal;
      uniform float unTime;
      `
      
      + noise4d

      + `
      
      float noise(vec4 position, int octaves, float frequency, float persistence) {
        float total = 0.0; // Total value so far
        float maxAmplitude = 0.0; // Accumulates highest theoretical amplitude
        float amplitude = 1.0;
        for (int i = 0; i < 20; i++) {

          if (i >= octaves) return total / maxAmplitude;

            // Get the noise sample
            total += snoise(position * frequency) * amplitude;
    
            // Make the wavelength twice as small
            frequency *= 2.0;
    
            // Add to our maximum possible amplitude
            maxAmplitude += amplitude;
    
            // Reduce amplitude according to persistence for the next octave
            amplitude *= persistence;

        }
    
        // Scale the result by the maximum amplitude
        return total / maxAmplitude;
    }

    float ridgedNoise(vec4 position, int octaves, float frequency, float persistence) {
      float total = 0.0; // Total value so far
      float maxAmplitude = 0.0; // Accumulates highest theoretical amplitude
      float amplitude = 1.0;
      for (int i = 0; i < 20; i++) {

        if (i >= octaves) return total / maxAmplitude;

          // Get the noise sample
          total += ((1.0 - abs(snoise(position * frequency))) * 2.0 - 1.0) * amplitude;
  
          // Make the wavelength twice as small
          frequency *= 2.0;
  
          // Add to our maximum possible amplitude
          maxAmplitude += amplitude;
  
          // Reduce amplitude according to persistence for the next octave
          amplitude *= persistence;

      }
  
      // Scale the result by the maximum amplitude
      return total / maxAmplitude;
  }

    float computeDiffuse(vec3 normal) {
        return clamp( dot( normal, vec3(0.1, 0.0, 5.0) ), 0.0, 1.0 );
      }

      void main() {
        vec4 position = vec4(vNormal, unTime);
        float n = noise(position, 4, 40.0, 0.7) / 2.0 + 0.5;
        float total = n;

        gl_FragColor = vec4(total, total, total, 1.0); // vec4(total, total, total, 1.0);

    }
    `
  }

  model  : Model3D | null;
  renderCount : number
  unTimeMaterial : THREE.ShaderMaterial[];
  currentZoom : THREE.Mesh | null = null;
  currentLabel : CSS2DObject | null = null;
  zoomObject : SystemObject | null = null;
  localLight : THREE.Light | null = null;

  constructor(props : Props) {
    super (props);

    this.renderCount = 0;
    this.unTimeMaterial = [];
    this.model = null;
    this.beforeRender = this.beforeRender.bind(this);
  }

  componentDidMount() {
    this.model = new Model3D(this.mount, 1, 1.5e15, 1e5, 1e10);
    this.addCustomSceneObjects();
    this.model.startAnimationLoop(this.beforeRender);
  }

  componentWillUnmount() {
    this.model!.stopAnimationLoop();
  }

  addCustomSceneObjects = () => {
    this.model!.camera.position.set(0.0, 1e9, 15 * 1.5e11);
    this.model!.camera.lookAt(0,0,0);
    this.localLight = new THREE.DirectionalLight("#fff", 1);
    this.localLight.position.set(0, 0, 1);
    this.model!.scene.add(this.localLight);
  }

  starMesh = (star : StarObject) : THREE.Mesh => {
    let geometry = new THREE.IcosahedronBufferGeometry(1, 4);
    let material = new THREE.ShaderMaterial({
      vertexShader: this.vertexShader(),
      fragmentShader: this.fragmentShader(),
      uniforms: {
//        textureSampler: { type: 't', value: cloudTexture },
        unTime: { type: 'f', value: 0 },
      },
    });

    const mesh = new THREE.Mesh(geometry, material);
    mesh.name = star.id;
    return mesh;
  }

  showDistance = (d : number) : string => {
    if (d < 1000) {
      return Math.floor(d) + 'm';
    } else if (d < 1e9) {
      return Math.floor(d / 1000) + 'km';
    } else {
      return Math.floor(d / 1.5e8) / 1000 + 'AU';
    }
  }

  beforeRender() {
    for (const mat of this.unTimeMaterial) {
      mat.uniforms.unTime.value = this.renderCount;
    }

    const updateLabel = (object : THREE.Object3D) : void => {
      const obj = this.model!.objects[object.name];
      if (obj) {
        const label = object.getObjectByName(object.name + '-label') as CSS2DObject;
        if (label) {
          const v1 = this.model!.camera.position;
          const v2 = object.position;
          const d = v1.distanceTo(v2);
          const img = this.showDistance(d);
          label!.element.textContent = obj.name + img;
        }
        if (obj.type == SystemObjectType.Ship) {
          object.rotateY(0.001);
          object.rotateZ(0.01);
        }
      }
    }

    this.model!.scene.traverse(updateLabel);

    this.renderCount += 0.0002;
  }

  addObject = (obj : SystemObject, mesh : THREE.Mesh, scale : number = 1) => {
    if (scale != 1) mesh.scale.set(scale, scale, scale);
    mesh.name = obj.id;
    this.model!.scene.add( mesh );

    var labelDiv = document.createElement( 'div' );
    labelDiv.className = 'concorde-star-label';
    labelDiv.textContent = obj.name;
    labelDiv.style.marginTop = '-1em';
    var labelObject = new CSS2DObject( labelDiv );
    labelObject.position.set( 0.5, -0.1, 0 );
    labelObject.name = mesh.name + '-label';
    mesh.add( labelObject );
    const mat = mesh.material;

    if ('uniforms' in mat) {
      this.unTimeMaterial.push(mat);
    }

    this.model!.addObject(obj, mesh, labelObject);
  }

  updateScene = (obj : SystemObject, origin : THREE.Vector3) => {
    const x = origin.x + obj.orbit * Math.cos(obj.longitude);
    const y = 0;
    const z = origin.z + obj.orbit * Math.sin(obj.longitude);

    if (!this.model!.objects[obj.id]) {
      switch (obj.type) {
        case SystemObjectType.Star:
          this.addObject(obj, this.starMesh(obj as StarObject), obj.radius)
          break;

        case SystemObjectType.World:
          this.addObject(obj, worldMesh(this.model!, obj as WorldObject, 3, new THREE.Vector3(-x, -y, -z)), obj.radius);
          break;

        case SystemObjectType.Ship:
          this.addObject(obj, shipMesh(obj as ShipObject));
          break;
      }  
  
    }

    const mesh = this.model!.scene.getObjectByName(obj.id)!;
    mesh.position.set(x, y, z);

    for (const dep of obj.dependents) {
      this.updateScene(dep, mesh.position)
    }
  }

  render() {

    if (this.props.clientState.primary) {
      this.updateScene(this.props.clientState.primary, new THREE.Vector3(0,0,0));
    }

    if (this.props.clientState.zoom && (!this.currentZoom || this.currentZoom.name !== this.props.clientState.zoom)) {
      const newZoom = this.model!.scene.getObjectByName(this.props.clientState.zoom);
      if (newZoom) {
        this.currentZoom = newZoom as THREE.Mesh;
        this.currentLabel = (newZoom.getObjectByName(newZoom.name + '-label') as CSS2DObject);
        this.zoomObject = this.model!.objects[this.props.clientState.zoom];
        const mesh = this.currentZoom;
        const { x, y, z } = mesh.position;
        const d = this.model!.objects[this.props.clientState.zoom].radius;
        const n1 = new THREE.Vector3(x, y, z).normalize().multiplyScalar(d * 50);
        const n2 = new THREE.Vector3(x, y, z).normalize().multiplyScalar(d * 5);
        const wp1 = new THREE.Vector3 (x, y, z).sub(n1);
        const wp2 = new THREE.Vector3 (x, y, z).sub(n2);
        const lt = mesh.position.clone().normalize();
        this.localLight!.position.set(-lt.x, -lt.y, -lt.z);

        console.log('add waypoint', new THREE.Vector3(x, y, z), wp1, wp2)
        this.model!.addWaypoint(null, new THREE.Vector3(x, y, z), 1.0);
        this.model!.addWaypoint(wp1, new THREE.Vector3 (x, y, z), 3.0);
        this.model!.addWaypoint(wp2, new THREE.Vector3 (x, y, z), 1.0);
      }
    }

    return (
      <div className="harriet-model3d" ref={ref => (this.mount = ref)} />
    )
  }
}

export default Component;
