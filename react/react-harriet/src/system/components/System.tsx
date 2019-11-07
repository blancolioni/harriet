import React from "react";
import * as THREE from "three";
import { noise4d } from '../../_3d/Noise';
import { State, SystemObject, SystemObjectType, StarObject, WorldObject } from '../model';
import { ClientDispatch } from '../../clients/model';
import Model3D from '../../_3d/Model3D';
import { worldMesh } from "../../world/components/World";
import { CSS2DObject } from "three/examples/jsm/renderers/CSS2DRenderer";

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
  currentZoom : THREE.Mesh | null;

  constructor(props : Props) {
    super (props);

    this.renderCount = 0;
    this.unTimeMaterial = [];
    this.model = null;
    this.currentZoom = null;
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
    this.model!.camera.position.z = 15 * 1.5e11;
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
    mesh.name = star.name;
    return mesh;
  }

  beforeRender() {
    for (const mat of this.unTimeMaterial) {
      mat.uniforms.unTime.value = this.renderCount;
    }
    
    this.renderCount += 0.0002;
  }

  addObject = (obj : SystemObject, mesh : THREE.Mesh) => {
    let scale = obj.radius;    
    // if (obj.type === SystemObjectType.World) {
    //   scale /= 10;
    // } else {
    //   scale /= 2
    // }

    mesh.scale.set(scale, scale, scale);
    mesh.name = obj.name;
    this.model!.scene.add( mesh );

    var labelDiv = document.createElement( 'div' );
    labelDiv.className = 'concorde-star-label';
    labelDiv.textContent = obj.name;
    labelDiv.style.marginTop = '-1em';
    var labelObject = new CSS2DObject( labelDiv );
    labelObject.position.set( 0.5, -0.1, 0 );
    mesh.add( labelObject );
    const mat = mesh.material;

    if ('uniforms' in mat) {
      this.unTimeMaterial.push(mat);
    }


  }

  updateScene = (obj : SystemObject, origin : THREE.Vector3) => {
    const x = origin.x + obj.orbit * Math.cos(obj.longitude);
    const y = 0;
    const z = origin.z + obj.orbit * Math.sin(obj.longitude);
    let newObject = false;

    if (!this.model!.scene.getObjectByName(obj.name)) {
      newObject = true;
      switch (obj.type) {
        case SystemObjectType.Star:
          this.addObject(obj, this.starMesh(obj as StarObject))
          break;

        case SystemObjectType.World:
          this.addObject(obj, worldMesh(this.model!, obj as WorldObject, 3, new THREE.Vector3(-x, -y, -z)));
          break;

        case SystemObjectType.Ship:
          this.addObject(obj, new THREE.Mesh(new THREE.IcosahedronGeometry(1, 3), new THREE.MeshBasicMaterial()));
          break;
      }  
  
    }

    const mesh = this.model!.scene.getObjectByName(obj.name)!;
    mesh.position.set(x, y, z);

    if (false) {
      if (newObject) {
        const wp = new THREE.Vector3 (x, y, z + mesh.scale.z * 2.5);
        console.log('add waypoint', x, y, z, mesh.scale.z, wp)
        this.model!.addWaypoint(wp, new THREE.Vector3 (0, 0, 1), 5.0);
        this.model!.addWaypoint(new THREE.Vector3 (x, y, z + mesh.scale.z * 2), new THREE.Vector3 (0, 0, 1), 5.0);
      }
    }
    
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
        const mesh = this.currentZoom;
        const { x, y, z } = mesh.position;
        const d = mesh.scale.z;
        const n = new THREE.Vector3(x, y, z).normalize().multiplyScalar(d * 3);
        const wp = new THREE.Vector3 (x, y, z).sub(n);
        
        console.log('add waypoint', x, y, z, mesh.scale.z, wp)
        this.model!.addWaypoint(wp, new THREE.Vector3 (x, y, z), 5.0);
      }
    }

    return (
      <div className="harriet-model3d" ref={ref => (this.mount = ref)} />
    )
  }
}

export default Component;
