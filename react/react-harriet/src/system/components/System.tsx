import React from "react";
import * as THREE from "three";
import { noise4d } from '../../_3d/Noise';
import { State, SystemObject, SystemObjectType, StarObject, WorldObject } from '../model';
import { ClientDispatch } from '../../clients/model';
import Model3D from '../../_3d/Model3D';
import { worldMesh } from "../../world/components/World";

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

  constructor(props : Props) {
    super (props);

    this.renderCount = 0;
    this.unTimeMaterial = [];
    this.model = null;
    this.beforeRender = this.beforeRender.bind(this);
  }

  componentDidMount() {
    this.model = new Model3D(this.mount);
    this.addCustomSceneObjects();
    this.model.startAnimationLoop(this.beforeRender);
  }

  componentWillUnmount() {
    this.model!.stopAnimationLoop();
  }

  addCustomSceneObjects = () => {
    this.model!.camera.position.z = 10;
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

    this.unTimeMaterial.push(material);
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
    if (obj.type === SystemObjectType.World) {
      scale /= 10;
    } else {
      scale /= 2
    }

    mesh.scale.set(scale, scale, scale);
    mesh.name = obj.name;
    this.model!.scene.add( mesh );
  }

  updateScene = (obj : SystemObject) => {
    if (!this.model!.scene.getObjectByName(obj.name)) {
      switch (obj.type) {
        case SystemObjectType.Star:
          this.addObject(obj, this.starMesh(obj as StarObject))
          break;

        case SystemObjectType.World:
          this.addObject(obj, worldMesh(this.model!, obj as WorldObject))
          break;
      }
    }

    const mesh = this.model!.scene.getObjectByName(obj.name)!;
    mesh.position.set(5.0 * obj.orbit * Math.cos(obj.longitude), 0.0, 5.0 * obj.orbit * Math.sin(obj.longitude));
    console.log('updateScene', mesh.name, mesh.position);

    for (const dep of obj.dependents) {
      this.updateScene(dep)
    }
  }

  render() {
    console.log('star-system', 'render', this.props.clientState)

    if (this.props.clientState.primary) {
      this.updateScene(this.props.clientState.primary);
    }

    return (
      <div ref={ref => (this.mount = ref)} />
    )
  }
}

export default Component;
