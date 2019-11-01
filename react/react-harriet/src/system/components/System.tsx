import React from "react";
import * as THREE from "three";
import { noise4d } from '../../_3d/Noise';
import { State } from '../model';
import { ClientDispatch } from '../../clients/model';
import Model3D from '../../_3d/Model3D';

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
  star : THREE.Mesh | null;
  renderCount : number

  constructor(props : Props) {
    super (props);

    this.renderCount = 0;
    this.star = null;
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
    let geometry = new THREE.IcosahedronBufferGeometry(1, 4);
    let material = new THREE.ShaderMaterial({
      vertexShader: this.vertexShader(),
      fragmentShader: this.fragmentShader(),
      uniforms: {
//        textureSampler: { type: 't', value: cloudTexture },
        unTime: { type: 'f', value: 0 },
      },
    });

    this.star = new THREE.Mesh(geometry, material);

    this.model!.scene.add( this.star );
  }

  beforeRender() {
    (this.star!.material as THREE.ShaderMaterial).uniforms.unTime.value = this.renderCount;
    this.renderCount += 0.0002;
  }

  render() {
    console.log('star-system', 'render', this.props.clientState)
    return (
      <div ref={ref => (this.mount = ref)} />
    )
  }
}

export default Component;
