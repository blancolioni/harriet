import React from "react";
import * as THREE from "three";
import { noise4d } from '../../_3d/Noise';
import { State } from '../model';
import { ClientDispatch } from '../../clients/model';

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

  componentDidMount() {

    const scene = new THREE.Scene();
    let itemElement = this.mount.closest(".concorde-dashboard-item");
    console.log("model3d", itemElement.clientWidth, itemElement.clientHeight);

    let width = itemElement.clientWidth; 
    let height = itemElement.clientHeight - 30;

    const camera = new THREE.PerspectiveCamera( 75, width / height, 0.1, 20 );
    const renderer = new THREE.WebGLRenderer({ 
      antialias: true,
    });
    renderer.setSize(width, height);
    this.mount.appendChild( renderer.domElement );

    console.log('star-system-mount', this.props.clientState);

    // var textureLoader = new THREE.TextureLoader();
    // var cloudTexture = textureLoader.load('gas_giant_jovian.png');
    
    var renderCount = 0;

    // var light = new THREE.DirectionalLight(0xffffff);
    // light.position.set(0, 0, 10);
    // scene.add(light);

    var geometry = new THREE.IcosahedronBufferGeometry(1, 4);
    var material = new THREE.ShaderMaterial({
      vertexShader: this.vertexShader(),
      fragmentShader: this.fragmentShader(),
      uniforms: {
//        textureSampler: { type: 't', value: cloudTexture },
        unTime: { type: 'f', value: renderCount },
      },
    });

    var star = new THREE.Mesh(geometry, material);

    scene.add( star );
    camera.position.z = 2;
    var animate = function () {
      requestAnimationFrame( animate );
      material.uniforms.unTime.value = renderCount;
      renderer.render( scene, camera );
      renderCount += 0.0002;
    };
    animate();
  }

  render() {
    return (
      <div ref={ref => (this.mount = ref)} />
    )
  }
}

export default Component;
