import React from "react";
import * as THREE from "three";
import { noise3d } from '../../_3d/Noise';
import { State } from '../model';
import { ClientDispatch } from '../../clients/model';

interface Dispatch extends ClientDispatch {
}

interface Props {
    clientState : State,
    clientDispatch : Dispatch
}

class World extends React.Component {

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
      uniform sampler2D textureSampler;
      uniform float unTime;
      `
      
      + noise3d

      + `
      
      float noise(vec3 position, int octaves, float frequency, float persistence) {
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

    float ridgedNoise(vec3 position, int octaves, float frequency, float persistence) {
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
        vec3 position = vNormal + vec3(unTime, 0.0, unTime);
        float n1 = noise(position, 6, 10.0, 0.8) * 0.01;
        float n2 = ridgedNoise(position, 5, 5.8, 0.75) * 0.015 - 0.01;
        // Get the three threshold samples

        float s = 0.6;
        float t1 = snoise(position * 2.0) - s;
        float t2 = snoise((position + 800.0) * 2.0) - s;
        float t3 = snoise((position + 1600.0) * 2.0) - s;
        
        // Intersect them and get rid of negatives
        
        float threshold = max(t1 * t2 * t3, 0.0);
        
        // Storms
        
        float n3 = snoise(position * 0.1) * threshold;
        
        float n = n1 + n2 + n3;
        float newTexCoord = vUv.y + n;
        vec4 texColor = texture2D(textureSampler, vec2(newTexCoord, 0.0));

        vec3 light = vec3(0.1, 0.0, 5.0);
        light = normalize(light);

        float dProd = max(0.0, dot(vNormal, light));

        gl_FragColor = texColor * vec4(dProd, dProd, dProd, 1.0);

    }
    `
  }

  componentDidMount() {

    const scene = new THREE.Scene();
    let itemElement = this.mount.closest(".concorde-dashboard-item");
    console.log("model3d", itemElement.clientWidth, itemElement.clientHeight);

    let width = itemElement.clientWidth; 
    let height = itemElement.clientHeight - 30;

    const camera = new THREE.PerspectiveCamera( 75, width / height, 0.1, 10 );
    const renderer = new THREE.WebGLRenderer({ 
      antialias: true,
    });
    renderer.setSize(width, height);
    this.mount.appendChild( renderer.domElement );

    var textureLoader = new THREE.TextureLoader();
    var cloudTexture = textureLoader.load('gas_giant_jovian.png');
    var renderCount = 0;

    var light = new THREE.DirectionalLight(0xffffff);
    light.position.set(0, 0, 10);
    scene.add(light);

    var geometry = new THREE.IcosahedronBufferGeometry(1, 4);
    var material = new THREE.ShaderMaterial({
      vertexShader: this.vertexShader(),
      fragmentShader: this.fragmentShader(),
      uniforms: {
        textureSampler: { type: 't', value: cloudTexture },
        unTime: { type: 'f', value: renderCount },
      },
    });

    var rotation = 0;
    var planet = new THREE.Mesh(geometry, material);

    scene.add( planet );
    camera.position.z = 2;
    var animate = function () {
      requestAnimationFrame( animate );
      //planet.rotateY(rotation);
      material.uniforms.unTime.value = renderCount;
      //rotation += (0.1 * Math.pi / 180);
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

export default World;
