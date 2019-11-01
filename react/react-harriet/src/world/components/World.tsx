import React from "react";
import * as THREE from "three";
import { noise3d } from '../../_3d/Noise';
import { State, Composition, Climate } from '../model';
import { ClientDispatch } from '../../clients/model';
import { WorldObject } from "../../system/model";

interface Dispatch extends ClientDispatch {
}

interface Props {
    clientState : State,
    clientDispatch : Dispatch
  }

interface WorldSceneState {
  world : WorldObject | null
}

const standardVertexShader : string = `
  varying vec2 vUv;
  varying vec3 vNormal;

  void main() {
    vUv = uv;
    vNormal = normal;
    gl_Position = projectionMatrix *
                  modelViewMatrix *
                  vec4(position,1.0);
  }  
`

const gasGiantFragmentShader = `
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

const rockWorldFragmentShader = `
  varying vec2 vUv;
  varying vec3 vNormal;
  void main() {
    vec3 light = vec3(0.1, 0.0, 5.0);
    light = normalize(light);
  
    float dProd = max(0.0, dot(vNormal, light));
  
    gl_FragColor = (0.5, 0.1, 0.5, 1.0) * vec4(dProd, dProd, dProd, 1.0);
  }
`

const worldTextureName = (world : WorldObject) : string => {
  const { climate, composition, mass } = world;
  let textureName : string = '';
  
  if (composition === Composition.Hydrogen) {
    if (mass >= 100) {
      textureName = 'gas_giant_jovian'
    } else if (mass >= 50) {
      textureName = 'gas_giant_saturnian'
    } else if (mass >= 15) {
      textureName = 'gas_giant_blue'
    } else {
      textureName = 'gas_giant_cyan'
    }
  } else {
    textureName = 'logo512'
  }

  return textureName;
}

export function worldMesh(
  textureLoader : THREE.TextureLoader,
  world         : WorldObject | null,
) : THREE.Mesh
{
  const vertexShader = standardVertexShader;

  const fragmentShader = world && world.climate === Climate.Jovian ? gasGiantFragmentShader : rockWorldFragmentShader;
  const textureName = world ? worldTextureName(world) : 'logo512';
  const texture =  textureLoader.load(textureName + '.png');

  const geometry = new THREE.IcosahedronBufferGeometry(1, 4);
  const material = new THREE.ShaderMaterial({
    vertexShader,
    fragmentShader,
    uniforms: {
      textureSampler: { type: 't', value: texture },
      unTime: { type: 'f', value: 0},
    },
  });

  return new THREE.Mesh(geometry, material);
}

class World extends React.Component<Props,WorldSceneState> {

  mount: any;
  scene: THREE.Scene | null;
  camera: THREE.Camera | null;
  renderer: THREE.Renderer | null;
  textureLoader: THREE.TextureLoader | null;

  planet : THREE.Mesh | null;
  requestID : number

  constructor(props : Props) {
    super (props);

    this.state = {
      world: null,
    }

    this.scene = null;
    this.camera = null;
    this.renderer = null;
    this.textureLoader = null;
    this.planet = null;
    this.requestID = 0;
  }

  componentDidMount() {
    this.sceneSetup();
    this.addCustomSceneObjects();
    this.startAnimationLoop();
  }

  componentWillUnmount() {
    window.cancelAnimationFrame(this.requestID);
  }

  sceneSetup = () => {
    const itemElement = this.mount.closest(".concorde-dashboard-item");
    console.log("model3d", itemElement.clientWidth, itemElement.clientHeight);

    const width = itemElement.clientWidth; 
    const height = itemElement.clientHeight - 30;

    this.scene = new THREE.Scene();
    this.camera = new THREE.PerspectiveCamera( 75, width / height, 0.1, 10 );
    this.camera.position.z = 3;

    this.renderer = new THREE.WebGLRenderer({ 
      antialias: true,
    });
    this.renderer.setSize(width, height);
    this.mount.appendChild( this.renderer.domElement );

    this.textureLoader = new THREE.TextureLoader();

  }

  addCustomSceneObjects = () => {
    this.planet = worldMesh(this.textureLoader!, this.state.world);
    this.scene!.add(this.planet);
  }

  startAnimationLoop = () => {
    let renderCount = 0;
    
    (this.planet!.material as THREE.ShaderMaterial).uniforms.unTime.value = renderCount;
    this.renderer!.render( this.scene!, this.camera! );
    renderCount += 0.0002;
    this.requestID = requestAnimationFrame( this.startAnimationLoop );
}

  //   let textureLoader = new THREE.TextureLoader;
  //   let scene = new THREE.Scene();
  //   let itemElement = this.mount.closest(".concorde-dashboard-item");
  //   console.log("model3d", itemElement.clientWidth, itemElement.clientHeight);

  //   let width = itemElement.clientWidth; 
  //   let height = itemElement.clientHeight - 30;

  //   const camera = new THREE.PerspectiveCamera( 75, width / height, 0.1, 10 );
  //   const renderer = new THREE.WebGLRenderer({ 
  //     antialias: true,
  //   });
  //   renderer.setSize(width, height);
  //   this.mount.appendChild( renderer.domElement );

  //   var light = new THREE.DirectionalLight(0xffffff);
  //   light.position.set(0, 0, 10);
  //   scene.add(light);

  //   camera.position.z = 2;
    
  //   let planet = worldMesh(textureLoader, this.state.world);
  //   scene.add(planet);

  //   let renderCount = 0;
   
  //   var animate = function () {
  //     requestAnimationFrame( animate );
  //     (planet.material as THREE.ShaderMaterial).uniforms.unTime.value = renderCount;
  //     renderer.render( scene, camera );
  //     renderCount += 0.0002;
  //   };
  //   animate();
  // }

  render() {
    console.log('World', 'render', this.props);
    return (
      <div ref={ref => (this.mount = ref)} />
    )
  }
}

export default World;
