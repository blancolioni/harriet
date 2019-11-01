import React from "react";
import * as THREE from "three";
import { noise3d, fractalNoise3d, ridgedNoise3d } from '../../_3d/Noise';
import { State, Composition, Climate } from '../model';
import { ClientDispatch } from '../../clients/model';
import { WorldObject } from "../../system/model";
import Model3D from '../../_3d/Model3D';

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

+ fractalNoise3d

+ ridgedNoise3d

+ `
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
  ` +
  noise3d +
  fractalNoise3d +
  `
  void main() {
    vec3 light = vec3(0.1, 0.0, 5.0);
    light = normalize(light);
    float n = noise(vNormal, 8, 1.0, 0.5) / 2.0 + 0.5;
    float dProd = max(0.0, dot(vNormal, light));
  
    gl_FragColor = vec4(n, n, n, 1.0) * vec4(dProd, dProd, dProd, 1.0);
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
  model : Model3D,
  world : WorldObject | null,
) : THREE.Mesh
{
  const vertexShader = standardVertexShader;

  const fragmentShader = world && world.climate === Climate.Jovian ? gasGiantFragmentShader : rockWorldFragmentShader;
  const textureName = world ? worldTextureName(world) : 'logo512';
  const texture =  model.textureLoader.load(textureName + '.png');

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

  model  : Model3D | null;
  planet : THREE.Mesh | null;
  renderCount : number

  constructor(props : Props) {
    super (props);

    this.state = {
      world: null,
    }

    this.renderCount = 0;
    this.planet = null;
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
    this.planet = worldMesh(this.model!, this.state.world);
    this.model!.scene.add(this.planet);
  }

  beforeRender() {
    (this.planet!.material as THREE.ShaderMaterial).uniforms.unTime.value = this.renderCount;
    this.renderCount += 0.0002;
  }

  render() {
    return (
      <div ref={ref => (this.mount = ref)} />
    )
  }
}

export default World;
