import React from "react";
import * as THREE from "three";
import { noise3d, fractalNoise3d, ridgedNoise3d } from '../../_3d/Noise';
import { State, Composition, Climate } from '../model';
import { ClientDispatch } from '../../clients/model';
import { WorldObject } from "../../system/model";
import Model3D from '../../_3d/Model3D';
import { GLTF } from "three/examples/jsm/loaders/GLTFLoader";

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
  uniform sampler2D textureSampler;
  uniform vec3 starLight;
  ` +
  noise3d +
  fractalNoise3d +
  `
  void main() {
    vec3 light = normalize(starLight);
    float n = noise(vNormal, 16, 1.5, 0.4) / 2.0 + 0.5;
    float dProd = max(0.0, dot(vNormal, light));
    vec4 texColor = texture2D(textureSampler, vec2(n, 0.0));
    gl_FragColor = texColor * vec4(dProd, dProd, dProd, 1.0);
  }
`

const worldTextureName = (world : WorldObject) : string => {
  const { climate, composition, mass } = world;
  let textureName : string = '';
  
  if (climate == Climate.Jovian) {
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
    textureName = 'temperate'
  }

  return textureName;
}

export function worldMesh(
  model : Model3D,
  world : WorldObject,
  detail : number,
  lightFrom : THREE.Vector3,
) : THREE.Mesh
{

  if (world.surface.length === 0) {
      const vertexShader = standardVertexShader;

      const fragmentShader = world.climate == Climate.Jovian ? gasGiantFragmentShader : rockWorldFragmentShader;
      const textureName = worldTextureName(world);
      const texture =  model.textureLoader.load(textureName + '.png');

      const geometry = new THREE.IcosahedronBufferGeometry(1, detail);
      const material = new THREE.ShaderMaterial({
        vertexShader,
        fragmentShader,
        uniforms: {
          textureSampler: { type: 't', value: texture },
          unTime: { type: 'f', value: 0},
          starLight: { value: lightFrom }
        },
      });

      return new THREE.Mesh(geometry, material);
    } else {
      //let mats : THREE.Material[] = [];
      const mesh = new THREE.Mesh();
      for(const sector of world.surface) {
        let geometry = new THREE.Geometry();
        for (const vertex of sector.border) {
          const v = new THREE.Vector3(vertex.x, vertex.z, -vertex.y);
          geometry.vertices.push(v);
        }
        for (let i = 1; i < sector.border.length - 1; ++i) {
          const face = new THREE.Face3(0, i + 1, i,  new THREE.Vector3(sector.normal.x, sector.normal.z, -sector.normal.y));
          for (const vertex of sector.border) {
            const v = new THREE.Vector3(vertex.x, vertex.z, -vertex.y);
            face.vertexNormals.push(v);
          }
          geometry.faces.push(face);
        }

        const material = new THREE.MeshBasicMaterial({
            color: new THREE.Color(sector.color),
          });
        mesh.add(new THREE.Mesh(geometry, material));

        if (false && sector.model) {
            model.getModel(sector.model, (gltf : GLTF) => {
              gltf.scene.scale.set(0.0001, 0.0001, 0.0001);
              const position = sector.normal;
              console.log(sector.model, position);
              gltf.scene.position.set(position.x * 1.1, position.z * 1.1, -position.y * 1.1);
              gltf.scene.lookAt(position.x, position.z, -position.y);
              // gltf.scene.traverse( function ( child ) {

							// 	if ( child instanceof THREE.Mesh ) {

							// 		child.scale(0.001, 0.001, 0.001);

							// 	}

							// } );

              mesh.add(gltf.scene)
            });
        } 
      }
      return mesh;
    }

}

class World extends React.Component<Props,WorldSceneState> {

  mount: any;

  model  : Model3D | null;
  planetLo : THREE.Mesh | null;
  planetHi : THREE.Mesh | null;
  renderCount : number

  constructor(props : Props) {
    super (props);

    this.state = {
      world: null,
    }

    this.renderCount = 0;
    this.planetLo = null;
    this.planetHi = null;
    this.model = null;
    this.beforeRender = this.beforeRender.bind(this);
  }

  componentDidMount() {
    this.model = new Model3D(this.mount, 0.1, 10, 0.1, 10);
    this.addCustomSceneObjects();
    this.model.startAnimationLoop(this.beforeRender);
  }

  componentWillUnmount() {
    this.model!.stopAnimationLoop();
  }

  addCustomSceneObjects = () => {
    this.model!.light.position.set(3, 0, 3);
    this.model!.scene.add(this.model!.light);
    // const light2 = new THREE.DirectionalLight();
    // light2.position.set(3, 0, -3);
    // this.model!.scene.add(light2);
  }

  beforeRender() {
    if (this.planetLo && !this.planetHi) {
      if (this.props.clientState.world && this.props.clientState.world.surface.length === 0) {
        (this.planetLo!.material as THREE.ShaderMaterial).uniforms.unTime.value = this.renderCount;
      } 
    } else if (this.planetHi) {
        this.planetHi.rotateY(0.002);
    }
    
    this.renderCount += 0.0002;
  }

  updateScene = (world : WorldObject) => {
    if (!this.planetLo) {
      this.planetLo = worldMesh(this.model!, world, 6, new THREE.Vector3(-5, 0, 3));
      this.model!.scene.add(this.planetLo);
      this.props.clientDispatch.requestUpdate(2);
    } else if (world.surface.length > 0 && !this.planetHi) {
      this.planetHi = worldMesh(this.model!, world, 6, new THREE.Vector3(-5, 0, 3));
      this.model!.scene.remove(this.planetLo);
      this.model!.scene.add(this.planetHi);
    }
  }

  render() {
    if (this.props.clientState.world) {
      console.log('render', this.props.clientState.world)
      this.updateScene(this.props.clientState.world);
    }
    return (
      <div className="harriet-model3d" ref={ref => (this.mount = ref)} />
    )
  }
}

export default World;
