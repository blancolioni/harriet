import React from 'react'
import * as THREE from "three";

import { Model3D } from '../Model3D';

class World extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
      cameraXYZ: { x: 0, y: 2, z: 0 },
      scene: null,
      camera: null,
      raycaster: null,
      intersectedObject: null,
      rotation: 0,
    }

    this.initScene = this.initScene.bind(this);
    this.loadScene = this.loadScene.bind(this);
    this.mouseMove = this.mouseMove.bind(this);
    this.beforeRender = this.beforeRender.bind(this);
}

initScene(scene, width, height) {
  let camera = new THREE.PerspectiveCamera( 75, width / height, 0.1, 1000 );
  camera.position.x = this.state.cameraXYZ.x;
  camera.position.y = this.state.cameraXYZ.y;
  camera.position.z = this.state.cameraXYZ.z;

  let raycaster = new THREE.Raycaster();

  var light1 = new THREE.DirectionalLight( 0xffffff, 0.5 );
  light1.position.set( 2, -2, 0 );
  scene.add( light1 );

  var light2 = new THREE.DirectionalLight( 0xffffff, 1.5 );
  light2.position.set( -2, -2, 0 );
  scene.add( light2 );

this.setState({
    scene: scene,
    camera: camera,
    raycaster: raycaster,
    intersectedObject: null,
  });

  return camera;
}

loadScene(data) {

    let positions = [];
    let normals = [];
    let colors = [];
    let geometry = new THREE.BufferGeometry();

    for (const sector of data.data) {
      const vs = sector.vertices;
      const color = { r: sector.r, g: sector.g, b: sector.b };

      for(let i=1; i < vs.length - 1; ++i) {
        const v1 = vs[0];
        const v2 = vs[i];
        const v3 = vs[i+1];
        positions.push(v1.x, v1.y, v1.z);
        positions.push(v2.x, v2.y, v2.z);
        positions.push(v3.x, v3.y, v3.z);
        normals.push(v1.x, v1.y, v1.z);
        normals.push(v2.x, v2.y, v2.z);
        normals.push(v3.x, v3.y, v3.z);
        colors.push(color.r, color.g, color.b);
        colors.push(color.r, color.g, color.b);
        colors.push(color.r, color.g, color.b);
      }
    }

    function disposeArray() {
      this.array = null;
    }
    
    geometry.addAttribute( 'position', new THREE.Float32BufferAttribute( positions, 3 ).onUpload( disposeArray ) );
    geometry.addAttribute( 'normal', new THREE.Float32BufferAttribute( normals, 3 ).onUpload( disposeArray ) );
    geometry.addAttribute( 'color', new THREE.Float32BufferAttribute( colors, 3 ).onUpload( disposeArray ) );  

    geometry.computeBoundingSphere();

    var material = new THREE.MeshPhongMaterial( {
      color: 0xaaaaaa, specular: 0xffffff, shininess: 250,
      side: THREE.DoubleSide, vertexColors: THREE.VertexColors
    } );
    let mesh = new THREE.Mesh( geometry, material );
    this.setState({worldMesh: mesh});
    this.state.scene.add( mesh );
  }

  mouseMove(mouse) {

  }

  beforeRender() {
    if (this.state.worldMesh) {
      let rot = this.state.rotation;
      this.state.worldMesh.rotateOnAxis(new THREE.Vector3(0, 0, 1), 0.001);
      this.setState(state => {
        return { ...state, rotation: rot + 0.01 }
      });
    }
  }

  render () {
      return (
        <Model3D
          title={this.props.title} 
          model={this.props.model} 
          modelArg={this.props.modelArgs} 
          onDashboardCommand={this.props.onDashboardCommand}
          initScene={this.initScene}
          loadScene={this.loadScene}
          mouseMove={this.mouseMove}
          beforeRender={this.beforeRender}
        >
      </Model3D>
      );
  }

}

export { World };