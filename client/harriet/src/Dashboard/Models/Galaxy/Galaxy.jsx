import React from 'react'
import * as THREE from "three";
import { CSS2DObject } from 'three/examples/jsm/renderers/CSS2DRenderer.js';

import { Model3D } from '../Model3D';

class Galaxy extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
      cameraXYZ: { x: 0, y: 0, z: 25 },
      scene: null,
      camera: null,
      raycaster: null,
      intersectedObject: null,
    }

    this.initScene = this.initScene.bind(this);
    this.loadScene = this.loadScene.bind(this);
    this.mouseMove = this.mouseMove.bind(this);
}

initScene(scene, width, height) {
  let camera = new THREE.PerspectiveCamera( 75, width / height, 0.1, 1000 );
  camera.position.x = this.state.cameraXYZ.x;
  camera.position.y = this.state.cameraXYZ.y;
  camera.position.z = this.state.cameraXYZ.z;

  let raycaster = new THREE.Raycaster();

  this.setState({
    scene: scene,
    camera: camera,
    raycaster: raycaster,
    intersectedObject: null,
  });

  return camera;
}

loadScene(data) {
    let textureLoader = new THREE.TextureLoader();
    let starTexture = textureLoader.load("textures/galaxy/star.png");
    let gateMaterial = new THREE.LineBasicMaterial({
      color: 0x303030,
      lineWidth: 5,
      dashed: false,
    });

    for (const item of data.table.data) {
        const material = new THREE.SpriteMaterial( { map: starTexture, color: item.color } );
        const star = new THREE.Sprite(material);
        star.position.set(item.x, item.y, item.z);
        star.name = item.name;
				var starDiv = document.createElement( 'div' );
				starDiv.className = 'concorde-star-label';
				starDiv.textContent = star.name;
				starDiv.style.marginTop = '-1em';
				var starLabel = new CSS2DObject( starDiv );
				starLabel.position.set( 0.5, -0.1, 0 );
        star.add( starLabel );
        star.selectable = true;
        this.state.scene.add(star);      
        for (let i=1; item['gate' + i]; ++i) {
          let geometry = new THREE.Geometry();
          let from = [item.x, item.y, item.z];
          let dest = data.table.data[item['gate' + i] - 1];
          let to = [dest.x, dest.y, dest.z];

          for (let j=0; j < from.length; ++j) {
            let f = from[j];
            let t = to[j];
            from[j] = 0.1 * t + 0.9 * f;
            to[j] = 0.9 * t + 0.1 * f;
          }
          geometry.vertices.push(new THREE.Vector3(from[0], from[1], from[2]));
          geometry.vertices.push(new THREE.Vector3(to[0], to[1], to[2]));
          let line = new THREE.Line(geometry, gateMaterial);
          this.state.scene.add(line);
        }
      }
  }

  mouseMove(mouse) {

    const { scene, camera, raycaster } = this.state;

    let intersectedObject = this.state.intersectedObject;

    if (raycaster) {
      raycaster.setFromCamera(mouse, camera);

      var intersects = raycaster.intersectObjects( scene.children );

      let object = null;

      for (const intersect of intersects)
      {
        if (intersect.object.selectable) {
          object = intersect.object;
          break;
        }
      }

      if ( object !== intersectedObject ) 
      {
        if ( intersectedObject ) {
          intersectedObject.material.color.setHex( intersectedObject.currentHex );
        }

        intersectedObject = object;

        if (intersectedObject) {
          intersectedObject.currentHex = intersectedObject.material.color.getHex();
          intersectedObject.material.color.setHex( 0xffff00 );
        }
      }

      this.setState({
        intersectedObject: intersectedObject,
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
      >
      </Model3D>
      );
  }

}

export { Galaxy };