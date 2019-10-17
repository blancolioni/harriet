import React from 'react'
import * as THREE from "three";

import { Model3D } from '../Model3D';
import { ImprovedNoise } from "three/examples/jsm/math/ImprovedNoise";

class TerrestrialWorld extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
      cameraXYZ: { x: 1, y: 0, z: 4 },
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

  var light1 = new THREE.DirectionalLight( 0xffffff, 1.5 );
  light1.position.set( -2, 0, 4 );
  scene.add( light1 );

  // var light2 = new THREE.DirectionalLight( 0xffffff, 1.5 );
  // light2.position.set( -2, 0, 1 );
  // scene.add( light2 );

  this.setState({
      scene: scene,
      camera: camera,
      raycaster: raycaster,
      intersectedObject: null,
    });

  return camera;
}



fractalNoise(perlin, position, octaves, frequency, persistence) {
  let total = 0.0; // Total value so far
  let maxAmplitude = 0.0; // Accumulates highest theoretical amplitude
  let amplitude = 1.0;
  for (let i = 0; i < octaves; i++) {

      // Get the noise sample
      total += perlin.noise(position.x * frequency, position.y * frequency, position.z * frequency) * amplitude;

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

generateHeight(width, height) {
  var size = width * height, data = new Uint8Array( size ),
  perlin = new ImprovedNoise(), quality = 1;
  let normals = [];
  for(let i=0; i<size; ++i) {
    let x = i % width;
    let y = ~ ~ (i / width);
    let theta = y / height * Math.PI;
    let gamma = x / width * 2 * Math.PI;
    let nx = Math.sin(theta) * Math.cos (gamma);
    let ny = Math.sin(theta) * Math.sin (gamma);
    let nz = Math.cos(theta);
    normals.push({x: nx, y:ny, z:nz });
  }

  let totalH = 0, maxH = -1, minH = 1;
  let totalD = 0, minD = 255, maxD = 0;

  let hs = [];

  for ( var i = 0; i < size; i ++ ) {
    let p = normals[i];
    let h = (this.fractalNoise(perlin, p, 8, 100, 0.5) + 1) / 2;
    totalH += h;
    maxH = Math.max(maxH, h);
    minH = Math.min(minH, h);
    hs.push(h);
  }

  for(let i=0;i<size;++i) {
    let h = (hs[i] - minH) * 256 / (maxH - minH);
    data[i] = h;
    totalD += data[i];
    minD = Math.min(minD, data[i]);
    maxD = Math.max(maxD, data[i]);
  }

  let metrics = { averageHeight: totalH / size, minHeight: minH, maxHeight: maxH, averageData: totalD / size, minData: minD, maxData: maxD };
  console.log(metrics);
  return data;
}

generateTexture(data, palette, width, height) {
  var canvas, context, image, imageData;
  canvas = document.createElement( 'canvas' );
  canvas.width = width;
  canvas.height = height;
  context = canvas.getContext( '2d' );
  context.fillStyle = '#000';
  context.fillRect( 0, 0, width, height );
  image = context.getImageData( 0, 0, canvas.width, canvas.height );
  imageData = image.data;
  for ( var i = 0, j = 0, l = imageData.length; i < l; i += 4, j ++ ) {
    imageData[i] = palette[data[j]].r;
    imageData[i+1] = palette[data[j]].g;
    imageData[i+2] = palette[data[j]].b;
  }
  context.putImageData( image, 0, 0 );
  return canvas;
}

circleGeometry() {
  var vertices = [];
  var divisions = 50;
  for ( var i = 0; i <= divisions; i ++ ) {
    var v = ( i / divisions ) * ( Math.PI * 2 );
    var x = Math.sin( v );
    var z = Math.cos( v );
    vertices.push( x, 0, z );
  }
  var geometry = new THREE.BufferGeometry();
  geometry.addAttribute( 'position', new THREE.Float32BufferAttribute( vertices, 3 ) );
  return geometry;
}

loadScene(data) {

  const w = data.world;
  let circle = this.circleGeometry();

  for (const ship of w.ships) {
    let material = new THREE.LineBasicMaterial( {
      color: ship.color,
      linewidth: 5
    } );
    let line = new THREE.Line( circle, material );
    line.scale.setScalar( ship.orbit );
    line.rotateX(ship.inclination);
    this.state.scene.add( line );
  }

    const detail = w.detail || 128;
    const worldWidth = detail * 2;
    const worldHeight = detail;
    const { x, y, z } = data.origin || { x: 0, y: 0, z:0 }
    const hs = this.generateHeight(worldWidth, worldHeight);
    const palette = w.palette;

    const texture = new THREE.CanvasTexture(this.generateTexture(hs, palette, worldWidth, worldHeight));
    
    let geometry = new THREE.SphereBufferGeometry(w.radius, 32, 32);
    geometry.rotateY(Math.PI / 2);
    let material = new THREE.MeshLambertMaterial( {
      map: texture,
    } );
    let mesh = new THREE.Mesh( geometry, material );
    mesh.position.set(x,y,z);
    this.setState({worldMesh: mesh});
    this.state.scene.add( mesh );
  }

  mouseMove(mouse) {

  }

  beforeRender() {
    if (this.state.worldMesh) {
      let rot = this.state.rotation;
      this.state.worldMesh.rotateOnAxis(new THREE.Vector3(0, 1, 0), 0.001);
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

export { TerrestrialWorld };