import React from 'react'
import * as THREE from "three";

import { OrbitControls } from "three/examples/jsm/controls/OrbitControls";
import { CSS2DRenderer } from 'three/examples/jsm/renderers/CSS2DRenderer.js';

import { userService } from '../../../_services'
import { DashboardItem } from '../../DashboardItem';

class ModelCanvas extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      scene: null,
    }
    this.mountRef = React.createRef();
    this.mouse = new THREE.Vector2();
  }

  componentDidMount() {

    var scene = new THREE.Scene();
    this.props.onSceneCreated(scene);

    this.setState({
        scene: scene,
      });

    let itemElement = this.mountRef.current.closest(".concorde-dashboard-item");
    console.log("model3d", itemElement.clientWidth, itemElement.clientHeight);

    let width = itemElement.clientWidth; 
    let height = itemElement.clientHeight - 30;

    var renderer = new THREE.WebGLRenderer({ 
      antialias: true,
    });
    renderer.setSize(width, height);
    this.mountRef.current.appendChild( renderer.domElement );
    
    var labelRenderer = new CSS2DRenderer();
    labelRenderer.setSize( width, height );
		labelRenderer.domElement.style.position = 'absolute';
		labelRenderer.domElement.style.top = 0;
    this.mountRef.current.appendChild( labelRenderer.domElement );
    
    let camera = this.props.initScene(scene, width, height);

    let controls = new OrbitControls( camera, labelRenderer.domElement );

				//controls.addEventListener( 'change', render ); // call this only in static scenes (i.e., if there is no animation loop)

    controls.enableDamping = true; // an animation loop is required when either damping or auto-rotation are enabled
    controls.dampingFactor = 0.05;

    controls.screenSpacePanning = false;

    controls.minDistance = 5;
    controls.maxDistance = 50

    controls.maxPolarAngle = Math.PI / 2;

    if (this.props.mouseMove) {
      let mouse = this.mouse;
      let onDocumentMouseMove = function (event) {
        event.preventDefault();
        mouse.x = ( event.offsetX / width ) * 2 - 1;
        mouse.y = - ( event.offsetY / height ) * 2 + 1;
      }
      document.addEventListener( 'mousemove', onDocumentMouseMove, false );
    }

    let mouse = this.mouse;
    let mouseMove = this.props.mouseMove;
    let beforeRender = this.props.beforeRender;

    var animate = function () {
      requestAnimationFrame( animate );
      if (beforeRender) beforeRender();
      renderer.render( scene, camera );
      labelRenderer.render (scene, camera);
      if (mouseMove) {
        mouseMove(mouse);
      }
    };

    animate(this.mouse, this.props.mouseMove);

  }


  render() {
    return (
      <div ref={this.mountRef} style={{position: "relative"}}>

      </div>
    )
  }
}

class Model3D extends React.Component {

  constructor(props) {
    super(props);

    this.onConnected = this.onConnected.bind(this);
    this.setScene = this.setScene.bind(this);
}

onConnected(clientId) {
  this.getData(clientId);
}

  getData(clientId) {
    userService.postRequest('client/' + clientId, {data: 'get', sort: 0, ascending: true})
    .then((result) => result.json())   
    .then((resp) => {
        this.props.loadScene(resp);
    });
  }

  setScene(scene) {
    this.setState({
      scene: scene,
    });
  }

  render () {
      return (
        <DashboardItem 
          title={this.props.title} 
          model={this.props.model} 
          modelArg={this.props.modelArgs} 
          onConnected={this.onConnected} 
          onDashboardCommand={this.props.onDashboardCommand}
        >
          <ModelCanvas
            onSceneCreated={this.setScene}
            initScene={this.props.initScene || null}
            mouseMove={this.props.mouseMove || null}
            beforeRender={this.props.beforeRender || null}
          >
          </ModelCanvas>
        </DashboardItem>
      );
  }

}

export { Model3D };