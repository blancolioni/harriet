import React, { useRef } from 'react';
import { Canvas, useFrame } from 'react-three-fiber';


import { State } from '../model';

import { ClientDispatch } from '../../clients/model';

interface Dispatch extends ClientDispatch {
}

interface Props {
    clientState : State,
    clientDispatch : Dispatch
}

interface Point {
  x : number
  y : number
}

interface Current {
  rotation: Point
}

function Thing() {
    const ref = useRef()
    
    useFrame(() => {
      const current = ref.current! as Current;
      current.rotation.x = current.rotation.y += 0.01;
    });
    return (
      <mesh
        ref={ref}
        onClick={e => console.log('click')}
        onPointerOver={e => console.log('hover')}
        onPointerOut={e => console.log('unhover')}>
        <boxBufferGeometry attach="geometry" args={[1, 1, 1]} />
        <meshNormalMaterial attach="material" />
      </mesh>
    )
  }

export default class World extends React.Component<Props,State> {
    
    render() {
        return (
            <Canvas>
                <Thing></Thing>
            </Canvas>
        );
    }
}
