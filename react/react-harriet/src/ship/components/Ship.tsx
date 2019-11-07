import * as THREE from "three";
import { ShipObject, ComponentShape } from "../../system/model";

const componentGeometry = (shape : ComponentShape) : THREE.Geometry => {
  switch (shape) {
    case ComponentShape.Cuboid:
      return new THREE.BoxGeometry(1, 1, 1);
    case ComponentShape.Cylinder:
      return new THREE.CylinderGeometry();
    case ComponentShape.Ellipsoid:
      return new THREE.IcosahedronGeometry(1, 3);
    case ComponentShape.Truncated_Cone:
      return new THREE.CylinderGeometry(0.5, 1);
    default:
      return new THREE.DodecahedronGeometry();
  }
}

export function shipMesh(ship   : ShipObject) : THREE.Mesh {
  let mesh = new THREE.Mesh();

  for (const module of ship.modules) {
      const geometry = componentGeometry(module.shape);
      const material = new THREE.MeshStandardMaterial({color: "#ccc", wireframe: false});
      const childMesh = new THREE.Mesh(geometry, material);
      if (module.shape = ComponentShape.Truncated_Cone) {
        childMesh.rotateX(Math.PI / 2.0);
      }
      childMesh.scale.set(module.dx, module.dy, module.dz);
      childMesh.position.set (module.x, module.y, module.z);
      mesh.add(childMesh);
  }
  mesh.rotateY(Math.PI / 3.0);
  mesh.name = ship.id;
  return mesh;
}
