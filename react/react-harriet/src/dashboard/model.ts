interface Anchor {
  left : number
  top : number
  right : number
  bottom : number
}

function splitV (anchor : Anchor) : Anchor[] {
  const { left, top, right, bottom } = anchor;
  const anchor1 = { left, top, right, bottom: top + (bottom - top) / 2};
  const anchor2 = {left, top: anchor1.bottom, right, bottom}
  return [anchor1, anchor2]
}

function splitH (anchor : Anchor) : Anchor[] {
  const { left, top, right, bottom } = anchor;
  const anchor1 = {left, top, right: left + (right - left) / 2, bottom};
  const anchor2 = {left: anchor1.right, top, right, bottom}
  return [anchor1, anchor2]
}

export class Box {

  id : number
  anchor : Anchor
  children : number[]
  clientId: number

  constructor(id : number, anchor : Anchor, children : number[] = [], clientId : number = -1) {
      this.id = id;

      this.anchor = anchor;
      this.children = children;
      this.clientId = clientId;

      this.createContainerBox = this.createContainerBox.bind(this);
      this.splitVertical = this.splitVertical.bind(this);
      this.splitHorizontal = this.splitHorizontal.bind(this);
      this.split = this.split.bind(this);
      this.concatLeaves = this.concatLeaves.bind(this);
      this.mapLeaves = this.mapLeaves.bind(this);
      this.setClient = this.setClient.bind(this);
  }

  createContainerBox(newChildIds : number[], anchors : Anchor[]) : Box[] {
    const newBox = new Box(this.id, this.anchor);
    newBox.children = newChildIds;
    let result : Box[] = [newBox];
    for(let i=0; i < newChildIds.length; ++i) {
      const newChild = new Box(newChildIds[i], anchors[i]);
      if (i === 0) newChild.clientId = this.clientId;
      result.push(newChild);
    }
    return result;
  }

  splitVertical(topId : number, bottomId : number) : Box[] {
    return this.createContainerBox([topId, bottomId], splitV(this.anchor));
  }

  splitHorizontal(leftId : number, rightId : number) {
    return this.createContainerBox([leftId, rightId], splitH(this.anchor));
  }

  split(horizontal : boolean, childIds : number[]) {
    return horizontal ? this.splitHorizontal(childIds[0], childIds[1]) : this.splitVertical(childIds[0], childIds[1]);
  }

  concatLeaves(get : (id : number) => Box) : Box[] {

    if (this.children.length > 0) {
        let acc : Box[] = []
        for (const child of this.children) {
            acc = acc.concat(get (child).concatLeaves(get));
        }
        return acc;
    } else {
        return [this];
    }

  }

  mapLeaves(get : (id : number) => Box, f : (box : Box) => any) : any[] {
    let acc = this.concatLeaves(get);
    return acc.map(f);
  }

  setClient(clientId : number) : void {
    this.clientId = clientId;
  }
}

export type State = {
  boxes: Box[];
};