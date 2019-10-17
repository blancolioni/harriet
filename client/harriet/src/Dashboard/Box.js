class Box {

    constructor(id, left, top = null, right = null, bottom = null) {
        this.id = id;
        if (typeof left === 'object') {
            this.anchor = {
                ...left,
            }
        } else {
            this.anchor = {
                left: left,
                top: top,
                right: right,
                bottom: bottom,
            }
        }
        this.childBoxes = null;
        this.childComponent = null;
    }
}

function splitVertical(box, topId,bottomId) {
    const { left, top, right, bottom } = box.anchor;
    const newBox = new Box(box.id, box.anchor);
    const topChild = new Box(topId, left, top, right, top + (bottom - top) / 2);
    topChild.childComponent = box.childComponent;
    const bottomChild = new Box(bottomId, left, topChild.anchor.bottom, right, bottom);
    newBox.childBoxes = [topId, bottomId];
    return [ newBox, topChild, bottomChild];
}

function splitHorizontal(box, leftId, rightId) {
    const { left, top, right, bottom } = box.anchor;
    const newBox = new Box(box.id, box.anchor);
    const leftChild = new Box(leftId, left, top, left + (right - left) / 2, bottom);
    leftChild.childComponent = box.childComponent;
    const rightChild = new Box(rightId, leftChild.anchor.right, top, right, bottom);
    newBox.childBoxes = [leftId, rightId];
    return [ newBox, leftChild, rightChild];
}

function concatLeaves(box, get) {
    if (box.childBoxes) {
        let acc = []
        for (const child of box.childBoxes) {
            acc = acc.concat(concatLeaves(get(child), get));
        }
        return acc;
    } else {
        return [box];
    }

}

function mapLeaves(box, get, f) {
    let acc = concatLeaves(box, get, box);
    console.log('mapLeaves', acc);
    return acc.map(f);
}

function setChildComponent(box, title, view, model, client) {
    box.childComponent = { title, view, model, client };
}

export { Box, splitVertical, splitHorizontal, concatLeaves, mapLeaves, setChildComponent }