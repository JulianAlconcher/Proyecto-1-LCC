export function numberToColor(num) {
    switch (num) {
        case 2: return "#239cd1";
        case 4: return "#ee8a3c";
        case 8: return "#e25a88";
        case 16: return "#ad4e7c";
        case 32: return "#a63c4b";
        case 64: return "#8d6ebc";
        case 128: return "#118182";
        case 256: return "#52d5b2";
        case 512: return "#f8958e";
        case 1024: return "#94db5d";
        case 2048: return "#e85164";
        case 4096: return "#4878f1";
        default: return "black";
    }
}

export const equalPos = (posA, posB) => posA.toString() === posB.toString();

export const valueInPos = (pos, grid, numOfColumns) => {
    return grid[pos[0] * numOfColumns + pos[1]];
}

export const posInPath = (pos, path) => {
    return path.some(posI => equalPos(posI, pos));
}

export const connectionInPath = (posA, posB, path) => {
    return path.some((pos, i) => equalPos(pos, posA) && i + 1 < path.length && equalPos(path[i + 1], posB));
}

export const isAdyacent = (posA, posB) => {
    return !equalPos(posA, posB) && Math.abs(posA[0] - posB[0]) <= 1 && Math.abs(posA[1] - posB[1]) <= 1;
}

const smallerPow2GreaterOrEqualThan = (num) => {
    const log2num = Math.floor(Math.log2(num));
    return Math.pow(2, log2num) === num ? num : Math.pow(2, log2num + 1);
}

export const joinResult = (path, grid, numOfColumns) => smallerPow2GreaterOrEqualThan(path.reduce((result, pos) => result + valueInPos(pos, grid, numOfColumns), 0));
