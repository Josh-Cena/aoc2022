// I cheated :) Trees are hard in Haskell

import fs from "node:fs/promises";

const input = await fs.readFile("./Day7/input.txt", "utf-8");

const tree = {
  parent: null,
  children: new Map(),
};
let cur = tree;
for (const entry of input.substring(2).split("\n$ ")) {
  const cmd = entry.substring(0, 2);
  if (cmd === "cd") {
    const dirName = entry.substring(3);
    if (dirName === "..") {
      cur = cur.parent;
    } else if (dirName === "/") {
      cur = tree;
    } else {
      cur = cur.children.get(dirName);
    }
  } else {
    const files = entry.split("\n").slice(1);
    for (const file of files) {
      if (file.startsWith("dir")) {
        const dirName = file.substring(4);
        if (!cur.children.has(dirName)) {
          cur.children.set(dirName, { parent: cur, children: new Map() });
        }
      } else {
        const [size, fileName] = file.split(" ");
        if (!cur.children.has(fileName)) {
          cur.children.set(fileName, parseInt(size));
        }
      }
    }
  }
}

let sum = 0;
const choices = [];

function getSize(tree) {
  let total = 0;
  for (const dirent of tree.children.values()) {
    const size = typeof dirent === "number" ? dirent : getSize(dirent);
    if (typeof dirent === "object") {
      if (size < 100000) {
        sum += size;
      }
      choices.push(size);
    }
    total += size;
  }
  return total;
}

const total = getSize(tree);
console.log(sum);

const curUnused = 70000000 - total;
console.log(
  choices.sort((a, b) => a - b).find((size) => size + curUnused >= 30000000)
);
