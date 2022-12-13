// I cheated again :) JS has really good APIs

import fs from "node:fs/promises";

const input = await fs.readFile("./Day13/input.txt", "utf-8");

const pairs = input.split("\n\n").map((line) => line.split("\n").map(JSON.parse));

const rightOrder = pairs.map((l, i) => [l, i + 1]).filter(([[a, b]]) => compare(a, b) === -1).map(([_, i]) => i);

console.log(rightOrder.reduce((a, b) => a + b));

const d1 = [[2]];
const d2 = [[6]];

const sorted = pairs.flat().concat([d1, d2]).sort(compare);

console.log((sorted.indexOf(d1) + 1) * (sorted.indexOf(d2) + 1));

function compare(a, b) {
  if (typeof a === "number" && typeof b === "number") {
    if (a !== b) {
      return a < b ? -1 : 1;
    }
  } else if (Array.isArray(a) && Array.isArray(b)) {
    for (let i = 0; i < Math.min(a.length, b.length); i++) {
      const result = compare(a[i], b[i]);
      if (result !== 0) {
        return result;
      }
    }
    if (a.length !== b.length) {
      return a.length < b.length ? -1 : 1;
    }
  } else if (typeof a === "number") {
    return compare([a], b);
  } else {
    return compare(a, [b]);
  }
  return 0;
}
