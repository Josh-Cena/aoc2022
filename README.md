# Advent of Code 2022

Language: ![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white) (9.4.3)
Package Manager: [Stack](https://docs.haskellstack.org/en/stable/)

This repo uses my standard AoC setup. Inputs are stored as `inputs/day{n}/{name}.txt`. By default `name` is `real` (the real question). To run a specific day's solution, use the following command:

```bash
stack run {day} {part} {name}
```

For example, to run the solution for day 1, part 2 with the example input:

```bash
stack run 1 2 ex
```

(And make sure that `inputs/day1/ex.txt` exists.)
