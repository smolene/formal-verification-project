# Formal verification project: typechecking with Datalog

## Setup
You need `sbt` and `racket` to run our project. You can find it at https://download.racket-lang.org/, but you can of course install it through `pacman` or your favorite package manager.

## Usage
Run `sbt` in the root folder. Then start a typecheck by running `run path-to-file`. If the path is not provided, it will run the demo file `src/test/test-dl.formal`. This folder also contains other tests and programs we tried to typecheck by hand.

The program will output:
1. The pretty print of the parsed program
2. The pretty print after uniquification of symbols
3. The generated datalog that will be passed on to the solver
4. The output of the racket solver (this may take a few minutes, it is very slow)

The file `src/test/output.formal` will contain the generated datalog + the header `src/main/datalog/header.dlog`, which contains the typing rules.

How to interpret the racket output: if it prints the same line as the query (the `typed(e,t)?` lines), then it typechecked correctly. If an empty line appears, then the typechecking failed. This is not a good user interface but it worked for our prototyping.

You can rerun the Datalog solver on its own by running `racket path-to-datalog-file`.
