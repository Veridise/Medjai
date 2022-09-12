# Medjai: A Symbolic Execution Tool for Cairo

<div>Medjai is an open-sourced general framework for reasoning about <img src="./docs/cairo-icon.png" width=24px> Cairo programs based on symbolic execution.</div>

***Note: Medjai is still under active development. For development notes, please see [here](./DEV.md).***

## Features

- [x] [Dev] **Program Exploration**: Medjai can execute Cairo program with symbolic inputs and explore all its possible program states.
- [x] [Dev] **Property Verification**: Medjai can check whether certain properties hold on Cairo program.
- [ ] [Dev] **Attack Synthesis**: Medjai can automatically solve for concrete inputs that crash given Cairo program.
- [ ] [Dev] **Integrations with <img src="./docs/veridise-icon.png" width=24px> Veridise Product Lines**: Medjai integrates with [[V] specication language](https://github.com/Veridise/V) that allows developers to express correctness properties.

## Running An ERC20 Demo (Docker)

First you need to build the demo from the latest version. Make sure you have Docker installed, and then type in the following command to build an image:

```bash
cd Medjai/
docker build -t medjai:demo .
```

It should take a short time to set up the environment. Then use the following command to start a container:

```bash
docker run -it --rm medjai:demo bash
```

Then you will enter a pre-set docker environment.

### ERC20 Bug Detection

To test the buggy version, use the following command in the container:

```bash
cd Medjai/
racket ./cairo-run.rkt --cname ./benchmarks/overflowTest/erc20demo_bug_compiled.json
```

The tool will be invoked and run symbolic execution for bug detection. You'll see the following output if the command runs correctly:

```bash
Finished Symbolic Execution
Bug found with
total_supply = Uint256(1, 2)
amount = Uint256(340282366920938463463374607431768211455, 340282366920938463463374607431768211453)
```

### ERC20 Symbolic Execution

For a non-buggy version, use the following command in the container to verify it:

```bash
racket ./cairo-run.rkt --cname ./benchmarks/overflowTest/erc20demo_fix_compiled.json
```

The tool will be invoked and also run symbolic execution for bug detection. You'll see the following output if the command runs correctly:

```bash
Finished Symbolic Execution
No bugs found!
```

This means this version of ERC20 is good.

## Dependencies (Building from Source)

- Cairo (0.8.2 Tested): [https://www.cairo-lang.org/](https://www.cairo-lang.org/)
- Racket (8.0+): [https://racket-lang.org/](https://racket-lang.org/)
  - Rosette (4.0+): [https://github.com/emina/rosette](https://github.com/emina/rosette)
    - `raco pkg install rosette`

## Getting Started: Interpretation

The repo comes with the simple example that you can also find on the official Cairo documentation:

```cairo
# examples/test0.cairo: interpretation
func main():
    [ap] = 1000; ap++
    [ap] = 2000; ap++
    [ap] = [ap - 2] + [ap - 1]; ap++
    ret
end
```

where the user assigns two values `1000` and `2000` to corresponding memory addresses and assigns the result of adding them up to another memory address. By calling:

```bash
./run-medjai.sh ./examples/test0.cairo
```

Medjai can then execute the program and outputs the desired final memory states.

## Getting Started: Symbolic Reasoning (Dev)

Medjai also supports reasoning and you can utilize it to verify several properties of Cairo programs. For example:

```cairo
# examples/test1.cairo: symbolic reasoning
func main():
    [ap] = 1000; ap++
    [ap] = symbolic(type='integer', name='var0'); ap++
    [ap] = [ap - 2] / [ap - 1]; ap++
    ret
end
```

The above code snippet creates a symbolic integer `var0` and assigns it to a certain memory address. We would like to find out whether this piece of code is correct or not by asking Medjai to find a counterexample of `var0` that would potentially crash the execution of the program. By calling:

```bash
./run-medjai.sh ./examples/test0.cairo
```

Medjai will reason about all possible values of `var0` together with all program states, and return one of them that can compromise the program execution, which is `0` that would cause a "division by 0" error. 

As in the line of code after we create the symbolic variable, `var0` is immediately used as denominator of a division arithmetic operation `[ap] = [ap - 2] / [ap - 1]; ap++`, which will cause an "unknown value for memory cell"  exception if `var0` is set to `0`. This verifies the counterexample it found.

## Commands & Usages

### Using `run-medjai.sh`

```bash
usage: run-medjai.sh <path-to-cairo-program>
```

### Using `cairo-run.rkt`

```bash
usage: cairo-run.rkt [ <option> ... ]

<option> is one of

  --cname <p-cname>
     path to a compiled Cairo program (.json)
  --help, -h
     Show this help
  --
     Do not treat any remaining argument as a switch (at this level)

 /|\ Brackets indicate mutually exclusive options.

 Multiple single-letter switches can be combined after
 one `-`. For example, `-h-` is the same as `-h --`.
```
## Verifying MakerDAO Specs

### Downloading Cairo Compiler

Mejai currently expects all Starknet programs to be compiled using a fork of Cairo's compiler which can be found [here](https://github.com/Veridise/pip-cairo-lang). Make sure to install it before proceeding. This dependence is temporary and in the very near future we will not require it. 

### Specs

Example functional specifications for MakerDAO's `Vat` are found in `MCD/contracts/vatspec.cairo`.

### Running Medjai (Example)

The following commands show how you can use Medjai to verify a spec in `vatspec.cairo`.

```bash
cd MCD/
./compile-vat.sh
cd ../
racket ./cairo-run --starknet --cname vat_noauth_compiled.json --entry move_demo_spec
```

If everything ran successfully you should see 

```bash
[tokamak:log] done handling storage
[tokamak:log] second rvmod
[tokamak:log] third rvmod, context-pc: #(struct:rv 7 0)
[tokamak:log] context-pc after: #(struct:rv 7 0)
Finished Symbolic Execution
No bugs found!
```
