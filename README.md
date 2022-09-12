# Medjai: A Symbolic Execution Tool for Cairo

<div>Medjai is an open-sourced general framework for reasoning about <img src="./docs/cairo-icon.png" width=24px> Cairo programs based on symbolic execution.</div>

***Note: Medjai is still under active development. For development notes, please see [here](./DEV.md).***

## Features

- [x] [Dev] **Program Exploration**: Medjai can execute Cairo program with symbolic inputs and explore all its possible program states.
- [x] [Dev] **Property Verification**: Medjai can check whether certain properties hold on Cairo program.
- [ ] [Dev] **Attack Synthesis**: Medjai can automatically solve for concrete inputs that crash given Cairo program.
- [ ] [Dev] **Integrations with <img src="./docs/veridise-icon.png" width=24px> Veridise Product Lines**: Medjai integrates with [[V] specication language](https://github.com/Veridise/V) that allows developers to express correctness properties.

## Setup and Build

- Cairo (0.8.2 Tested): [https://www.cairo-lang.org/](https://www.cairo-lang.org/)
- Racket (8.0+): [https://racket-lang.org/](https://racket-lang.org/)
  - Rosette (4.0+): [https://github.com/emina/rosette](https://github.com/emina/rosette)
    - `raco pkg install rosette`
- V (latest): [https://github.com/Veridise/V](https://github.com/Veridise/V)
  - *This dependency is optional*; needed only for parsing specifications in [V]. Can be ignored when using specifications in Cairo.
  - Follow build instructions: https://github.com/Veridise/V/blob/main/README.md
  - Add path to `parse` to `PATH` variable
- Cairo Compiler: [https://github.com/Veridise/pip-cairo-lang](https://github.com/Veridise/pip-cairo-lang)
  - We use a custom Cairo/Starknet compiler. Instructions to install are in the repo's README.

## Quickstart: Running An ERC20 Demo (Docker)

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

## End-To-End Example
This section shows how you can use Medjai to verify a protocol end-to-end. The file [MCD/contracts/vat.cairo](MCD/contracts/vat.cairo) contains a StarkNet implementation of MakerDAO's Vat protocol. 

### Building Vat

To build `Vat.cairo`, run 
```commandline
cp -r utils/veridise MCD/
./compile-vat.sh
```

this should generate a file ```vat_compiled.json```. 
### Maker DAO's VAT Protocol
We now show how you can use Medjai to prove properties about your Cairo programs. Consider the external method `move` from MakerDAO's [Vat.cairo](MCD/contracts/vat.cairo) shown below

```javascript
# function move(address src, address dst, uint256 rad) external {
@external
func move{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}(src : felt, dst : felt, rad : Uint256):
    alloc_locals

    check(rad)

    # require(wish(src, msg.sender), "Vat/not-allowed");
    let (caller) = get_caller_address()
    # let caller = 0
    let (src_consents) = wish(src, caller)
    with_attr error_message("Vat/not-allowed"):
        assert src_consents = 1
    end

    # dai[src] = dai[src] - rad;
    let (dai_src) = _dai.read(src)
    let (dai_src) = sub(dai_src, rad)
    _dai.write(src, dai_src)

    # dai[dst] = dai[dst] + rad;
    let (dai_dst) = _dai.read(dst)
    let (dai_dst) = add(dai_dst, rad)
    _dai.write(dst, dai_dst)

    # TODO
    # emit Move(src, dst, rad);

    return ()
end
```

`move` moves `rad` amount of Dai from address `src` to destination address `dst`. 
Suppose we want to use Medjai to prove that no matter what values we give for `src`, `dst`, and `rad` that the amount of dai in `src` should always decrease (or stay the same). Our first step is to create a specification for MEdjai.

### Creating a specification

Medjai can accept multiple forms of specification but in this example we will express the spec as a Cairo method (shown below and found in [vatspec.cairo](MCD/contracts/vatspec.cairo)). 

```javascript
@external
func move_demo_spec{
    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*
}():
    alloc_locals
    local otherUser
    local src
    local dst

    let (local src) = medjai_make_symbolic_felt()
    let (local dst) = medjai_make_symbolic_felt()

    # _dai.write(src, Uint256(low=0, high=340282366920938463463374607431768211455))
    let (local daiSrcBefore : Uint256) = _dai.read(src)
    medjai_assume_valid_uint256(daiSrcBefore)  # Assume the value is a valid uint256
    let (local daiDstBefore : Uint256) = _dai.read(dst)
    medjai_assume_valid_uint256(daiDstBefore)  # Assume the value is a valid uint256

    let (local rad : Uint256) = medjai_make_symbolic_uint256()  # Checked in move
    move(src, dst, rad)

    let (local daiSrcAfter : Uint256) = _dai.read(src)
    medjai_assert_le_uint256(daiSrcAfter, daiSrcBefore)
    return ()
end
```

The test first creates symbolic `src` and `dst` addresses by calling `medjai_make_symbolic_felt`. Next, it tells Medjai to assume that `src` and `dst` are well formed Uint256s via `medjai_assume_valid_uint256`. 
Afterwards, it creates a symbolic uint256 for `rad` and calls `move`. 
Finally, it tells Medjai to check whether the amount of Dai in the `src` address after the call is less than or equal to the amount before the call via `medjai_assert_le_uint256`.   

### Using Medjai

To use Medjai to check the specification we just need to do the following

```commandline
racket ./cairo-run.rkt --starknet --cname vat_compiled.json --entry move_demo_spec
```

## Property Specification

### Cairo Specifications

Medjai takes correctness specifications in the form of Cairo programs which emit special assertions and assumptions.
To create a specification, users write a test as a Cairo function that performs some actions,
including assuming and asserting properties using the
[Medjai verification library](https://github.com/Veridise/Medjai-Dev/blob/main/utils/veridise/cairo/verification.cairo).
Note that Cairo `assert` statements are treated as assumptions for Medjai.
This is because, by default, Medjai will not consider reverts to be bugs.

Cairo specifications for Medjai can be compiled in the same way as normal Cairo contracts.
See [StarkNet documentation](https://www.cairo-lang.org/docs/hello_starknet/intro.html) for instructions on compiling Cairo contracts.

### \[V\] Specifications

\[V\] is a concise specification language developed by Veridise.
In addition to taking Cairo specifications as input, Medjai can also automatically convert specifications in \[V\] to an equivalent Cairo specification.
Generally, specifications in \[V\] are much smaller than their Cairo counterparts, so specifying properties is usually quicker and easier in \[V\] than in Cairo.
However, for cases where symbolic execution performance is a concern, hand-written Cairo specifications are more desired.
Note that since Medjai's \[V\]-to-Cairo parser can be used standalone, users can write specifications in \[V\], convert them to Cairo, and optimize as needed.

Below is an example \[V\] specification that expresses the same constraints as the specification for `move` in the above end-to-end example:
```
vars: contract c
spec: finished(c.move(src, dst, rad),
               valid_uint256(old(_dai.read(src))) && valid_uint256(old(_dai.read(dst)))
               |=>
               _dai.read(src) <= _dai.read(dst)
```

For full documentation on \[V\] specifications, see: [https://github.com/Veridise/V/blob/main/docs/statements.rst](https://github.com/Veridise/V/blob/main/docs/statements.rst).

## Commands & Usages

### Using Cairo Specification
`racket cairo-run.rkt [options]`

where options include:  
    &emsp;`--cname <path>`: Path to a compiled Cairo program  
    &emsp;`--entry <func>`: Name of a Cairo function to run  
    &emsp;`--starknet`: Used to indicate compiled file is `%lang starknet`  
    &emsp;`--track-revert`: Indicates testing necessary conditions for revert  
    &emsp;`--verbose`: Print all output  
    &emsp;`--silent`: Print no output  
    &emsp;`--ambivalent`: Used to indicate that Medjai should ignore hints

### Using \[V\] Specification
`run-medjai.sh <path-to-v.spec> <path-to-header.cairo> <source-dir> <contract-dir>`

where each option specifies:  
    - `path-to-v.spec`: \[V\] specificaiton  
    - `path-to-header.cairo`: Cairo file that defines imports used by the specification  
    - `source-dir`: Source directory from which `starknet-compile` should be run  
    - `path-to-header.cairo`: Subdirectory within `source-dir` that contains the contract file (possibly `.`)  

### Parsing \[V\] into Cairo specifications
`racket parser-run.rkt [options] <v-spec.json>`

where options include:  
    &emsp;`--header <path>`: Path to a header for Cairo specifications (`.cairo` file)  
    &emsp;`--spec-name <func>`: Name of the generated specification function  
    &emsp;`--output <path>`: Path to the output file (`.cairo`)
 
