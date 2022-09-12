#!/bin/zsh
cd MCD;
starknet-compile contracts/vatspec.cairo --output ../vat_compiled.json --gen_stubs;
cd ..
