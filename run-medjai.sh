#!/bin/zsh
vspec="$1" # path to [V] .spec file
header="$2" # path to .cairo file representing header of translated spec
srcdir="$3" # path to folder containing source Cairo files (where starknet-compile can be run)
contractdir="$4" # relative path within srcdir where the Cairo specification can be placed (so that header reads from correct files)

echo "# parsing ${vspec} to my-spec.json"
parse "${vspec}" > my-spec.json

echo "# parsing my-spec.json to ${srcdir}/${contractdir}/my-vatspec.cairo..."
racket parser-run.rkt --header ${header} --output "${srcdir}/${contractdir}/my-vatspec.cairo" my-spec.json

echo "# compiling ${srcdir}/${contractdir}/my-vatspec.cairo to my_vat_compiled.json..."
cd ${srcdir}
starknet-compile "${contractdir}/my-vatspec.cairo" --output ../my_vat_compiled.json --gen_stubs
cd ..

echo "# Running Medjai..."
racket ./cairo-run.rkt --starknet --cname my_vat_compiled.json --entry my_spec