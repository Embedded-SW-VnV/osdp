#!/bin/sh

# -cclib flags should be after csdp_stubs.o,
# there seems to be no way to fix that in ocamlbuild, hence this dirty hack

ARGS=$(echo "$*" | sed -e 's/\(\-cclib .*\) \([^ ]*\.o\)/\2 \1/')
echo "$0 calls to \"${ARGS}\""
${ARGS}
