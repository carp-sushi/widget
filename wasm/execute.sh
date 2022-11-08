#!/bin/bash 

# Display input
echo 'input widget'
cat data/input.json | jq -r '.widget'
echo 'actions'
cat data/input.json | jq -r '.actions'

# Should succeed
echo 'output widget'
cat data/input.json \
  | wasmtime cmd.wasm \
  | jq -r '.widget'

# Should produce errors
echo 'bad input'
cat data/errors.json | jq
echo 'errors'
cat data/errors.json \
  | wasmtime cmd.wasm \
  | jq -r '.errors'

