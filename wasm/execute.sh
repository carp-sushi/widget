#!/bin/bash 

echo 'widget'

# Should succeed
cat data/input.json \
  | wasmtime cmd.wasm \
  | jq -r '.widget'

echo 'errors'

# Should produce errors
cat data/errors.json \
  | wasmtime cmd.wasm \
  | jq -r '.errors'

