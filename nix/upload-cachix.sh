#!/bin/bash

cd $(dirname $0)/..

nix flake archive --json \
| jq -r '.path, (.inputs|to_entries[].value.path)' \
| cachix push takoeight0821

nix build --json \
| jq -r '.[].outputs | to_entries[].value' \
| cachix push takoeight0821
