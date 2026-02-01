#!/usr/bin/env bash

set -e

# Ensure a predictable environment
export HOME=/home/arduino
export USER=arduino
export PATH=/usr/local/bin:/usr/bin:/bin

# Go to the Leiningen project root (where project.clj lives)
cd /home/arduino/replbox/replbox/scratch

# Start the REPL
exec lein repl :headless :host 0.0.0.0 :port 7888
