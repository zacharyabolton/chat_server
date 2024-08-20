#!/bin/bash

# Function to clean up and exit
cleanup() {
    echo "Stopping watch_and_test.sh"
    kill $(jobs -p)
    exit 0
}

# Set up signal handling
trap cleanup SIGINT SIGTERM

while true; do
    find src test -type f \( -name "*.erl" -o -name "*.hrl" \) | entr -d ./run_tests.sh &
    wait $!
done
