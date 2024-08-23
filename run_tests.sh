#!/bin/bash

echo "Running EUnit tests..."
rebar3 eunit

echo "Running Common Tests..."
rebar3 ct -v

if [ $? -eq 0 ]; then
    echo "All tests passed!"
else
    echo "Some tests failed. Check the output above for details."
fi
