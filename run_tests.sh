#!/bin/bash

echo "Running EUnit tests..."
rebar3 as test eunit

echo "Running Common Tests..."
rebar3 as test ct -v

if [ $? -eq 0 ]; then
    echo "All tests passed!"
else
    echo "Some tests failed. Check the output above for details."
fi
