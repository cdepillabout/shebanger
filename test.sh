#!/usr/bin/env bash

echo "arguments to this script: $@"
echo

# Say hello world!
echo "hello!"
echo

# Show files in current directory
ls
echo

# Print numbers from 1 to 10
for i in `seq 1 10` ; do
    echo $i
done
echo

# List system information
uname
echo

# Curl some placeholder information.
response=$(curl -s https://jsonplaceholder.typicode.com/posts/1)

# If data was fetched successfully, display it.
if [ "$?" -eq 0 ]; then
    echo "Data fetched successfully:"
    echo "$response" | jq .
else
    echo "Failed to fetch data."
fi
