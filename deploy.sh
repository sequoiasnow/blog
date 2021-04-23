#!/bin/bash

# Verify correct branch
git checkout develop

# Build new files
stack exec site clean
stack exec site build

# Commit
git add -A
git commit -m "Publish."

# Push
git push 
