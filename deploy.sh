#!/bin/bash

# Check that we aren't current running the website


if [[ $(ps aux | grep " stack" | wc -l) -ge 2 ]]; then
    echo "Please terminate the locally running site before proceeding!"
    exit 1
fi

# SOURCE:
# https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html

# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout develop

# Build new files
stack exec site clean
stack exec site build

# Get previous files
git fetch --all
git checkout -b master --track origin/master

# Overwrite existing files with new files
cp -a _site/. .

# Commit
git add -A
git commit -m "Publish: $@"

# Push
git push origin master:master

# Restoration
git checkout develop
git branch -D master
git stash pop
