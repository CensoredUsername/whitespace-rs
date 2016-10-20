#!/bin/bash

set -o errexit

shopt -s globstar

cargo doc --no-deps

git clone --branch gh-pages --depth 1 "git@github.com:CensoredUsername/whitespace-rs.git" deploy_docs
cd deploy_docs
git config user.name "CensoredUsername"
git config user.email "cens.username@gmail.com"

# copy over the docs folder
cp ../target/doc/* ./ -r

git add . -A
git commit -m "Rebuild docs"
git push origin gh-pages
cd ..
rm deploy_docs -rf

exit
