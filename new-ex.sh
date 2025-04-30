#!/bin/bash
n=$1
ex=ex$(printf "%02d" "$n")
mkdir -p $ex
cat > $ex/Main.hs <<EOF
module Main where

main :: IO ()
main = putStrLn "$ex"
EOF

echo "
  $ex:
    main: Main.hs
    source-dirs: $ex
    ghc-options: -Wall" >> package.yaml

