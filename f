#!/bin/bash
find src prog-src tests -name "*.hs"
find src prog-src tests -name "*.hsc"
find cbits -name "*.[ch]"
echo hs-plasma.cabal
