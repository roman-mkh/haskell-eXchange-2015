Sample code from my talk at the Haskell eXchange 2015.

stack ghci --package sqlite-simple --package haxl --package transformers --package text --package time --package text-show --package containers --package hashable
:set -XOverloadedStrings
stack build --watch-file BlogDB.hs
 ghcid -c "stack XTest.hs"


:set
-fwarn-incomplete-uni-patterns 	Emit warnings for incomplete patterns in lambdas or pattern bindings
-fwarn-incomplete-patterns 	Warn on non-exhaustive patterns
-fwarn-overlapping-patterns