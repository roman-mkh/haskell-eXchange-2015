Sample code from my talk at the Haskell eXchange 2015.

stack ghci --package sqlite-simple --package haxl --package transformers --package text --package time --package text-show --package containers --package hashable --package formatting --package http-client --package http-conduit --package bytestring
:set -XOverloadedStrings
stack build --watch-file BlogDB.hs
 ghcid -c "stack XTest.hs"

stack exec ghc-pkg -- list

 stack ghci Main.hs --ghci-options '+RTS -M20M'

:set
-fwarn-incomplete-uni-patterns 	Emit warnings for incomplete patterns in lambdas or pattern bindings
-fwarn-incomplete-patterns 	Warn on non-exhaustive patterns
-fwarn-overlapping-patterns

https://gitlab.com/spisemisu/remove-facebook-content

 /home/osboxes/.stack/snapshots/x86_64-linux/d9fbcb8bd83b4992d8820e5cfa898b47e148398a5b49bcd8cb15917a88be18ec/8.10.4/lib/x86_64-linux-ghc-8.10.4/hourglass-0.2.12-96iqIVDbXZlKOLxhgi4kiQ

