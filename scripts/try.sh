#!/bin/bash

flag=$1
test_files=( ${@:2} )

cat -n $test_files
echo '>>>'
cd src && runhaskell Main.hs $flag ../$test_files
