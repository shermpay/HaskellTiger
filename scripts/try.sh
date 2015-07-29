#!/bin/bash

flag=$1
test_files=( ${@:2} )

cd src && runhaskell Main.hs $flag ../$test_files
