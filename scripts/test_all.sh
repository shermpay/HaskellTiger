#!/bin/bash

EXECUTABLE=$HOME/Dropbox/Projects/HaskellTiger/.cabal-sandbox/bin/HaskellTiger
TIGER_FLAGS=( '-p' '--parse' '-h' '--help' )
flag=$1

num_passed=0
test_files=( ${@:2} )

for f in $TIGER_FLAGS; do
    if [[ $flag == $f ]] ; then
        echo 'Testing '$flag' flag'
        # Iterate over all the files
        for file in ${test_files[@]}; do
            suffix=${file##*.}
            if [[ $suffix == 'tig' ]] ; then
                echo -e '\e[33m'"$EXECUTABLE $flag $file"'\e[0m'
                echo '----------------------------------------------------------------'
                # Run executable with flag on file
                $EXECUTABLE $flag $file
                ret=$?
                echo '----------------------------------------------------------------'
                # Output Pass/Fail
                if [[ $ret -eq 0 ]] ; then
                    (( num_passed++ ))
                    echo -e '\e[30;42m''PASSED''\e[0m'
                else
                    echo -e '\e[41m''FAILED''\e[0m'
                fi
                echo '================================================================'
            else
                echo $file' does not have suffix ".tig"'
            fi
        done

        # Output results
        total=${#test_files[@]}
        echo 'Test Results'
        echo '------------'
        echo -e '+ Total:\t'$total
        echo -e '+ Passed:\t'$num_passed
        echo -e '+ Failed:\t'$(( $total - $num_passed ))

        exit 0
    fi
done

exit 1
