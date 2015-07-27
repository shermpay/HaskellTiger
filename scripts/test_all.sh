#!/bin/bash

PROJ_DIR=$HOME/Dropbox/Projects/HaskellTiger
EXECUTABLE=$PROJ_DIR/.cabal-sandbox/bin/HaskellTiger
LOG_DIR=$PROJ_DIR/.cabal-sandbox/logs
TIGER_FLAGS=( '-p' '--parse' '-h' '--help' '-a' '--ast' )
flag=$1

num_passed=0
test_files=( ${@:2} )

usage() {
    echo 'usage: ./test_all.sh [TIGER_FLAG] [INPUT_FILE]'
    echo -e 'Possible Tiger Flags:'
    echo -e '  '${TIGER_FLAGS[@]}
}
echo 'FLAGS '${TIGER_FLAGS[@]}
if [[ $flag == '' ]] ; then
    usage
    exit 1
fi

print() {
    echo -e $1 | tee -a $logfile
}

# cabal install
for f in ${TIGER_FLAGS[@]}; do
    if [[ $flag == $f ]] ; then
        print 'Testing '$flag' flag'
        # Create a logging file
        logfile=$LOG_DIR'/test_'$(date +%s)'.log'
        touch $logfile
        # Iterate over all the files
        for file in ${test_files[@]}; do
            suffix=${file##*.}
            if [[ $suffix == 'tig' ]] ; then
                print '\e[33m'"$EXECUTABLE $flag $file"'\e[0m'
                print '----------------------------------------------------------------'
                # Run executable with flag on file
                $EXECUTABLE $flag $file | tee -a $logfile
                # PIPESTATUS captures the exit codes through the pipe
                ret=${PIPESTATUS[0]}
                print '----------------------------------------------------------------'
                # Output Pass/Fail
                if [[ $ret -eq 0 ]] ; then
                    (( num_passed++ ))
                    print '\e[30;42m''PASSED''\e[0m'
                else
                    print '\e[41m''FAILED''\e[0m'
                fi
                print '================================================================'
            else
                print $file' does not have suffix ".tig"'
            fi
        done

        # Output results
        total=${#test_files[@]}
        print 'Test Results'
        print '------------'
        print '+ Total:\t'$total
        print '+ Passed:\t'$num_passed
        print '+ Failed:\t'$(( $total - $num_passed ))

        exit 0
    fi
done

usage
exit 1
