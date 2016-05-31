#!/bin/bash
VERSION=R2015-11-01
BINPATH=/home/HEEDS/src/v8
IP=127.0.0.1

# There must be at least 4 arguments
if [ -z "$4" ]; then
    echo "USAGE: $0 UNIV YEAR TERM ACTION"
    exit 
fi

# University/Port_Number combination must be the same as in nginx.conf
declare -a  University_Code=( CSU-Andrews CSU-Aparri CSU-Carig CSU-Gonzaga CSU-Lal-lo CSU-Lasam CSU-Piat CSU-Sanchez-Mira ASC   DEMO )
declare -a      Port_Number=( 56000       58020      58030     58040       58050      58060     58070    58080            59000 60000 )

declare -a      BATCH_Action=( Import Checklists Restore Special Passwords Payments Assessments Convert )

# Search for matching university
for univ in ${!University_Code[*]}
do
    if [ $1 = ${University_Code[$univ]} ]; then
        #printf "%4d: %s, %d\n" $univ ${University_Code[$univ]} ${Port_Number[$univ]}
        # Search for matching BATCH_Action 
        for action in ${!BATCH_Action[*]}
        do
            if [ $4 = ${BATCH_Action[$action]} ]; then
		EXE=$BINPATH/HEEDS_STATIC-$VERSION
		if [ ! -x $EXE ]; then
		    echo Not found: $EXE
		    exit
		fi
		echo $EXE $*
		$EXE $*
		exit
	    fi
	done

	# Mirror/Mobile ?
	if [ "$4" = 'Mirror' ]; then
	    EXE=$BINPATH/HEEDS_MOBILE-$VERSION
	    if [ ! -x $EXE ]; then
		echo Not found: $EXE
		exit
	    fi
	    echo spawn-fcgi -a $IP -p $((${Port_Number[$univ]})) -f "$EXE $*"
	    spawn-fcgi -a $IP -p $((${Port_Number[$univ]})) -f "$EXE $*"
	    exit
        fi

	# Server?
	if [ "$4" = 'Server' ]; then
	    EXE=$BINPATH/HEEDS_SERVER-$VERSION
	    if [ ! -x $EXE ]; then
		echo Not found: $EXE
		exit
	    fi
	    echo spawn-fcgi -a $IP -p $((${Port_Number[$univ]}+$2)) -f "$EXE $*"
	    spawn-fcgi -a $IP -p $((${Port_Number[$univ]}+$2)) -f "$EXE $*"
	    exit
        fi

	echo "Action '$4' not recognized"
        exit
    fi
done
echo "University '$1' not recognized"
