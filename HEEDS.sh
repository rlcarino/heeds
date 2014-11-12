#!/bin/bash                                                                                                                                    

VERSION=R2014-11-07
IP=127.0.0.1                                                                                                                                   
# There must be at least 4 arguments
if [ -z "$4" ]; then
    echo "USAGE: $0 UNIV YEAR TERM ACTION"
    exit 
fi

# University/Port_Number combination must be the same as in nginx.conf
declare -a University_Code=( CSU-Andrews CSU-Aparri CSU-Carig CSU-Gonzaga CSU-Lal-lo CSU-Lasam CSU-Piat CSU-Sanchez-Mira DEMO   ISU-Echague )
declare -a            Disk=( /data1      /data      /data     /data       /data      /data     /data    /data            /data1 /data1 )
declare -a     Port_Number=( 58010       58020      58030     58040       58050      58060     58070    58080            60000  60020  )
declare -a     BATCH_Action=( Import Checklists Restore Special )
declare -a     SERVER_Action=( Classlists Advising Gradesheets Preregistration )

# Search for matching university
for univ in ${!University_Code[*]}
do
    if [ $1 = ${University_Code[$univ]} ]; then
        #printf "%4d: %s, %d\n" $univ ${University_Code[$univ]} ${Port_Number[$univ]}
        # Search for matching BATCH_Action 
        for action in ${!BATCH_Action[*]}
        do
            if [ $4 = ${BATCH_Action[$action]} ]; then
		EXE=/home/HEEDS/src/HEEDS_STATIC-$VERSION
		if [ ! -x $EXE ]; then
		    echo Not found: $EXE
		    exit
		fi
                echo $EXE $* ${Disk[$univ]}
                $EXE $* ${Disk[$univ]}
                exit
            fi
        done
        # Search for matching SERVER_Action 
        for action in ${!SERVER_Action[*]}
        do
            if [ $4 = ${SERVER_Action[$action]} ]; then
		EXE=/home/HEEDS/src/HEEDS_SERVER-$VERSION
		if [ ! -x $EXE ]; then
		    echo Not found: $EXE
		    exit
		fi
	        if [ "$5" = 'Mirror' ]; then
	            echo spawn-fcgi -a $IP -p $((${Port_Number[$univ]}+1)) -f "$EXE $1-mirror $2 $3 $4 ${Disk[$univ]}"
	            spawn-fcgi -a $IP -p $((${Port_Number[$univ]}+1)) -f "$EXE $1-mirror $2 $3 $4 ${Disk[$univ]}"
	        elif [ "$5" = 'Rectify' ]; then
	            echo spawn-fcgi -a $IP -p $((${Port_Number[$univ]}+2)) -f "$EXE $1-rectify $2 $3 $4 ${Disk[$univ]}"
	            spawn-fcgi -a $IP -p $((${Port_Number[$univ]}+2)) -f "$EXE $1-rectify $2 $3 $4 ${Disk[$univ]}"
	        else
                    echo spawn-fcgi -a $IP -p ${Port_Number[$univ]} -f "$EXE $* ${Disk[$univ]}"
                    spawn-fcgi -a $IP -p ${Port_Number[$univ]} -f "$EXE $* ${Disk[$univ]}"
		fi
                exit
            fi
        done
        echo "Action '$4' not recognized"
        exit
    fi
done
echo "University '$1' not recognized"

