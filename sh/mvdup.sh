#!/bin/bash

# clean up duplicates in media libraries (photos, music etc.)
# mv files that contain "-001" in the name to /tmp that look like duplicates
# i.e. if there's a file of the same size and the same name without "-001".

for i in `find .  -name *-001.*`
do
    j=`echo $i | sed -e s/\-001// `
    sizei=`wc "$i" | cut -d' ' -f1`
    sizej=`wc "$j" | cut -d' ' -f1`
    if [ $sizei -eq $sizej ]
    then
	echo "same size "$i" "$j
	mv $i /tmp/
    else 
	echo "NOT"
    fi
done
