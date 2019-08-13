#! /bin/bash

if [[ "" == "$1" ]]
then
    cat << EOF
Syntax: $0 [difficulty]

Example:
$0 00

EOF
    exit 1

fi

for (( i=1; i<=1000000; i++ ))
do
    TEXT="Vires in numeris. $i" 
    SHA256=`echo -n "$TEXT" | shasum -a 256`
    if [[ $SHA256 == $1* ]]
    then  
        echo "$i : $TEXT : $SHA256"
        break
    fi
done
