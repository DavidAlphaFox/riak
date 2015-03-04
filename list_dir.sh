#!/bin/sh

list_dir(){
    for file in $1/*
    do
        if [ -d $file ]
        then 
            echo $file
            list_dir $file
        elif [ -f $file ]
        then
            echo $file
        fi
    done

}

if [ $# = 0 ]
then
    echo at least 1 arguments
elif [ ! -d $1 ]
then
    echo not a directory
else    
    cd "$1"
    p=$(pwd)
    list_dir "$p"
fi
