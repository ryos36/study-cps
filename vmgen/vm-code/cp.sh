#!/bin/sh
SOURCE_DIR=../../cps-compiler/scm-script
i=$1
FILE=test-$i.vmc 
SRC_FILE=$SOURCE_DIR/$FILE
DST_FILE=./$FILE
if [ -e $SRC_FILE ] ;then
    sed -e 's/^((/((:code  (/' -e 's/^ /  /' $SRC_FILE  > $DST_FILE
    echo ')' >> $DST_FILE
else
    echo $SRC_FILE is not exist
fi
