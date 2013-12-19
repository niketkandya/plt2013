#!/bin/zsh
source=$1
temp=`echo $source | wc -c`
len=`echo "$temp - 1" | bc`

{echo $len; echo $source} | ./bf.out
