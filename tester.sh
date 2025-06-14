#! /bin/bash

M=1 ; cat input.txt | while read X ; 
do 
	N=1 ; 
	echo "$X" | sed 's/\(.\)/\1\n/g' | sed '/^$/d' | while read Y ; 
	do 
		echo "$M,$N,$Y" ; 
		N=$(($N+1)); 
	done; 
	M=$(($M+1)); 
done | stack run sudoku

