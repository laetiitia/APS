#! /bin/bash

for i in `ls ./*.aps`
do
	echo $i " -> "
	../../prologTerm $i
  echo  " typage : "
  ../../prologTerm $i| swipl -s ../../typrog.pl -g main_stdin

	echo -e
done