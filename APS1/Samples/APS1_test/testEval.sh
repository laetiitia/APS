#! /bin/bash

for i in `ls ./*.aps`
do
	echo $i " -> "
	../../prologTerm $i
  echo  " eval : "
  ../../eval $i

	echo -e
done