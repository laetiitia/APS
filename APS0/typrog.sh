#! /bin/bash

./prologTerm $@| swipl -s ./typage.pl -g main_stdin