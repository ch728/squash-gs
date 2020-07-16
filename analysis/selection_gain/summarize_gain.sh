#!/usr/bin/sh

grep "Univariate" asreml_out/gain.asr | tr -s " "| cut -d " " -f5 > tmp1

grep "Cycle" asreml_out/gain.sln | tr -s " " | cut -d " " -f4,5 | tr " " "," > tmp2

paste -d "," tmp1 tmp2 > gain_summmary.csv

rm tmp1 tmp2