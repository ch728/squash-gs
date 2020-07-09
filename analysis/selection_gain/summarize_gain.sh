#!/usr/bin/sh

grep "Univariate" asreml/gain/gain.asr | tr -s " "| cut -d " " -f5 > data/tmp1

grep "Cycle" asreml/gain/gain.sln | tr -s " " | cut -d " " -f4,5 | tr " " "," > data/tmp2

paste -d "," data/tmp1 data/tmp2 > data/gain_summmary.csv

rm data/tmp1 data/tmp2