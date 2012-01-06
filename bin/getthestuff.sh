#!/bin/bash
[ -f korrelation.csv ] && mv korrelation.csv korrelation.csv~
wget https://www.bmovieprojekt.tk/korrelation.csv --no-check-certificate
cat korrelation.csv | ./preparse.pl | ./cluster 2> cluster.log | ./deanonymize.pl korrelation.csv  > clusters.list 


