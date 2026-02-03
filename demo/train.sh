#!/bin/bash

set -e # exit if any simple command returns a non-zero exit code

min_bins=${1:-3}
max_bins=${2:-4}
executable=${3:-"train-cloud-microphysics"}

let subfloor=$min_bins-1
j=subfloor
while (( j++ < max_bins )); do
 echo ""
 echo "---------> Training with $j bins along each phase-space dimension <---------"
 max_inner=1000
 i=0
 while (( i++ < max_inner )); do

   if [ -f stop ]; then
     echo ""
     echo "---------> 'stop' file found -- removing 'stop' & exiting script <---------" 
     rm stop
     exit 0
   fi 

   echo ""
   echo "---------> Run $i <---------"
   ./"$executable" --base fiats-training-data/training --epochs 10000 --bins $j --report 1000 --tolerance "5.0E-04"

   if [ -f converged ]; then
     echo ""
     echo "---------> 'converged' file found exiting inner loop <-------------"
     break
   fi 
 done
 if [ -f converged ]; then
   echo "---------> removing 'converged' file <-------------"
   rm converged
 else
   echo ""
   echo "---------> train.sh: training with $j bins did not converge within $max_inner inner-loop iterations <-------------"
 fi
done
