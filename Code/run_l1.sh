#!/bin/bash
for i in ccs truncated raw ahrq
do
    Rscript run_l1.R $i 
done
