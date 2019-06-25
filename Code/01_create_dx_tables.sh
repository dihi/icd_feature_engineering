#!/bin/bash
for i in ccs truncated raw ahrq
do
    Rscript ./Code/mimic_dx_tables.R -g $i 
done
