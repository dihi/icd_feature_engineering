#!/bin/bash
for i in ccs truncated raw ahrq
do
    Rscript mimic_dx_tables.R -g $i 
done
