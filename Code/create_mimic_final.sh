#!/bin/bash
for i in ccs truncated raw ahrq
do
    Rscript mimic_final_dataset.R $i 
done
