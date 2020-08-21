#!/bin/bash
for g in ccs trunc raw ahrq
do
	for m in l1 l2 rf xgb
	do 
		./Code/run_model.R -g $g -m $m
	done
done
