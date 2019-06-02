#!/bin/bash
for g in ccs truncated raw ahrq
do
	for m in l1 l2 rf xgb
	do 
		./run_model.R -g $g -m $m
	done
done
