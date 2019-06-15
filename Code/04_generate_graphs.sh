#!/bin/bash
for m in l1 l2 rf xgb
	do 
		./graph_results.R -m $m
	done
