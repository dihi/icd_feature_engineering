#!/bin/bash
echo "Beginning Experiments" && \
echo "Creating Diagnoses Tables..." && \
./01_create_dx_tables.sh && echo "Creating Final Matrices..." && \
./02_create_mimic_final.sh && echo "Running Models..." &&\
./03_run_experiments.sh && echo "Running bootstrap for Confidence Intervals..." &&\
./04_run_bootstrap.sh && echo "Generating figures..." &&\
./05_generate_graphs.sh
