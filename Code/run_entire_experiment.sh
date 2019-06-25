#!/bin/bash
echo "Beginning Experiments" && \
echo "Creating Diagnoses Tables..." && \
./Code/01_create_dx_tables.sh && echo "Done." && echo "Running Models..." &&\
./Code/02_run_experiments.sh && echo "Done." && echo "Running bootstrap for Confidence Intervals..." &&\
./Code/03_run_bootstrap.sh && echo "Done." && echo "Generating figures..." &&\
./Code/04_generate_graphs.sh && echo "Done."
