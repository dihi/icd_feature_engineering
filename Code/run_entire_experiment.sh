#!/bin/bash
echo "Beginning Experiments" && \
echo "Creating Diagnoses Tables..." && \
./01_create_dx_tables.sh && echo "Done." && echo "Running Models..." &&\
./02_run_experiments.sh && echo "Done." && echo "Running bootstrap for Confidence Intervals..." &&\
./03_run_bootstrap.sh && echo "Done." && echo "Generating figures..." &&\
./04_generate_graphs.sh && echo "Done."
