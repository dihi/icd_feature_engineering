# ICD Feature Engineering

This repository contains all of the code and infrastructure necessary to reproduce the results of an experiment examining different representations of ICD codes and their contribution to performance of machine learning models on the MIMIC-III database. In order to adhere to the values of reproducible research, the entire experiment is easily reproducible cross-platform with the help of Docker.

## Requirements

Before you begin, make sure you have access to the MIMIC-III Database. You will need the `ADMISSIONS.csv`, `DIAGNOSES_ICD.csv` and `PATIENTS.csv` files. In addition, make sure that you have Docker installed: [Docker](https://docs.docker.com/install/).

## Instructions

1. Clone the repository

```git clone https://github.com/dihi/icd_feature_engineering.git```

2. Navigate to the `Code` folder and run the following to initialize the directories

```./00_create_directories.sh```

3. Navigate to the top-level directory and build the dockerfile

```cd .. && sudo docker build -t icd_feature_engineering .```

4. Run the following command which will mount the folder into the container and run the experiment. Replace `<path_to_top_level_directory>` with the full path on your system to the cloned repository

```docker run -v <path_to_top_level_directory>:/opt/ -w /opt/Code icd_feature_engineering ./run_entire_experiment.sh```