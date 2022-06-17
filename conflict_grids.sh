#!/bin/bash
# PK conflict-grids research project
# Creates a docker container and runs all code to reproduce all results
# Adam Kunkel
# 28 October 2021

# set wd in powershell Set-Location -Path Z:\606-Repository\final_project
# Set the working directory
cd .

# Build the Docker Image
# ZW: should be ./logs
docker build -t conflict-grids .

# Deploy the container
docker run --rm -it -v "$(pwd)/data:/data" -v "$(pwd)/results:/results" -v "$(pwd)/scripts:/scripts" -v "$(pwd)/logs:/logs" pset-2

# Run the R script #
R CMD Rscript --verbose /scripts/data_cleaning.R &> "/logs/question-1.log"

# Exit
exit
docker exec mycontainer /path/to/test.sh
