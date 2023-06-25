#!/bin/bash

#SBATCH -A ihesd
#SBATCH -t 3:00:00
#SBATCH -p short
#SBATCH -n 4

R CMD BATCH query_all_ag.R
