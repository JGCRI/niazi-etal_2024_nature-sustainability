#!/bin/bash

#SBATCH -A im3
#SBATCH -t 5:00:00
#SBATCH -p shared
#SBATCH -n 4

R CMD BATCH query_all.R

