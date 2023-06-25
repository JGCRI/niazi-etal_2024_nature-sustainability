#!/bin/bash

#SBATCH -p shared
#SBATCH -n 1

R CMD BATCH query_mindata.R

