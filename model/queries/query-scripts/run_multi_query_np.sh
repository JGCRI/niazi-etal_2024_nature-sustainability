#!/bin/bash

#SBATCH -p shared
#SBATCH -n 2

R CMD BATCH multi_query_np.R