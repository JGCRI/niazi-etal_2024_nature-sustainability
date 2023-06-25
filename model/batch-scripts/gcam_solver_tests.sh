#!/bin/bash

RUN_SCRIPT=/people/turn652/GCAM/gcam-5/exe/run-gcam_shared.sh

#sbatch -J wg_md_x $RUN_SCRIPT
sbatch -J solve_test_000 $RUN_SCRIPT
#sbatch -J solve_test_001 $RUN_SCRIPT
#sbatch -J solve_test_002 $RUN_SCRIPT




