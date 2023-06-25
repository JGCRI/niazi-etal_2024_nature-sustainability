#!/bin/bash

RUN_SCRIPT=/people/turn652/GCAM/gcam-5/exe/run-gcam_.sh

sbatch -J watergap_med_CC_gfdl_6p0 $RUN_SCRIPT
sbatch -J watergap_med_CC_had_6p0 $RUN_SCRIPT
sbatch -J watergap_med_CC_ipsl_6p0 $RUN_SCRIPT
sbatch -J watergap_med_CC_miroc_6p0 $RUN_SCRIPT
sbatch -J watergap_med_CC_noresm_6p0 $RUN_SCRIPT




