#!/bin/bash

RUN_SCRIPT=/people/turn652/GCAM/gcam-5/exe/run-gcam_.sh

sbatch -J watergap_med_noCC_tax10 $RUN_SCRIPT
sbatch -J watergap_med_noCC_tax15 $RUN_SCRIPT
sbatch -J watergap_med_noCC_tax20 $RUN_SCRIPT
sbatch -J watergap_med_noCC_tax25 $RUN_SCRIPT

