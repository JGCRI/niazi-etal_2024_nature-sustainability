#!/bin/bash

RUN_SCRIPT=/people/turn652/GCAM/gcam-5/exe/run-gcam_shared.sh
cd `pwd`
RUN=${PWD##*/}
sbatch -J $RUN $RUN_SCRIPT