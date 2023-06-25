#!/bin/bash

#SBATCH -p shared
#SBATCH -n 1

### CREATE constrained_water_supply xml from updated groundwater curves

module load java/1.8.0_31
java -jar /people/turn652/GCAM/gcam-5/input/gcam-data-system/_common/ModelInterface/src/CSVToXML.jar batch_water_supply_gleeson.xml