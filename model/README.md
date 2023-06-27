# 900 GCAM runs: Key Data Assumptions, Model Inputs, Setup Files, Scripts, and Key Model Outputs 

This folder contains data and scripts required for reproducing the experiment ranging from steps related to setting up the model to running batch simulations to querying relevant outputs from 900 GCAM.

The folder should be completed by downloading and extracting files and folders from [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6480465.svg)](https://doi.org/10.5281/zenodo.6480465) as instructed in the data repository. 

For first-time users of GCAM, please follow the guidance on [GCAM wiki](http://jgcri.github.io/gcam-doc/toc.html) to setup GCAM or for background knowledge. 

<h3> Overview of the folders: </h3>

| Folder name          | Description |
|----                  |----|
| [`water-data/`](/water-data/)        | This folder contains assumptions and data specific to water system, such as runoff, groundwater supply, water demands, accessible water fractions etc, that are used to represent dynamics within water system and its connections with other sectors. 
| [`combined_impacts/`](/combined_impacts/)  | This folder supplies scenario-specific information related to the climate impacts on energy, water, land systems. These scenario-specific climate impacts input files (`.XMLs`) are the read in at the end of the [configuration files](/config_files/) as inputs to the model, representing the effect of changes in climate and it's representation in models on water supply, hydropower, crop yields, and building energy demand. 
| [`xml-creation/`](/xml-creation/)      | This folder contains scripts and files used to systematically list and create 900 configuration files for each scenario. Each configuration file specifies which scenario-specific inputs would be read-in.
| [`xml-batch/`](/xml-batch/)         | This folder provides batch input `.XML` files for water system that are required in each of the 900 simulation runs.
| [`config_files/`](/config_files/)      | This folder contains 900 configuration files for each simulation, containing simulation specific model settings and paths to files containing scenario-specific input conditions. 
| [`gcam-5/`](/gcam-5/) | This folder, currently empty, would contain the version of the GCAM model used for this study. The model folder ([`gcam-5.7z`](https://zenodo.org/record/6480465/files/gcam-5.7z?download=1)) could be downloaded from [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6480465.svg)](https://doi.org/10.5281/zenodo.6480465) and extracted here as `gcam-5/`.
| [`batch-scripts/`](/batch-scripts/)     | This folder contains shell scripts (`.sh`) used to run multiple GCAM scenario simulations on cluster in parallel.
| [`queries/`](/queries/)           | This folder contains structure for main queries used to extract the relevant [outputs](/outputs/) from 900 GCAM runs. The queries are provided in `.XML` format along with `.R` scripts that describe how the query would interact with 900 output databases. Shell `.sh` scripts are also provided to run these queries on cluster.
| [`outputs/`](/outputs/)           | This folder contains key outputs queried and collated from 900 GCAM runs. Some of the files of this folder are hosted in the data repository here [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6480465.svg)](https://doi.org/10.5281/zenodo.6480465). [outputs](/outputs/) folder contains [further information](/outputs/README.md) on each of the key output files. 

Complete this folder by following guidance provided in data repository here [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6480465.svg)](https://doi.org/10.5281/zenodo.6480465)