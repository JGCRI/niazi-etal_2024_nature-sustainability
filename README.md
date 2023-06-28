[![DOI](https://zenodo.org/badge/658384847.svg)](https://zenodo.org/badge/latestdoi/658384847) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6480465.svg)](https://doi.org/10.5281/zenodo.6480465)

# niazi-etal_202X_xyz

**Global Peak Water Limit of Future Groundwater Withdrawals**

Hassan Niazi<sup>1\*</sup>, Thomas Wild<sup>1</sup>, Sean Turner<sup>2</sup>, Neal Graham<sup>1</sup>, Mohamad Hejazi<sup>3</sup>, Siwa Msangi<sup>4</sup>, Son Kim<sup>1</sup>, Jonathan Lamontagne<sup>5</sup>, and Mengqi Zhao<sup>2</sup>

<sup>1 </sup> Joint Global Change Research Institute, Pacific Northwest National Laboratory (JGCRI-PNNL), College Park, MD, USA.
<sup>2 </sup> Pacific Northwest National Laboratory (PNNL), Richland, WA, USA.
<sup>3 </sup> King Abdullah Petroleum Studies and Research Center (KAPSARC), Riyadh, Saudi Arabia.
<sup>4 </sup> Economic Research Service, United States Department of Agriculture, Washington DC, USA.
<sup>5 </sup> Tufts University, Boston, MA, USA.

\* corresponding author: hassan.niazi@pnnl.gov

## Abstract
Using GCAM, we simulate groundwater withdrawals across 235 water basins under 900 scenarios covering a variety of global change drivers over the 21st century. We find that global groundwater withdrawals robustly peak around mid-century, followed by a consistent decline through 21st century, exposing about half of the population living in one-third of basins to groundwater stress, with cost and availability of surface water storage being the most significant driver of future groundwater withdrawals. This first-ever robust, quantitative confirmation of the peak-and-decline pattern for groundwater, previously only known for fossil fuels and minerals, raises concerns for basins heavily dependent on groundwater for food productions and meeting other water demands. 

## Journal reference
To be updated later

## Repository Overview 

| Item  | Purpose        | Key folders and files | 
|-------|----------------|-----------------------|
| [`model/`](/model/)	 | Contains data and scripts required for [reproducing the experiment](#reproduce-the-experiment) ranging from steps related to setting up the model to running batch simulations to querying relevant outputs from 900 GCAM runs	          |   [`water-data/`](model/water-data/), [`combined_impacts/`](model/combined_impacts/), [`xml-creation/`](model/xml-creation/), [`xml-batch/`](model/xml-batch/), [`config_files/`](model/config_files/), [`gcam-5/`](model/gcam-5/), [`batch-scripts/`](model/batch-scripts/), [`queries/`](model/queries/), and [`outputs/`](model/outputs/) folders
| [`processing/`](/processing/)	| Contains scripts and files required to [reproduce the analysis](#reproduce-the-analysis)	and visualizations as presented in the [paper](paper)          | [`inputs/`](/processing/inputs/) and  [`ouputs/`](/processing/outputs/) folders along with `.R` scripts for analysis    |
| [`figures/`](/figures/) | Contains major figures presented in the manuscript | [figures](/figures/) and [shape files](/figures/shapefiles/) to plot maps |
| data  | Associated data repository to host larger files and folders. This code repository must be complemented with data from [data repository](https://doi.org/10.5281/zenodo.6480465) | [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6480465.svg)](https://doi.org/10.5281/zenodo.6480465) |



## Reproduce the Experiment
The files and scripts presented in the [model/](/model/) folder along with data from the data repository [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6480465.svg)](https://doi.org/10.5281/zenodo.6480465) such as version of the model ([`gcam-5`](https://zenodo.org/record/6480465/files/gcam-5.7z?download=1)) could be used to reproduce the experiment. 

For first-time users of GCAM, please follow the guidance on [GCAM wiki](http://jgcri.github.io/gcam-doc/toc.html) to setup GCAM or for background knowledge. 

Generally, running GCAM has three major steps: 
1. preparing the scenario-specific input data and formatted `.xml` files as inputs to GCAM 
2. running GCAM using [configuration files](/model/config_files/). In the context of this project running GCAM 900 times in parallel on cluster also requires [batch-scripts](model/batch-scripts/) 
3. quering the databases for outputs of interest to be later used in [post-processing and analysis](/processing/) scripts 

Overview of the [`model/`](/model/) folder:

| Folder name          | Description |
|----                  |----|
| [`water-data/`](/model/water-data/)        | This folder contains assumptions and data specific to water system, such as runoff, groundwater supply, water demands, accessible water fractions etc, that are used to represent dynamics within water system and its connections with other sectors. 
| [`combined_impacts/`](/model/combined_impacts/)  | This folder supplies scenario-specific information related to the climate impacts on energy, water, land systems. These scenario-specific climate impacts input files (`.XMLs`) are the read in at the end of the [configuration files](/model/config_files/) as inputs to the model, representing the effect of changes in climate and it's representation in models on water supply, hydropower, crop yields, and building energy demand. 
| [`xml-creation/`](/model/xml-creation/)      | This folder contains scripts and files used to systematically list and create 900 configuration files for each scenario. Each configuration file specifies which scenario-specific inputs would be read-in.
| [`xml-batch/`](/model/xml-batch/)         | This folder provides batch input `.XML` files for water system that are required in each of the 900 simulation runs.
| [`config_files/`](/model/config_files/)      | This folder contains 900 configuration files for each simulation, containing simulation specific model settings and paths to files containing scenario-specific input conditions. 
| [`gcam-5/`](/model/gcam-5/) | This folder, currently empty, would contain the version of the GCAM model used for this study. The model folder ([`gcam-5.7z`](https://zenodo.org/record/6480465/files/gcam-5.7z?download=1)) could be downloaded from [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6480465.svg)](https://doi.org/10.5281/zenodo.6480465) and extracted here as `gcam-5/`.
| [`batch-scripts/`](/model/batch-scripts/)     | This folder contains shell scripts (`.sh`) used to run multiple GCAM scenario simulations on cluster in parallel.
| [`queries/`](/model/queries/)           | This folder contains structure for main queries used to extract the relevant [outputs](/model/outputs/) from 900 GCAM runs. The queries are provided in `.XML` format along with `.R` scripts that describe how the query would interact with 900 output databases. Shell `.sh` scripts are also provided to run these queries on cluster.
| [`outputs/`](/model/outputs/)           | This folder contains key outputs queried and collated from 900 GCAM runs. Some of the files of this folder are hosted in the data repository here [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6480465.svg)](https://doi.org/10.5281/zenodo.6480465). [outputs](/model/outputs/) folder contains [further information](/outputs/README.md) on each of the key output files. 


Completely reproducing the experiment would require a cluster for efficiency. Some of the paths specific to each cluster (e.g., `/pic/` in this case) must be changed  to the home directory of your cluster. 

In case of any errors or questions, feel free to get in touch at hassan.niazi@pnnl.gov for debugging details. 

## Reproduce the Analysis 
The files and scrpts in the [processing](/processing/) folder could be used to reproduce the analysis and the core versions of the figures presented in the [paper](paper). Please feel free to reach out at hassan.niazi@pnnl.gov in case of technical difficulties or if the analysis output is found to be different from those presented in the [paper](paper). 

Overview of the [processing](/processing/) folder:
| Script       | Description                |
| ------       | ----                       |
| `plot_basins.R` | This creates a map of all 235 basins represented in GCAM
| `groundwater_irr_production.R` | Function to calculate and process [irrigated agriculture](https://zenodo.org/record/6480465/files/global_irrigated_prod_by_crop.csv?download=1) output across all scenarios
| `function_geom_splitviolin.R` | Function to split the probability distribution (aka violins) of the main y-axis variable into half, resulting in two asymmetric distributions on each side of the median of x-axis variable
| `function_multiplot.R` | Function to plot panels across all ensemble members of the experiments. This is used in `splot density plots.R` to plot probability distributions in `Figure S18` and cumulative probability distributions in `Figure S19` in the SI
| `dataprep.R` | Script to clean and prepare the [groundwater withdrawals](/model/outputs/groundwater_production_FINAL.7z) model output from 900 runs for post-processing. [groundwater withdrawals](/model/outputs/groundwater_production_FINAL.7z) is the key file for analysis
| `allbasins.R` | This script is used to analyze peak and decline from GCAM simulations for *each basin* across all scenarios 
| `allscenarios.R` | This script is used to analyze peak and decline from GCAM simulations for *each scenario* across all basins (i.e. globally)
| `analysis_pnd.R` | Key analysis script generating key insights about groundwater peak and decline across all regions, basins, and scenarios. It analyzes raw [groundwater withdrawals](/model/outputs/groundwater_production_FINAL.7z) as well as peak groundwater withdrawals in each scenario. It also calculates peak water value and peak year along with total groundwater withdrawals by region, basin, and scenario
| `globalpnd_main.R` |This scripts plots `Figure 1` of the main text highlighting global peak and decline of groundwater withdrawals, along with marginal distributions, and validation data from [previous studies](/processing/inputs/historyv1.csv)
| `groundwater.R` | Analysis and visualization script that processes [model outputs](/model/outputs/), plots probability density of instantaneous and cumulative groundwater withdrawals across all ensemble groups (presented in `Figure S12`), and plots agricultural production in basins that are going to peak and decline
| `groundwater_pdf_gen.R` | Script for calculating agricultural production in 2100 and it's probability density form the basins that show peak and decline in groundwater withdrawals
| `gw depletion with double density peaks.R` | Script for plotting groundwater depletion trajectories with densities on top and side as presented in `Figure S9`, and for highlighting trajectories by categories on top of all trajectories as presented in `Figure S10`. 
| `interpolation.R` | Script to interpolate withdrawals between GCAM years i.e., to produce yearly withdrawals from withdrawals on 5-year timestep
| `map plot.R` | Key analysis script calculating which scenario peaked, it's probability, and related statistics which were used to produce `Figure 2` in the main text and all five map figures in the SI. This takes in the [shape files](/figures/shapefiles/) provided in the [figures](/figures/) folder to plot maps. 
| `depletion spark plots.R` | Script to calculate and plot key statistics for smaller basin-specific plots shown around the main `Figure 2` map in the main text. This scripts also plots trajectories of [groundwater withdrawals for each basin](/figures/allbasinspnd_v1_withHistory.png) with bands showing uncertainty all scenarios. 
| `peak bubble plot.R` | This script plots multiple variations of representing uncertainty of peak water and peak year across SSP and RCP ensemble groups, one of which is shown in `Figure S11` 
| `pop and ag histograms.R` | This script analyzes population and GDP in peak and decline basins. It is also used to calculate the fraction of population living in basins that are going to peak and decline as presented in `Figure S14` and irrigated production (% of global total) from basins that will face peak groundwater depletion in the 21st century as presented in `Figure S15`
| `splot density plots.R` | This script is used to plot probability densities and cumulative probability distributions of peak water and peak year across all ensemble groups as shown in `Figure S18` and `S19` respectively 
| `synthesisviolins.R` |  This script is used for plotting `Figure 3` in the manuscript which has uncertainty representation across SSPs and RCPs for peak water and peak year across all scenarios |
| `violins_synthesis.R` | This script is an earlier version of the `synthesisviolins.R` script which is used for plotting `Figure 3` in the manuscript |
| `wg gl checks.R` | This script analyzes initial results to check sensitivity of groundwater withdrawals to historical depletion trends (Gleeson vs WaterGap). It also plots 3 different kinds of sensitivity-type plots, one of which is shown in `Figure S11` |

### Data reference
Niazi, Hassan, Wild, Thomas, Turner, Sean, Graham, Neal, Hejazi, Mohamad, Msangi, Siwa, Kim, Son, Lamontagne, Jonathan, & Zhao, Mengqi. (2022). Large Ensemble Dataset for Discovering Global Peak Water Limit of Future Groundwater Withdrawals Using 900 GCAM Runs (v0_initial_release) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.6480465


In case of any questions, please reach out to Hassan Niazi via [email](mailto:hassan.niazi@pnnl.gov) or [issues](https://github.com/JGCRI/niazi-etal_202X_xyz/issues) section of this repository. 