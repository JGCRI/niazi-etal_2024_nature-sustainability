# Analysis and post-processing scripts and files 

This folder contains scripts and files required to reproduce the analysis presented in the [paper](paper). 

### Folders description: 

- [inputs](./inputs/) contain ancillary files required for the analysis such as the basins names or review of past studies about historical groundwater depletion. 
    - This folder complements [model outputs](../model/outputs/) folder which contains key model outputs queried and collated from 900 GCAM runs. 

- [outputs](./outputs/) folder contains some of the key outputs containing key metrics produced after post-processing of the 900 GCAM runs. 

### Scripts description: 

`.R` scripts contain analysis workflows for preparing [model outputs](../model/outputs/) for post-processing, producing key metrics such as peak water and peak year statistics by region, basin and scenario, and visualizing the key metrics as reported in the [paper](paper). 

The sequence in which the folders and scripts are mentioned hereunder is the order in which they'd be used. 

| Script       | Description                |
| ------       | ----                       |
| `plot_basins.R` | This creates a map of all 235 basins represented in GCAM
| `groundwater_irr_production.R` | Function to calculate and process [irrigated agriculture](../model/outputs/global_irrigated_prod_by_crop.7z) output across all scenarios
| `function_geom_splitviolin.R` | Function to split the probability distribution (aka violins) of the main y-axis variable into half, resulting in two asymmetric distributions on each side of the median of x-axis variable
| `function_multiplot.R` | Function to plot panels across all ensemble members of the experiments. This is used in `splot density plots.R` to plot probability distributions in `Figure S18` and cumulative probability distributions in `Figure S19` in the SI
| `dataprep.R` | Script to clean and prepare the [groundwater withdrawals](../model/outputs/groundwater_production_FINAL.7z) model output from 900 runs for post-processing. [groundwater withdrawals](../model/outputs/groundwater_production_FINAL.7z) is the key file for analysis
| `allbasins.R` | This script is used to analyze peak and decline from GCAM simulations for *each basin* across all scenarios 
| `allscenarios.R` | This script is used to analyze peak and decline from GCAM simulations for *each scenario* across all basins (i.e. globally)
| `analysis_pnd.R` | Key analysis script generating key insights about groundwater peak and decline across all regions, basins, and scenarios. It analyzes raw [groundwater withdrawals](../model/outputs/groundwater_production_FINAL.7z) as well as peak groundwater withdrawals in each scenario. It also calculates peak water value and peak year along with total groundwater withdrawals by region, basin, and scenario
| `globalpnd_main.R` |This scripts plots `Figure 1` of the main text highlighting global peak and decline of groundwater withdrawals, along with marginal distributions, and validation data from [previous studies](./inputs/historyv1.csv)
| `groundwater.R` | Analysis and visualization script that processes [model outputs](../model/outputs/), plots probability density of instantaneous and cumulative groundwater withdrawals across all ensemble groups (presented in `Figure S12`), and plots agricultural production in basins that are going to peak and decline
| `groundwater_pdf_gen.R` | Script for calculating agricultural production in 2100 and it's probability density form the basins that show peak and decline in groundwater withdrawals
| `gw depletion with double density peaks.R` | Script for plotting groundwater depletion trajectories with densities on top and side as presented in `Figure S9`, and for highlighting trajectories by categories on top of all trajectories as presented in `Figure S10`. 
| `interpolation.R` | Script to interpolate withdrawals between GCAM years i.e., to produce yearly withdrawals from withdrawals on 5-year timestep
| `map plot.R` | Key analysis script calculating which scenario peaked, it's probability, and related statistics which were used to produce `Figure 2` in the main text and all five map figures in the SI. This takes in the [shape files](../figures/shapefiles/) provided in the [figures](../figures/) folder to plot maps. 
| `depletion spark plots.R` | Script to calculate and plot key statistics for smaller basin-specific plots shown around the main `Figure 2` map in the main text. This scripts also plots trajectories of [groundwater withdrawals for each basin](../figures/allbasinspnd_v1_withHistory.png) with bands showing uncertainty all scenarios. 
| `peak bubble plot.R` | This script plots multiple variations of representing uncertainty of peak water and peak year across SSP and RCP ensemble groups, one of which is shown in `Figure S11` 
| `pop and ag histograms.R` | This script analyzes population and GDP in peak and decline basins. It is also used to calculate the fraction of population living in basins that are going to peak and decline as presented in `Figure S14` and irrigated production (% of global total) from basins that will face peak groundwater depletion in the 21st century as presented in `Figure S15`
| `splot density plots.R` | This script is used to plot probability densities and cumulative probability distributions of peak water and peak year across all ensemble groups as shown in `Figure S18` and `S19` respectively 
| `synthesisviolins.R` |  This script is used for plotting `Figure 3` in the manuscript which has uncertainty representation across SSPs and RCPs for peak water and peak year across all scenarios |
| `violins_synthesis.R` | This script is an earlier version of the `synthesisviolins.R` script which is used for plotting `Figure 3` in the manuscript |
| `wg gl checks.R` | This script analyzes initial results to check sensitivity of groundwater withdrawals to historical depletion trends (Gleeson vs WaterGap). It also plots 3 different kinds of sensitivity-type plots, one of which is shown in `Figure S11` |