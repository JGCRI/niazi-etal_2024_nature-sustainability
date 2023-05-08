_your zenodo badge here_

# niazi-etal_202X_xyz

**Global Peak Nonrenewable Water Limits over the 21st Century**

Hassan Niazi<sup>1\*</sup>, Thomas Wild<sup>1</sup>, Neal Graham<sup>1</sup>, Mohamad Hejazi<sup>2</sup>, Sean Turner<sup>3</sup>, Siwa Msangi<sup>1</sup>, Sonny Kim<sup>1</sup>, Jonathan Lamontagne<sup>4</sup>, Mengqi Zhao<sup>3</sup>, and Cathrine Yonkofski<sup>3</sup>

<sup>1 </sup> Joint Global Change Research Institute, Pacific Northwest National Laboratory (JGCRI-PNNL), College Park, MD, USA
<sup>2 </sup> King Abdullah Petroleum Studies and Research Center (KAPSARC), Riyadh, Saudi Arabia
<sup>3 </sup> Pacific Northwest National Laboratory (PNNL), Richland, WA, USA
<sup>4 </sup> Tufts University, Boston, MA, USA

\* corresponding author: hassan.niazi@pnnl.gov

## Abstract
Vast stores of groundwater accumulated below the Earth's surface during the Holocene Epoch (i.e., past 12,000 years). Over the past 50 years, these non-renewable stocks have been depleted at alarming rates, largely to fuel the irrigated green revolution of global agro-economic development. Given society's reliance on groundwater, we build on the concept of 'peak water limits' to explore whether, when, and where humanity might reach peak groundwater usage. Using an integrated model of the coupled human-earth system with global coverage, we simulate groundwater depletion across 235 water basins under 900 plausible scenarios covering a wide range of human and earth system dimensions of global change over the 21st century. Global nonrenewable groundwater extraction demonstrates a distinct peak-and-decline signature across nearly all (98\%) scenarios, peaking on average at 625 km^yr_1 in 2050, followed by a consistent decline through 2100. This global pattern is driven by about one-third of global water basins (33±9\%), which robustly exhibit this characteristic peak-and-decline signature across most scenarios. A peak-and-decline signature characterizes most depletable natural resources, such as oil, gold and copper, and suggests the need for a paradigm shift wherein groundwater is carefully valued and managed as a depletable, rather than renewable, resource. Eventually, groundwater pumping as a long-term strategy for mitigating water scarcity is likely to diminish toward the end of the century. And water basins which are now heavily dependent on groundwater resources for meeting their demands will likely face increasing costs of groundwater and food production.

## Journal reference
To be updated later

## Repository Overview 

| Type  | Category        | File/Folder                     | Contents                        |
|-------|-----------------|---------------------------------|---------------------------------|
| Data	| Input	          | GCAM v5.1	                    |                                 |
| 		|                 | Scenario xmls                   | 900 scenario input folders      |
| 	    | Output	      | Combined output file	        | groundwater withdrawals.csv     |
| 		| 	              |                                 | surfacewater withdrawals.csv    |
| 		|                 | Individual GCAM output          | 900 scenario output folders     |
| Code	| Preprocessing	  | Setting up runs (batch scripts) |                                 |
| 	    | 	              | Extracting outputs (queries)	|                                 |
| 	    | Postprocessing  | analysis.R, figures.R           |                                 |


## Contributing modeling software
| Model | Version | Repository Link | DOI |
|-------|---------|-----------------|-----|
| GCAM | v5.1 | Code: https://github.com/JGCRI/gcam-core/releases/tag/gcam-v5.1 | download: https://zenodo.org/record/1308172#.ZFhBX3bMKiw |

## Reproduce my experiment
Fill in detailed info here or link to other documentation that is a thorough walkthrough of how to use what is in this repository to reproduce your experiment.

Running GCAM has two parts: preparing the input data and preparing the formatted `.xml` inputs for GCAM. For this project specifically this entailed: 
1. Scenraios design, preparing water data, and preparing 900 input folders 
2. `xmls` for each folder as a direct input for a GCAM run -> to be provided 

### Reproduce simulations 

Preparing inputs: 900 scenario xmls 
Preprocessing: setting up 900 batch runs 
GCAM version access:  


1. Install the software components required to conduct the experiement from [Contributing modeling software](#contributing-modeling-software)
2. Download and install the supporting input data required to conduct the experiement from [Input data](#input-data)
3. Run the following scripts in the `workflow` directory to re-create this experiment:

| Script Name | Description | How to Run |
| --- | --- | --- |
| `step_one.R` | Script to run the first part of my experiment | `python3 step_one.py -f /path/to/inputdata/file_one.csv` |
| `step_two.R` | Script to run the last part of my experiment | `python3 step_two.py -o /path/to/my/outputdir` |

Should you run into errors while running GCAM, get in touch at hassan.niazi@pnnl.gov for debugging details. 

### Reproduce analysis 
Queries 
Post-processing: analysis.R (list inputs) 

4. Download and unzip the output data from my experiment [Output data](#output-data)
5. Run the following scripts in the `workflow` directory to compare my outputs to those from the publication

| Script Name | Description | How to Run |
| --- | --- | --- |
| `compare.R` | Script to compare my outputs to the original | `python3 compare.py --orig /path/to/original/data.csv --new /path/to/new/data.csv` |

## Reproduce my figures
Use the scripts found in the `figures` directory to reproduce the core versions of the figures used in this publication.

| Script Name | Description | How to Run |
| --- | --- | --- |
| `generate_figures.R` | Script to generate my figures | `python3 generate_figures.py -i /path/to/inputs -o /path/to/outuptdir` |

## Code reference


## Data reference

### Input data


### Output data
