# Overview

This repository hosts all materials that are used in the replication exercise for the [PPOL 6801: Text as Data](https://tiagoventura.github.io/PPOL_6801_2024/) course assignment. For the assignment, I chose to replicate Ban et al. (2019)'s 
article, "How Newspapers Reveal Political Power". This article shows how a simple method of counting term frequencies from newspaper data can actually measure how politically powerful a political actor is, and 
how this measure can be used to quantify political phenomenons.

The replication exercise aims to reproduce the figures and tables in the main section of the paper. Replication of the appendix figures/tables lies beyond the scope of this exercise.

The original article can be accessed [here](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/683B2D970E88A25239276D6FA071632B/S2049847017000437a.pdf/how_newspapers_reveal_political_power.pdf).
My replication results are accessible [here](https://rpubs.com/jswsean/ppol6081_replication_01), and the replication report can be seen [here](https://github.com/jswsean/PPOL6081_replication_1/blob/main/3_docs/replication_report/replication_report.pdf).

The following sections in this README provide information on the overall structure of this repository, as well as the original source for the replication materials.

# Structure

This repository has two main folders:

* `0_scripts`: this folder contains both the raw and the replication scripts. The raw scripts (do-file and Rscripts) are derived from the article's original replication folder. I only made a minor adjustment to the folder directories in the do-file
to make the flow consistent with this repository's folder structure. The folder of primary interest in this repository is the replication folder, which contains the Rscripts to build the required data and
replicate the paper's tables and figures. The build and replication Rscripts are separated for each individual tables/figures so they can run independently, despite being named in a chronological order:
  - `1.1.Build.Fig1_data.R`: Builds the data for Figure 1 tabulation
  - `1.2.Build.Fig2_data.R`: Builds the data for Figure 2 tabulation
  - `1.3.Build.Fig3_data.R`: Builds the data for Figure 3 tabulation and Table 1 regression
  - `1.4.Build.Fig4_data.R`: Builds the data for Figure 4 tabulation
  - `1.5.Build.Fig5_data.R`: Builds the data for Figure 5 tabulation
  - `2.1.Replicate.Fig1_data.R`: Replicates the paper's Figure 1
  - `2.2.Replicate.Fig2_data.R`: Replicates the paper's Figure 2
  - `2.3.Replicate.Fig3_data.R`: Replicates the paper's Figure 3
  - `2.4.Replicate.Tab1_data.R`: Replicates the paper's Table 1 
  - `2.5.Replicate.Fig4_data.R`: Replicates the paper's Figure 4
  - `2.6.Replicate.Fig5_data.R`: Replicates the paper's Figure 5
* `3_docs`: this folder contains the outputs of the replication exercise, both the figures (in the `fig` subfolder) and tables (in the `tab` subfolder). Additionally, this folder also contains the replication presentation and report.

# Data

All materials for the replication, including the raw data, are sourced from this [Harvard Dataverse repository](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ZH5YFY). While the workflow of the scripts is that they import data 
from the `1_raw` and export data to the `2_build` folder, I do not track them in my repository. Readers who wish to follow along can create these two folders in their own local machine. 

# References

Ban, P., Fouirnaies, A., Hall, A. B., & Snyder, J. M. (2019). How newspapers reveal political power. _Political Science Research and Methods_, 7(4), 661-678.

