# The-Effect-of-Data-Aggregation-Level-in-Social-Network-Monitoring

This is the readme file for an example case of the codes corresponding to the simulations and visulizations presented to the paper titled:

The Effect of Data Aggregation Level in Social Network Monitoring

The authors of the paper are: 
Meng J. Zhao1, Anne R. Driscoll1, Srijan Sengupta, Nathaniel T. Stevens, Ronald D. Fricker Jr. and William H. Woodall


Corresponding author: Meng J. Zhao
E-mail: xiaodongmen@gmail.com


This paper is submitted for consideration for publication on PLoS ONE.
This version of the paper is a revision completed on 12-August-2018. This revision is intended as a response to the review decision by the PLoS ONE journal reviewers received by the authors on 18-July-2018

All simulation codes are written in the open-source statistical language R (version 3.5.1). Specific packages used for the simulations are specified in the code itself. 
This repository/project on Github contains the following files corresponding to Figure 1 shown in the revised paper.
The parameter settings specified by the files contained in this repository/project can be readily changed/updated to generate the simulation results shown in Figures 2 to 5. 

#### All codes are devided into sections with appropriate description for the purpose of the codes in that section


#### The following 4 files contain simulations that is used to generate the output shown in Figure 1. For the simulation results where network data are not aggregated, i.e. W=1. The codes are contained within the the simulation codes for W=2 simulation.
###### V=20_W=2_2communities_50%increase.R
###### V=20_W=5_2communities_50%increase.R
###### V=20_W=10_2communities_50%increase.R
###### V=20_W=20_2communities_50%increase.R

#### The following 1 file contains all the custom functions that is used to generate/facilitate the simulations in the 4 files above. The functions contain in this file are parameter setting egnostic and works regardless of the aggregation level or other parameter values. Each function within this file is referred by the simulations.
###### Simulation Functions.R
