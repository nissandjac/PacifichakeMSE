# PacifichakeMSE
Management strategy evaluation of Pacific hake

This code runs a management strategy evaluation (MSE) of Pacific hake. The model can be run from R_scripts/run_MSE_XX.R where XX represent either of the four current MSE scenarios (climate, HCR, survey or selectivity). The repository also includes code to run the hake stock assessment using a reformulation in TMB.
PacifichakeMSE is now bundled as an R package - install with install.packages('~/PacifichakeMSE', repos = NULL, type="source")


## Operating model
The MSE runs with a spatial operating model and the estimation model used in the hake assessment using the file run_MSE_all.R
The file runs six hake management strategy evaluations, with 3 different harvest control rules, and 3 different movement rates. Run the file 'compare_MSE.R'to plot the results of the MSEs

## Hake stock assessment
Besides the MSE, the repository also contains the hake stock assessment rewritten in TMB. The assessment can be run from the file runHakeassessment.R

# Description
Detailed technical description is in progress.

### Disclaimer
All code by Nis Sand Jacobsen nissandjac@gmail.com . Please do not use in specific management scenarios without first consulting the author.  
