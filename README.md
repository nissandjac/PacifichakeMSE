# PacifichakeMSE
Management strategy evaluation of Pacific hake

This code runs a management strategy evaluation (MSE) of Pacific hake. The model can be run from R_scripts/run_MSE_all.R or  the hake assessment (rewritten in TMB) alone can be run from runHakeassessment.R and be compared with the SS3 assessment.

## Operating model
The MSE runs with a spatial operating model and the estimation model used in the hake assessment using the file run_MSE_all.R
The file runs six hake management strategy evaluations, with 3 different harvest control rules, and 3 different movement rates. Run the file 'compare_MSE.R'to plot the results of the MSEs

## Hake stock assessment
Besides the MSE, the repository also contains the hake stock assessment rewritten in TMB. The assessment can be run from the file runHakeassessment.R

### Disclaimer
All code by Nis Sand Jacobsen nissandjac@gmail.com . Please do not use in specific management scenarios without first consulting the author.  
