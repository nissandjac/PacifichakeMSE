# PacifichakeMSE
Management strategy evaluation of Pacific hake

This code runs a management strategy evaluation (MSE) of Pacific hake. The code has three subfolders: 1) Spatial MSE and 2) Hake SS3 3) Hake SAM.

## Spatial MSE
The Spatial MSE folder runs the MSE with a spatial operating model and the estimation model used in the hake assessment using the file run_MSE_all.R
The file runs six hake management strategy evaluations, with 3 different harvest control rules, and 3 different movement rates. Run the file 'compare_MSE.R'to plot the results of the MSEs

## Hake SS3
The hake SS3 folder is the hake stock assessment. The official stock assessment has been rewritten in TMB. The assessment can be run from the file runHakeassessment.R

## Hake SAM
Hake SAM folder is a state-space version of the hake assessment. Under development.
### Disclaimer
All code by Nis Sand Jacobsen nissandjac@gmail.com . Please do not use in specific management scenarios without first consulting the author.  
