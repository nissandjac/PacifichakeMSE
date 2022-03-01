# PacifichakeMSE
Management strategy evaluation of Pacific hake from the paper Jacobsen et al 2022.

This code runs a management strategy evaluation (MSE) of Pacific hake. The model can be run from /R_scripts/. The repository also includes code to run the hake stock assessment using a reformulation in TMB. PacifichakeMSE is now bundled as an R package - install with install.packages('~/PacifichakeMSE', repos = NULL, type="source")

## Operating model
The MSE runs with a spatial operating model and the estimation model used in the hake assessment using the file run_MSE_climate.R
The file runs six hake management strategy evaluations, with 3 different harvest control rules, and 3 different movement rates. Run the file 'compare_MSE.R'to plot the results of the MSEs
The run times are very long, and may crash R.

## Hake stock assessment
Besides the MSE, the repository also contains the hake stock assessment rewritten in TMB. The assessment can be run from the file runHakeassessment.R

# Description
See Jacobsen et al 2022 and supplementary for a detailed description of the model.
### Disclaimer
All code by Nis Sand Jacobsen nissandjac@gmail.com . Please do not use in specific management scenarios without first consulting the author.  
### Reference
Nis S Jacobsen, Kristin N Marshall, Aaron M Berger, Chris Grandin, Ian G Taylor, Climate-mediated stock redistribution causes increased risk and challenges for fisheries management, ICES Journal of Marine Science, 2022; https://doi.org/10.1093/icesjms/fsac029
