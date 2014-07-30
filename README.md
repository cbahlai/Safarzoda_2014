#Safarzoda_2014
==============

##Analysis and data for Safarzoda 2014

Analysis examining impact of natural enemies on cereal aphid population growth in winter wheat fields in early spring. Study examined population dynamics of two aphid species, Bird Cherry Oat aphid (BCHO) and Engligh grain aphid (EG), using various natural enemy exclusion treatments.

First experiment used closed cagwes to exclude natural enemies, open cages to allow natural enemies full access to aphids, and sham cages to account for cage effects while allowing natural enemies to access aphids

Second experiment consisted of four treatments- Closed cages, to exclude all natural enemies, open plots, to allow access to all natural enemies, top cages, to limit access to aphids by foliar foraging predators, and bottom cages, to limit access to aphids by ground-dwelling predators.

Analysis consisted of ANOVA, using models with negative binomial error structures to account for distribution of insect-count data. Rao's statistic was used to determine which factors were statistically significant. Post-hoc analyses consisted  of pairwise t-tests that were Holm adjusted for multiple comparisons. Plots were constructed using ggplot2.

## Data (Tab-separated variable files): <br>
safarzoda_pred_exclusion.txt- aphid counts over time, expt 1<br>
safarzoda_pred_guilds.txt - aphid counts over time, expt 2<br>
safarzoda_predators.txt -natural enemy counts over time, expt 2<br>
