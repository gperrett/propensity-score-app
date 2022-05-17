# propensity-score-app

## Overview
This project is aimed at showing the unreliability of propensity score matching estimates. Propensity score matching is a causal inference methodology, used with observational studies, to create the effect of a randomized study. That is, propensity score matching aims to create a control group that looks like the treatment group on average, given that actual randomization is not possible. The main challenge of propensity score matching has to do with researcher degrees of freedom. Propensity score model specifications are subjective, and this subjectivity influences the results of the estimated treatment effect. With propensity score matching, different model specifications can yield drastically different estimates of the Average Treatment Effect on the Treated (ATT). This app aims to show concrete evidence on the unreliability of propensity score estimates by visualizing the variability in ATT estimates from different model specifications, particularly given the true ATT is known.

The target user for this app is researchers who may not have a strong statistical background, but are familiar with propensity scores and the implementation. 
