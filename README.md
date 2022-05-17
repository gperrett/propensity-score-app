# propensity-score-app

## Overview
This project is aimed at showing the unreliability of propensity score matching in average treatment effect estimation. Propensity score matching is a causal inference methodology, used with observational studies, to create the setup of a randomized study. That is, propensity score matching aims to create a control group that looks like the treatment group on average, given that actual randomization is not possible. With this setup, propensity score matching then compares units from the control group with similar units in the treatment group, to yield an average treatment effect estimate. 

The main challenge of propensity score matching has to do with researcher degrees of freedom. Propensity score model specifications are subjective, and this subjectivity influences the results of the estimated treatment effect. With propensity score matching, different model specifications can yield drastically different estimates of the Average Treatment Effect on the Treated (ATT). This app aims to show concrete evidence on the unreliability of propensity score estimates by visualizing the variability in ATT estimates from different model specifications. In this application, the outcome variable was simulated such that the true ATT is known to be 0. Thus, this app shows the variability in ATT estimates from propensity score matching, and how much these estimates can vary from the truth. In other words, this app shows how wrong propensity score estimates can be. 

One set of target users for this app are researchers who may not have strong statistical backgrounds, but are familiar with propensity scores and their implementation (and perhaps use them in their research). Another set of target users is graduate-level students in a causal inference class (and who are thus familiar with randomized experiments, observational studies, treatment effects, etc.). This project aims to be a call to action for those who look to use propensity score matching techniques, to dissuade them from continuing to use them to draw causal conclusions. 

## User Research and Testing

