# propensity-score-app

## Overview
This project is aimed at showing the unreliability of propensity score matching in average treatment effect estimation. Propensity score matching is a causal inference methodology, used with observational studies, to create the setup of a randomized study. That is, propensity score matching aims to create a control group that looks like the treatment group on average, given that actual randomization is not possible. With this setup, propensity score matching then compares units from the control group with similar units in the treatment group, to yield an average treatment effect estimate. 

The main challenge of propensity score matching has to do with researcher degrees of freedom. Propensity score model specifications are subjective, and this subjectivity influences the results of the estimated treatment effect. With propensity score matching, different model specifications can yield drastically different estimates of the Average Treatment Effect on the Treated (ATT). This app aims to show concrete evidence on the unreliability of propensity score estimates by visualizing the variability in ATT estimates from different model specifications. In this application, the outcome variable was simulated such that the true ATT is known to be 0. Thus, this app shows the variability in ATT estimates from propensity score matching, and how much these estimates can vary from the truth. In other words, this app shows how wrong propensity score estimates can be. 

One set of target users for this app are researchers who may not have strong statistical backgrounds, but are familiar with propensity scores and their implementation (and perhaps use them in their research). Another set of target users is graduate-level students in a causal inference class (and who are thus familiar with randomized experiments, observational studies, treatment effects, etc.). This project aims to be a call to action for those who look to use propensity score matching techniques, to dissuade them from continuing to use them to draw causal conclusions. 

## User Research and Testing
To best understand our users, we would target a graduate-level causal inference class and gauge sentiments about propensity score matching. From our own experience, we have heard that students choose to take a causal inference class just to learn about propensity scores, so we are operating under the assumption that most of these students are not aware of the severe pitfalls of propensity score matching techniques. Our main stakeholder, George, is particularly aware of this and would like to use this app as a PoC in his causal inference class. Therefore, the main problem we aim to solve with this application is to create a concrete and tangible way that people can see the variation in using propensity scores. 

We have learned that most causal inference classes teach that the main way to check if propensity scores are 'reasonable enough' to be used in causal analysis is by plotting the balance and overlap between treatment groups that result from the propensity score creation and matching. Thus, we choose to include balance and overlap plots to make it abundantly clear that even if balance and overlap checks show that the models are reasonable, the propensity score models are still super divergent.  

Moreover, when discussing this application with people within our Interactive Data Science class, people questioned whether the average of all treatment effect estimates from various model specifications would result in the true ATT estimate. To address this, we chose to include a graph and a table that saves all of the model specifications and resulting ATT estimates from a user, to show that fitting a bunch of propensity score models and averaging them will still lead to the wrong answer. 

While we have not had the opportunity for full-scale user testing, if we were to user test, we would provide the causal inference class with the prototype of the app, without telling them the purpose of it. We would gauge to see if and how the students understand the app by letting them interact with it for a period of time, and then askign them why they should be hesitant abotu using propensity scores. If the students respond saying propensity scores are great, then clearly our app would not be getting the main point across. We want the students to use the app to understand and identify that propensity scores can create unreliable results. We would want them to make the connection with p-hacking, in that this method can create such a wide variation of results that one could pick and choose their answer for the estimated ATT. 

## Prototyping Round 1 
