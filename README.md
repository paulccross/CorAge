CorAge
=====================================================================================
An R package to run age-based coronavirus models and a Shiny app

This is a repository with R code for COVID simulation models and interactive Shiny applications. Currently there is only one discrete time (daily timestep) deterministic model that is age structured with S, E, I, and R categories. Exposed and Infectious categories are sub-divided into a box-car approach to control to distribution of waiting times in those categories.  

This is mostly a copy-paste from another model, apologies if there are some typos, weird references, etc.

### TO DO ###
1. calculate appropriate parameter values
2. compute years of life lost and plot
3. connect to shiny app
4. fix R0 calculation
5. add in different interventions by age and time
6. stochastic version?