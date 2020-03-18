CorAge
=====================================================================================
An R package to run age-based coronavirus models and a Shiny app

This is a repository with R code for a COVID simulation model and interactive Shiny application. Currently there is only one discrete time (daily timestep) deterministic model that is age structured with S, E, I, and R categories. Exposed and Infectious categories are sub-divided into a box-car approach to control to distribution of waiting times in those categories. The model tracks number of deaths and hospitalizations by age. 

Currently the user controls: intial prevalence and number of age categories. The number of subcategories for I and E, as well as the movement rate among them, which controls the duration of time an individual is exposed or infectious. The user also controls the transmission coefficient, theta = 0 for density or 1 for frequency dependence, a symmetrical matrix of relative contact rates among age classes, initial population size, severity of disease by age class, the number of beds or ICU units, and the proportion of severe cases that die with and without hospital care. 

This is mostly a copy-paste from a chronic wasting disease model, so apologies if there are some typos, weird references to deer, bucks, fawns, etc.

### TO DO ###
1. calculate appropriate parameter values  
2. compute years of life lost and plot  
3. fix R0 calculation  
4. calculate the doubling time so that you can dial in beta  
5. add in different interventions by age and time  
6. stochastic version?  

### Installation ###

To use this package, you should be using R 3.4+. You can install the package from github using the `remotes` package. Haven't yet built any vignettes. Working on shiny app first

First install remotes

```r
install.packages("remotes")
```
Then 
```r
remotes::install_github("https://github.com/paulccross/CorAge.git")
```

You should then be able to load it with
```
library(CorAge)
```

### Disclaimer ###

This is very preliminary. The default parameters are not formally estimated in any way, but just entered in a reasonable range to see if the functions are working. I'm doing this on non-work time, so you can reach me here: paulchafeecross@gmail.com


