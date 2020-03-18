# make sure everything is available to the app.
library(shiny)
library(reshape2)
library(popbio)
library(magrittr)
library(cowplot)
library(ggridges)
library(knitr)
library(shinydashboard)
library(dplyr)
library(forcats)
library(ggplot2)
library(markdown)
library(tidyr)
library(stringr)

source("det_mod_server.r", local = T)
source("det_modUI.r", local = T)

source("server.r", local = T)
source("ui.r", local = T)
