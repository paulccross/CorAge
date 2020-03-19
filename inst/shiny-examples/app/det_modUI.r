#' Shiny app UI function
#'
#' UI function to define the sliders and output plots for the single
#' deterministic model page of the application
#'
#' @param input provided by shiny
#'
#' @import shiny
#'
#' @export

det_modUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4, h4("Parameters"),
            tabsetPanel(
              tabPanel("Disease",
                       sliderInput(ns("beta"), "transmission coefficient",
                                   value = 0.34, min = 0, max = 0.7, step = 0.01),
                       sliderInput(ns("n.e.cats"), "Exposed subcategories",
                                   value = 6, min = 3, max = 10, step = 1),
                       sliderInput(ns("n.i.cats"), "Infectious subcategories",
                                   value = 4, min = 3, max = 10, step = 1),
                       sliderInput(ns("e.move"), "Exposed progression",
                                   value = .99, min = .1, max = .99, step = .01),
                       sliderInput(ns("i.move"), "Infectious progression",
                                   value = .8, min = .1, max = .99, step = .01),
                       sliderInput(ns("theta"), "FD = 1 vs DD = 0 transmission",
                                   value = 1, min = 0, max = 1, step = 0.1),
                       includeText("disease_text.txt")
                      ),
              tabPanel("Severity",
                      sliderInput(ns("s.lo"), "% severe cases in sub-adults",
                          value = 0.01, min = 0.0001, max = .2, step = 0.01),
                      sliderInput(ns("s.hi"), "% severe cases in the elderly",
                                  value = .2, min = 0.05, max = 0.8, step = 0.01),
                      sliderInput(ns("d.no"), "Daily mortality of non-hospitalized severe cases",
                                  value = 0.05, min = 0.01, max = .2, step = 0.01),
                      sliderInput(ns("d.yes"), "Daily mortality of hospitalized severe cases",
                                  value = 0.01, min = 0, max = .1, step = 0.01),
                      sliderInput(ns("beds"),"Number of beds for severe cases",
                                  value = 30, min = 10, max = 1000, step = 10),
                      includeText("severity_text.txt")
              ),
              tabPanel("Simulation",
                       sliderInput(ns("n0"),"Initial population size",
                                   value = 100000, min = 1000, max = 500000,step = 1000),
                       sliderInput(ns("n.days"),"# of days",
                                   value = 360, min = 30, max = 1000, step = 10),
                       sliderInput(ns("ini.prev"), "Initial prevalence",
                                   value = 0.001, min = 0.001, max = 0.05, step = 0.001),
                       sliderInput(ns("n.age.cats"), "Number of age categories",
                                   value = 10, min = 7, max = 10, step = 1),
                       includeText("simulation_text.txt")
                       
                   )
            )),
     column(width = 8, h4("Plots"),
            tabsetPanel(
              tabPanel("Totals", plotOutput(ns('TotalPlot'))),

              tabPanel("Infectious Cases", plotOutput(ns('CasesPlot'))),
              
              tabPanel("Needed hospitalizations", plotOutput(ns('HospitalPlot'))),
              
              tabPanel("Deaths", plotOutput(ns('DeathsPlot'))),
              
              tabPanel("Emergent Parameters", plotOutput(ns('ParamPlot'))),
              
              tabPanel("Age Distribution", plotOutput(ns('AgePlot')))
              
              ),
            includeMarkdown("disclaimer.md")
          )
      )
)}

