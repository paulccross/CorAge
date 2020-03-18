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
                       sliderInput(ns("an.env.foi"), "Indirect transmission",
                                   value = 0, min = 0, max = 0.1, step = 0.01),
                       sliderInput(ns("r0_peryear"), "Direct R0 per year",
                                   value = .8, min = .1, max = 1.5, step = .05),
                       sliderInput(ns("gamma.m"), "Relative male infection",
                                   value = 2, min = 0.8, max = 5, step = 0.1),
                       sliderInput(ns("theta"), "Theta",
                                   value = 1, min = 0, max = 1, step = 0.1),
                       sliderInput(ns("p"), "index of disease mortality",
                                   value = 0.43, min = 0, max = .9, step = 0.01),
                       sliderInput(ns("rel.risk"),"Relative hunting risk",
                                   value = 1, min = 0.1, max = 3, step = 0.1),
                       includeMarkdown("disease_text.md")),
              tabPanel("Simulation",
                       sliderInput(ns("n0"),"Initial population size",
                                   value = 100000, min = 1000, max = 500000,step = 1000),
                       sliderInput(ns("n.days"),"# of days",
                                   value = 360, min = 30, max = 1000, step = 10),
                       sliderInput(ns("ini.prev"), "Initial prevalence",
                                   value = 0.01, min = 0.01, max = 0.4, step = 0.01),
                       sliderInput(ns("n.age.cats"), "Number of age categories",
                                   value = 9, min = 6, max = 10, step = 1),
                       sliderInput(ns("n.e.cats"), "Exposed subcategories",
                                   value = 5, min = 3, max = 10, step = 1),
                       sliderInput(ns("n.i.cats"), "Infectious subcategories",
                                   value = 5, min = 3, max = 10, step = 1),
                       sliderInput(ns("e.move"), "Exposed progression",
                                   value = .95, min = .1, max = .99, step = .01),
                       sliderInput(ns("i.move"), "Infectious progression",
                                   value = .9, min = .1, max = .99, step = .01)
                    )
            )),
     column(width = 8, h4("Plots"),
            tabsetPanel(
              tabPanel("Totals", plotOutput(ns('TotalPlot'))),

              tabPanel("Prevalence", plotOutput(ns('PrevPlot'))),

              tabPanel("Deaths", plotOutput(ns('DeathPlot'))),

              tabPanel("Age Distribution", plotOutput(ns('AgePlot'))),

              tabPanel("Parameters", plotOutput(ns('ParamPlot')))
            ),
            includeMarkdown("disclaimer.md")
          )
      )
)}
