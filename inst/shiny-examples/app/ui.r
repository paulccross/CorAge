#' Shiny app UI function
#'
#' @import shiny

ui <- fluidPage(navbarPage("CorAge: Age-based coronavirus model",
                           tabPanel("Description",
                                    withMathJax(includeHTML("description_combo.html"))),
                           tabPanel("Deterministic model",
                                    det_modUI(id = "det")),
                          ),
                )
