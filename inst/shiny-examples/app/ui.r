#' Shiny app UI function
#'
#' @import shiny

ui <- fluidPage(navbarPage(title = "",
                           tabPanel("Description",
                                    withMathJax(includeHTML("description.html"))),
                           tabPanel("Deterministic model",
                                    det_modUI(id = "det"))
                          )
                )


