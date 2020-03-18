
server <- function(input, output, session) {
  callModule(det_mod_server, id = "det")
  session$onSessionEnded(stopApp)
}
