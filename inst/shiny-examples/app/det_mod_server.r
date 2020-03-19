#' Shiny app server function
#'
#' Server function to run the deterministic model
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

det_mod_server <- function(input, output, session){

  #Create the reactive parameter set and run the model:
  react.params <- reactive({
    list(
         ini.prev = input$ini.prev,
         n.age.cats = 10,
         n.i.cats = input$n.i.cats,
         n.e.cats = input$n.e.cats,
         e.move = input$e.move,
         i.move = input$i.move, 
         beta = input$beta,
         theta = 1,
         
         k.k.c = input$k.k.c,
         a.a.c = input$a.a.c,
         k.a.c = input$k.a.c,
         eld.c = input$eld.c,
         red.c = input$red.c,
         
         #future change here
         s.lo = input$s.lo,
         s.hi = input$s.hi,
         
         n0 = input$n0,
         n.days = input$n.days,
         d.no = input$d.no,
         d.yes = input$d.yes,
         beds = input$beds
         )
  })

  simout <- reactive({
    params <- react.params()
    
    #Calculate some of the derived parameters
    
    # calculate severity vector as an exponential model from lo to hi
    # based on x_t = x_0(1+r)^t
    r <- (params$s.hi / params$s.lo)^(1/params$n.age.cats) - 1
    params$severity <- params$s.lo * (1+r) ^ seq(1,params$n.age.cats, 1)
    
    #browser()
    #contact matrix
    contacts <- matrix(1, nrow = 10, ncol = 10)
    diagonal <- c(params$k.k.c, params$k.k.c, params$a.a.c, params$a.a.c, params$a.a.c, 
                  params$a.a.c, params$a.a.c, params$eld.c, params$eld.c, params$eld.c)
    diag(contacts) <- diagonal
    contacts[8:10, ] <- params$eld.c
    contacts[ ,8:10] <- params$eld.c
    contacts[1, 3:4] <- params$k.a.c
    contacts[3:4, 1] <- params$k.a.c
    contacts[3:7, 3:7] <- params$a.a.c
    
    params$contacts <- contacts * (1-params$red.c)
    
    #run model
    out <- det_model(params)
    out
  })

  output$TotalPlot <- renderPlot({
    out <- simout()
    p1 <- plot_totals_time(out$counts)
    p2 <- plot_prev_time(out$counts)
    plot_grid(p1, p2, nrow = 1)
  })

  output$CasesPlot <- renderPlot({
    out <- simout()
    p1 <- plot_cases_time(out$counts$It, total = T)
    p2 <- plot_cases_time(out$counts$It, total = F)
    plot_grid(p1, p2, nrow = 1)
  })
  
  output$HospitalPlot <- renderPlot({
    out <- simout()
    p1 <- plot_hos_time(out$outcomes$Ht, total = T)
    p2 <- plot_hos_time(out$outcomes$Ht, total = F)
    plot_grid(p1, p2, nrow = 1)
  })
  
  output$DeathsPlot <- renderPlot({
    out <- simout()
    p1 <- plot_deaths_time(out$outcomes$Dt, total = T)
    p2 <- plot_deaths_time(out$outcomes$Dt, total = F)
    plot_grid(p1, p2, nrow = 1)
  })
  
  output$ParamPlot <- renderPlot({
    params <- react.params()
    p1 <- plot_periods(e.move = params$e.move, i.move = params$i.move, 
                 n.e.cats = params$n.e.cats, n.i.cats = params$n.i.cats)
    p2 <- plot_severity(s.lo = params$s.lo, s.hi = params$s.hi, 
                        n.age.cats = params$n.age.cats)
    plot_grid(p1, p2, nrow = 1)
    
  })
  
  output$AgePlot <- renderPlot({
    out <- simout()
    plot_age_dist(out$counts)
  })

  output$ContactPlot <- renderPlot({
    params <- react.params()

    #contact matrix
    contacts <- matrix(1, nrow = 10, ncol = 10)
    diagonal <- c(params$k.k.c, params$k.k.c, params$a.a.c, params$a.a.c, params$a.a.c, 
                  params$a.a.c, params$a.a.c, params$eld.c, params$eld.c, params$eld.c)
    diag(contacts) <- diagonal
    contacts[8:10, ] <- params$eld.c
    contacts[ ,8:10] <- params$eld.c
    contacts[1, 3:4] <- params$k.a.c
    contacts[3:4, 1] <- params$k.a.c
    contacts[3:7, 3:7] <- params$a.a.c
    params$contacts <- contacts * ( 1- params$red.c)# percent reduction overall

    p1 <- plot_contacts(params$contacts)
    
    out <- simout()
    p2 <- plot_exposed_age_end(out$counts)
    plot_grid(p1, p2, nrow = 2)
    })
}
