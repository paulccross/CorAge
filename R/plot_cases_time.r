#' Plot the infectious cases by age over time
#'
#' @param dat data.frame of infectious individuals provided as output from the det model functions
#' @param total plot all cases combined when total = True, otherwise split by age
#' @return a plot of the population totals split by age.
#'
#' @import ggplot2
#' @importFrom dplyr summarize group_by
#' @importFrom magrittr %>%
#' @examples
#' params <- list(ini.prev = 0.001, n.age.cats = 10,  n.e.cats = 6, n.i.cats = 5, 
#'               e.move = 0.99, i.move = 0.99, beta = .35, theta = 1, 
#'               n0 = 10000, n.days = 360, d.no = 0.05, d.yes = 0.01, beds = 20,
#'               contacts = matrix(1, nrow = 10, ncol = 10),
#'               severity = c(0, 0, 0.001, 0.002, 0.004, 0.010, 0.020, 0.044, 
#'               0.093, 0.200))
#' 
#' out <- det_model(params)
#' plot_cases_time(out$counts$It, total = T)
#' plot_cases_time(out$counts$It, total = F)
#' 
#' @export

plot_cases_time <- function(dat, total){
  if(missing(dat)==TRUE) warning("missing data to plot")
  if(missing(total)==TRUE) warning("missing total argument, assuming True")
  if(missing(total)==TRUE) total <- TRUE
  
  if(total == T){
    dat.sum <- dat %>% group_by(day) %>% summarize(n = sum(n))
    p <- ggplot(dat.sum, aes(day, n)) + geom_line(size = 1.5) + 
    ylab("Total infectious cases")
      
  }
  
  if(total == F){
    dat.sum <- dat %>% group_by(age, day) %>% summarize(n = sum(n))
    p <- ggplot(dat.sum, aes(day, n, color = as.factor(age))) +
      geom_line(size = 1.5) + 
      labs(color = "Age category")+ 
      ylab("Infectious cases by age")
  }
  
  p <- p  + 
    xlab("Day") + 
    theme_light(base_size = 16) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  
  p
}
