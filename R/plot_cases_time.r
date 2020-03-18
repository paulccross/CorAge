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
#' n.age.cats <- 10
#' 
#' params <- list(ini.prev = 0.02, n.age.cats = n.age.cats,  n.e.cats = 10, 
#' n.i.cats = 10, e.move = 0.4, i.move = 0.4, beta = 0.08, theta = 1, n0 = 10000, 
#' n.days = 360, contacts = matrix(1, nrow = n.age.cats, ncol = n.age.cats),
#' severity = seq(0.01, 0.2, length.out = n.age.cats), d.no = 0.1, d.yes = 0.02,
#' beds = 20)
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
