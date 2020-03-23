#' Plot the deaths by age over time
#'
#' @param dat deaths (Dt) as a data.frame provided as an outcome from the det model functions
#' 
#' @param total plot all deaths combined when True, otherwise split by age
#' @return a plot of the deaths over time.
#'
#' @import ggplot2
#' @examples
#' params <- list(ini.prev = 0.001, n.age.cats = 10,  n.e.cats = 6, n.i.cats = 5, 
#'               e.move = 0.99, i.move = 0.99, beta = .35, theta = 1, 
#'               n0 = 10000, n.days = 360, d.no = 0.05, d.yes = 0.01, beds = 20,
#'               contacts = matrix(1, nrow = 10, ncol = 10),
#'               severity = c(0, 0, 0.001, 0.002, 0.004, 0.010, 0.020, 0.044, 
#'               0.093, 0.200))
#' 
#' out <- det_model(params)
#' plot_deaths_time(out$outcomes$Dt, total = T)
#' plot_deaths_time(out$outcomes$Dt, total = F)
#'  
#' @export

plot_deaths_time <- function(dat, total){
  if(missing(dat)==TRUE) warning("missing data to plot")
  if(missing(total)==TRUE) warning("missing total argument, assuming True")
  if(missing(total)==TRUE) total <- TRUE
  
  if(total == T){
    dat.sum <- dat %>% group_by(day) %>% summarize(n = sum(n))
    p <- ggplot(dat.sum, aes(day, n)) + ylab("Total deaths per day") 

  }
  
  if(total == F){
    p <- ggplot(dat, aes(day, n, color = as.factor(age))) +
      labs(color = "Age category")  +  ylab("Deaths per day by age") 

  }
  
  p <- p + geom_line(size = 1.5) + 
    xlab("Day") + 
    theme_light(base_size = 16) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  p
}
