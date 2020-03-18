#' Plot the deaths by age over time
#'
#' @param dat deaths (Dt) as a data.frame provided as an outcome from the det model functions
#' 
#' @param total plot all deaths combined when True, otherwise split by age
#' @return a plot of the deaths over time.
#'
#' @import ggplot2
#' @examples
#' n.age.cats <- 10
#' 
#' params <- list(ini.prev = 0.02, n.age.cats = n.age.cats,  n.e.cats = 10, 
#' n.i.cats = 10, e.move = 0.4, i.move = 0.4, beta = 0.08, n0 = 10000, 
#' n.days = 360, contacts = matrix(1, nrow = n.age.cats, ncol = n.age.cats),
#' severity = seq(0.01, 0.2, length.out = n.age.cats), d.no = 0.1, d.yes = 0.02,
#' beds = 20)
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
    p <- ggplot(dat.sum, aes(day, n))
  }
  
  if(total == F){
    p <- ggplot(dat, aes(day, n, color = as.factor(age))) +
      labs(color = "Age category")
  }
  
  p <- p + geom_line(size = 1.5) + 
    xlab("Day") + 
    ylab("Deaths per day") +
    theme_light(base_size = 16) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  p
}
