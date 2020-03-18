#' Plot the total counts over time
#'
#' @param dat list of counts provided as output from the det model functions
#' @return a plot of the population totals split by age.
#'
#' @import ggplot2
#' @importFrom dplyr mutate summarize
#' @importFrom magrittr %>%
#' @examples
#' n.age.cats <- 10
#' 
#' params <- list(ini.prev = 0.02, n.age.cats = n.age.cats,  n.e.cats = 10, 
#' n.i.cats = 10, e.move = 0.4, i.move = 0.4, beta = 0.08, theta = 1, 
#' n0 = 10000, n.days = 360, contacts = matrix(1, nrow = n.age.cats, ncol = n.age.cats),
#' severity = seq(0.01, 0.2, length.out = n.age.cats), d.no = 0.05, d.yes = 0.01,
#' beds = 100)
#' 
#' out <- det_model(params)
#' plot_totals_time(out$counts)
#' 
#' @export

plot_totals_time <- function(dat){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  S <- dat$St %>% group_by(day) %>% summarize(n = sum(n)) %>% 
    mutate(category = "S")
  E <- dat$Et %>% group_by(day) %>% summarize(n = sum(n)) %>% 
    mutate(category = "E")
  I <- dat$It %>% group_by(day) %>% summarize(n = sum(n)) %>% 
    mutate(category = "I")
  R <- dat$Rt %>% group_by(day) %>% summarize(n = sum(n)) %>% 
    mutate(category = "R")
  
  dat.sum <- rbind(S, E, I, R)
  
  #plot
  p <- ggplot(dat.sum, aes(day, n, color = category)) +
    geom_line(size = 1.5) +
    xlab("Day") + ylab("Population") + labs(color = "Disease status") +
    theme_light(base_size = 16) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  p
}
