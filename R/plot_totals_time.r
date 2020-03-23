#' Plot the total counts over time
#'
#' @param dat list of counts provided as output from the det model functions
#' @return a plot of the population totals split by age.
#'
#' @import ggplot2
#' @importFrom dplyr mutate summarize
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
