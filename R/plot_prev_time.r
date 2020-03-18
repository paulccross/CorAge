#' Plot the prevalence over time
#'
#' @param dat list of counts provided as output from the det model functions
#' @return a plot of the prevalence over time
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
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
#' plot_prev_time(out$counts)
#' @export

plot_prev_time <- function(dat){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  # summarize by day and disease status, calculate the prevalence
  S <- dat$St %>% group_by(day) %>% summarize(n = sum(n)) %>% 
    mutate(category = "S")
  E <- dat$Et %>% group_by(day) %>% summarize(n = sum(n)) %>% 
    mutate(category = "E")
  I <- dat$It %>% group_by(day) %>% summarize(n = sum(n)) %>% 
    mutate(category = "I")
  R <- dat$Rt %>% group_by(day) %>% summarize(n = sum(n)) %>% 
    mutate(category = "R")
  
  dat.sum <- rbind(S, E, I, R)
 
  dat.sum <- dat.sum %>%
    spread(key = category, value = n) %>%
    mutate(prev = (I)/ (S + E + I + R)) 

  ggplot(dat.sum, aes(x = day, y = prev)) +
    geom_line(size = 1.5) +
    ylab("Prevalence") + xlab("Day") +
    theme_light()  + theme(text = element_text(size = 16),
                           panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank())

}
