#' Plot the needed hospitalizations by age over time
#'
#' @param dat data.frame of needed hospitalizations. Provided as output from the det model functions
#' @param total plot all hospitalizations combined when total = True, otherwise split by age
#' @return a plot of the hospitalizations split by age or combined
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
#' plot_hos_time(out$outcomes$Ht, total = T)
#' plot_hos_time(out$outcomes$Ht, total = F)
#' 
#' @export

plot_hos_time <- function(dat, total){
  if(missing(dat)==TRUE) warning("missing data to plot")
  if(missing(total)==TRUE) warning("missing total argument, assuming True")
  if(missing(total)==TRUE) total <- TRUE
  
  if(total == T){
    dat.sum <- dat %>% group_by(day) %>% summarize(n = sum(n))
    p <- ggplot(dat.sum, aes(day, n))+ ylab("Total hospitalizations needed")
  }
  
  if(total == F){
    p <- ggplot(dat, aes(day, n, color = as.factor(age))) +
      labs(color = "Age category") + ylab("Hospitalizations needed by age")
  }
  
  p <- p + geom_line(size = 1.5) + 
    xlab("Day") + 
    theme_light(base_size = 16) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  
  p
}
