#' Plot the needed hospitalizations by age over time
#'
#' @param dat data.frame of needed hospitalizations. Provided as output from the det model functions
#' @param total plot all hospitalizations combined when total = True, otherwise split by age
#' @return a plot of the hospitalizations split by age or combined
#'
#' @import ggplot2
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
    p <- ggplot(dat.sum, aes(day, n))
  }
  
  if(total == F){
    p <- ggplot(dat, aes(day, n, color = as.factor(age))) +
      labs(color = "Age category")
  }
  
  p <- p + geom_line(size = 1.5) + 
    xlab("Day") + 
    ylab("Hospitalizations needed") +
    theme_light(base_size = 16) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  
  p
}
