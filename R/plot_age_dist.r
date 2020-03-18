#' Plot the age distribution
#'
#'
#' @param dat dataframe of counts provided as output from the COVID model 
#' functions
#'
#' @return a plot the age distribution at the end point. Note that the final 
#' age category includes all individuals of that age or more 
#'
#' @import ggplot2
#' @import dplyr
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
#' plot_age_dist(out$counts)
#' 
#' @export

plot_age_dist <- function(dat){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  S.end <- dat$St %>% filter(day == max(day)) %>% select(age, n) %>% mutate(category = "S")
  E.end <- dat$Et %>% filter(day == max(day)) %>% group_by(age) %>%
    summarize(n = sum(n))  %>% mutate(category = "E")
  I.end <- dat$It %>% filter(day == max(day)) %>% group_by(age) %>%
    summarize(n = sum(n))  %>% mutate(category = "I")
  R.end <- dat$Rt %>% filter(day == max(day)) %>% select(age, n)%>% mutate(category = "R")
  
  N.end <- rbind(S.end, E.end, I.end, R.end)
  #create the plot
  p <- ggplot(N.end, aes(x = age, y = n, color = category)) +
    geom_line(size = 1.5) +
    ylab("Population") + xlab("Age category") +
    theme_light()  + theme(text = element_text(size = 18),
                           panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank(), legend.position = c(.15,.8))

  p
}

