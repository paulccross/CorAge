
#' Plot the exposure by age at the end of the simulation
#'
#' @param dat counts provided as output from the COVID model function
#'
#' @return a plot of the prevalence by age
#'
#' @import ggplot2
#' @import dplyr
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
#' plot_exposed_age_end(out$counts)
#' 
#' @export

plot_exposed_age_end <- function(dat){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  S.end <- dat$St %>% filter(day == max(day)) %>% select(age, n) %>% mutate(category = "S")
  E.end <- dat$Et %>% filter(day == max(day)) %>% group_by(age) %>%
    summarize(n = sum(n))  %>% mutate(category = "E")
  I.end <- dat$It %>% filter(day == max(day)) %>% group_by(age) %>%
    summarize(n = sum(n))  %>% mutate(category = "I")
  R.end <- dat$Rt %>% filter(day == max(day)) %>% select(age, n)%>% mutate(category = "R")
  
  N.end <- rbind(S.end, E.end, I.end, R.end)
  
  # summarize disease status on the last year, calculate the prevalence
  dat.sum <- N.end %>%
    group_by(age, category) %>%
    summarize(n = sum(n))  %>% 
    spread(key = category, value = n) %>%
    mutate(exposed = (E + I + R)/ (S + E + I + R)) %>%
    select(age, exposed)

  # prevalence by age
  ggplot(dat.sum, aes(x = age, y = exposed)) +
    geom_line(size = 1.5) + ylim(0,1) +
    ylab("") + xlab("Age category") +
    theme_light()  + theme(text = element_text(size = 18),
                           panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank(),
                           legend.position = c(.25,.85))
}
