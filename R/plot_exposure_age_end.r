
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
#' params <- list(ini.prev = 0.001, n.age.cats = 10,  n.e.cats = 6, n.i.cats = 5, 
#'               e.move = 0.99, i.move = 0.99, beta = .35, theta = 1, 
#'               n0 = 10000, n.days = 360, d.no = 0.05, d.yes = 0.01, beds = 20,
#'               contacts = matrix(1, nrow = 10, ncol = 10),
#'               severity = c(0, 0, 0.001, 0.002, 0.004, 0.010, 0.020, 0.044, 
#'               0.093, 0.200))
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
    ylab("% exposed") + xlab("Age category") +
    theme_light()  + theme(text = element_text(size = 16),
                           panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank())
}
