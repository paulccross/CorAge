#' Plot the severity versus age
#' 
#' Creates a plot of the proportion of severe cases by age category. Severe is 
#' assumed to be those that would need hospitalization
#'
#' @param s.lo proportion of severe cases in the lowest age category
#' @param s.hi proportion of severe cases in the highest age category
#' @param n.age.cats number of age categories
#' @return a plot of severity versus age
#'
#' @import ggplot2
#' @examples
#' plot_severity(s.lo = 0.01, s.hi = 0.2, n.age.cats = 10)
#' 
#' @export

plot_severity <- function(s.lo, s.hi, n.age.cats){
  if(missing(s.lo)==TRUE) warning("missing s.lo parameter")
  if(missing(s.hi)==TRUE) warning("missing s.hi parameter")
  if(missing(n.age.cats)==TRUE) warning("missing n.age.cats parameter")

  # calculate severity vector as an exponential model from lo to hi
  # based on s_t = s_0(1+r)^n.age.cats
  r <- (s.hi / s.lo)^(1/n.age.cats) - 1
  dat <- data.frame(severity = s.lo * (1+r) ^ seq(1,n.age.cats, 1), 
                       age = seq(1,n.age.cats,1))
  
  ggplot(dat, aes(age, severity)) + geom_line(size = 1.5) + 
    xlab("Age category") + ylab("% severe cases") +  
    theme_light(base_size = 16) +
    theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
}
