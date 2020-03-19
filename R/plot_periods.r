#' Plot time in exposed and infectious categories
#' 
#' Creates a plot of the time (in days) a person spends in exposed and infectious
#' categories. Uses 1000 draws from a Gamma distribution.
#'
#' @param e.move proportion or probability of progressing through the E 
#' category. Must be between 0 and 1.
#' @param i.move proportion or probability of progressing through the I
#' category. Must be between 0 and 1.
#' @param n.e.cats number of exposed sub categories
#' @param n.i.cats number of infectious sub categories
#' @return a density plot of time in the E or I category.
#'
#' @import ggplot2
#' @importFrom ggridges theme_ridges geom_density_ridges
#' @importFrom stats rgamma
#' @importFrom tidyr gather
#' @examples
#' plot_periods(e.move = 0.95, i.move = 0.95, n.e.cats = 5, n.i.cats = 3)
#' 
#' @export

plot_periods <- function(e.move, i.move, n.e.cats, n.i.cats){
  if(missing(e.move)==TRUE) warning("missing e.move parameter")
  if(missing(i.move)==TRUE) warning("missing i.move parameter")
  if(missing(n.e.cats)==TRUE) warning("missing n.e.cats parameter")
  if(missing(n.i.cats)==TRUE) warning("missing n.i.cats parameter")
  
  exposure <- data.frame(exposure = rgamma(1000, n.e.cats, e.move))
  infectious <- data.frame(infectious = rgamma(1000, n.i.cats, i.move))

  #create a wide data.frame
  periods <- data.frame(exposure = exposure, infectious = infectious)
  
  periods.2 <-  periods %>%
    gather('exposure', 'infectious', key = "parameter", value = "value")
  
  p <- ggplot(periods.2, aes(x = value, y = parameter)) +
        geom_density_ridges() + theme_ridges() + ylab("") + xlab("days") + 
        scale_y_discrete(labels = c("exposure period",
                                "infectious period")) +
    theme_bw(base_size = 16)  
  p
}
