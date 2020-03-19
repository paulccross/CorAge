#' Plot the contact matrix
#'
#' @param contacts a symmetrical matrix of contact rates by age
#' @return a heatmap of that contact matrix.
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr mutate
#' @examples
#' plot_contacts(contacts)
#' 
#' @export

plot_contacts <- function(dat){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  dat <- dat %>%
    as_tibble() %>%
    rowid_to_column(var="X") %>%
    gather(key="Y", value="Z", -1) %>%
    
    # Change Y to numeric
    mutate(Y=as.numeric(gsub("V","",Y))) 
  
  names(dat) <- c("age_i", "age_j", "contacts")
    
  # Viz
  ggplot(dat, aes(age_i, age_j, fill= contacts)) + 
    geom_tile() + xlab("Age") + ylab("Age") + 
    scale_fill_gradient2(low="blue", mid = "white", high="red", midpoint = 1) +
    theme_light(base_size = 16) +
    theme(legend.position="right",
          panel.grid.minor = element_blank())
}
