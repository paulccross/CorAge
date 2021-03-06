% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_cases_time.r
\name{plot_cases_time}
\alias{plot_cases_time}
\title{Plot the infectious cases by age over time}
\usage{
plot_cases_time(dat, total)
}
\arguments{
\item{dat}{data.frame of infectious individuals provided as output from the det model functions}

\item{total}{plot all cases combined when total = True, otherwise split by age}
}
\value{
a plot of the population totals split by age.
}
\description{
Plot the infectious cases by age over time
}
\examples{
n.age.cats <- 10

params <- list(ini.prev = 0.02, n.age.cats = n.age.cats,  n.e.cats = 10, 
n.i.cats = 10, e.move = 0.4, i.move = 0.4, beta = 0.08, theta = 1, n0 = 10000, 
n.days = 360, contacts = matrix(1, nrow = n.age.cats, ncol = n.age.cats),
severity = seq(0.01, 0.2, length.out = n.age.cats), d.no = 0.1, d.yes = 0.02,
beds = 20)

out <- det_model(params)
plot_cases_time(out$counts$It, total = T)
plot_cases_time(out$counts$It, total = F)

}
