#' Deterministic model function
#'
#' Deterministic daily age structured model
#'  
#' @param params A list with the following parameters included: 
#' 
#' ini.prev = percentage of individuals infected at the start (scalar value between 0 and 1),  
#' 
#' n.age.cats = number of age categories to monitor. Currently makes the most sense to be by decade for 10 categories (must be scalar value greater than 3).
#' 
#' n.i.cats = number of I subcategories
#' 
#' n.e.cats = number of E subcategories
#' 
#' e.move = rate of movement in the exposed categories (scalar values between 0 and 1). 
#' 
#' i.move = rate of movement in the infectious categories (scalar between 0 and 1)
#' 
#' beta = transmission coefficient   
#' 
#' contacts = symmetrical matrix of n.age.cats*n.age.cats of the contact rate between age categories. 1 = average, 1.1 = 10% increase, .9 = 10% decrease. 
#' 
#' n0 = initial population size (scalar value greater than 0)
#' 
#' n.days = number of days to run the model (scalar value greater than 2),
#' 
#' severity is a vector of length n.age.cats of the proportion of I that goes to the hospital
#' 
#' d.no is the proportion of severe cases that die per day w/o hospital care (currently scalar)
#' 
#' d.yes is the proportion that die with hospital care
#' 
#' beds is the # of beds or ICUs that are available
#' 
#' @return A list with 3 outputs: 
#' 
#' 1. List of counts of the # of individuals in S, E, I, R categories over time with the following columns: age category, day of simulation, sub category (for E and I), n = number of individuals
#'   
#' 2. List of outcomes-- Dt = deaths, Ht = hospitalizations needed
#' 
#'  Lists for Dt and Ht with the following columns: age category, day of the simulation, n = # of individuals 
#'  
#' 3. R0 = basic disease reproductive number 
#'    
#' @importFrom popbio stable.stage
#' @importFrom dplyr rename mutate
#' @importFrom reshape2 melt
#' @importFrom stats rgamma
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
#' 
#' @export

det_model <- function(params) {
  #### warnings ####
  # write the list objects to the local environment
  for (v in 1:length(params)) assign(names(params)[v], params[[v]])
  if(exists("ini.prev")==FALSE){
    message("initial prevalence is missing, using default value")
    ini.ad.m.prev <- 0.01
  }
  if(exists("n.age.cats")==FALSE){
    message("# of age categories is missing, using default value")
    n.age.cats <- 10
  }
  
  if(exists("e.move")==FALSE){
    message("e.move is missing, using default value")
    e.move <- 0.4
  }

  if(exists("i.move")==FALSE){
    message("i.move is missing, using default value")
    i.move <- 0.4
  }
  
  if(exists("beta")==FALSE){
    message("transmission beta is missing, using default value")
    beta <- 0.08
  }
  
  if(exists("theta")==FALSE){
    message("theta is missing, using default value")
    theta <- 1
  }
 
  if(exists("n0")==FALSE){
    message("initial population size n0 is missing, using default value")
    n0 <- 10000
  }
  
  if(exists("n.days")==FALSE){
    message("n.days is missing, using default value")
    n.days <- 360
  }

  ##### check parameter values #####
  if(ini.prev < 0) warning("ini.prev must >=0")
  if(ini.prev > 1) warning("ini.prev must be <= 1")

  if(n.age.cats < 4) warning("n.age.cats must be 4 or more")
  if(e.move < 0) warning("e.move must be between 0 and 1")
  if(e.move > 1) warning("e.move must be between 0 and 1")
  if(i.move < 0) warning("i.move must be between 0 and 1")
  if(i.move > 1) warning("i.move must be between 0 and 1")
  if(beta < 0) warning("beta cannot be negative")
  if(n0 <= 0) warning("n0 must be positive")
  if(n.days <= 0) warning("n.days must be positive")
  if(n.e.cats <= 2) warning("n.e.cats must be >= 2")
  if(n.i.cats <= 2) warning("n.i.cats must be >= 2")
  
  #### INITIAL CONDITIONS #### 
  
  #NOTE!!! currently making up these survival and repro values
  #NOTE!!! n.age.cats not fully flexible, best if n.age.cats = 10 (1 for each decade)
  prop.aging <- 1/n.age.cats # proportion that age out of a category (approx.)
  mort <- rep(0.002, n.age.cats - 3)
  old.mort <- rep(0.1, 3) # last 3 categories
  
  remain <- rep(1, n.age.cats) - prop.aging - c(mort, old.mort)
  repro <- 0.1

  # Create the matrix to start the population at stable age dist
  M <- matrix(rep(0, n.age.cats * n.age.cats), nrow = n.age.cats)
  diag(M) <- remain
  M[row(M) == (col(M) + 1)] <- prop.aging # off diagonal
  M[n.age.cats, n.age.cats] <- 1-old.mort[1] # old category mortality
    
  # insert the fecundity vector for prebirth census
  M[1, 2:4] <- repro * (1-mort[1]) * 0.5
  
  #lambda(M)
  #plot(stable.stage(M))
  
  # pre-allocate the output matrices
  tmp <- matrix(0, nrow = n.age.cats, ncol = n.days)
  St <- tmp  # susceptible 
  Et <- array(rep(tmp), dim = c(n.age.cats, n.days, n.e.cats)) # exposed
  It <- array(rep(tmp), dim = c(n.age.cats, n.days, n.i.cats))  # infectious
  Rt <- tmp # recovered
  
  # for tracking purposes
  Dt <- tmp # disease deaths
  Ht <- tmp  # hospitalizations 
  
  # Intializing with the stable age distribution.
  St[, 1] <- popbio::stable.stage(M)[1:n.age.cats] * n0 * (1 - ini.prev)

  # equally allocating prevalence across ages, but only in I not E.
  It[, 1, 1:n.i.cats] <- popbio::stable.stage(M)[1:n.age.cats] * n0/n.i.cats *
    ini.prev

  # calculate R0 (not sure about this)
  R0 <- (1-exp (-(beta * n0) / n0^theta)) * 
    mean(rgamma(1000, n.i.cats, i.move)) 
  # assumes no mortality during infectious period and no effect of 
  # hospitalization or containment
  # doesn't account for contact matrix. Probably need an NGM approach
  
  #### MODEL ####
  for (t in 2:(n.days)) {
    # bring forward previous month, then modify it
    St[, t]   <- St[, t - 1]
    Et[, t, ] <- Et[, t - 1, ]
    It[, t, ] <- It[, t - 1, ]
    Rt[, t]   <- Rt[, t - 1]
    
    # Move E individuals forward in their subcategories
    e.movers <- Et[, t, ] * e.move
    
    Et[, t, 1] <- Et[, t, 1] - e.movers[, 1]
    Et[, t, 2:n.e.cats] <- Et[, t, 2:n.e.cats] - e.movers[, 2:n.e.cats] + 
      e.movers[, 1:(n.e.cats-1)]
    
    # Exposed become infectious 
    It[, t, 1] <- It[, t, 1] + e.movers[ , n.e.cats]
    
    # Move I individuals forward in their subcategories
    i.movers <- It[, t, ] * i.move
    
    It[, t, 1] <- It[, t, 1] - i.movers[, 1]
    It[, t, 2:n.i.cats] <- It[, t, 2:n.i.cats] - i.movers[, 2:n.i.cats] + 
                            i.movers[, 1:(n.i.cats-1)]

    # infectious individs exit from the last I cat into recovered
    Rt[ , t] <- Rt[ , t] + i.movers[ , n.i.cats] 
    
    ##### Transmission #####
    I.current <- rowSums(It[ ,t, ]) # current # of I's by age
    Nall <- sum(St[, t]) + sum(Et[, t, ]) + sum(It[, t, ]) + sum(Rt[, t]) #total
    
    # probably a faster way to do this
    cases <- St[, t] * contacts %*% 
                       (1 - exp( -(beta * (as.matrix(I.current)/ Nall ^ theta))))
    
    St[, t] <- St[, t] - cases
    Et[, t, 1] <- Et[, t, 1] + cases
  
    ##### Hospitalizations needed #####
    Ht[ ,t] <- severity * I.current
    
    ##### Deaths #####
    # hospital beds are allocated proportion to case load
    # maybe modify in the future for different options
    hos.yes <- min(c(beds * Ht[,t]/sum(Ht[,t]), Ht[,t])) # hospitalized
    hos.no <- Ht[,t] - hos.yes # not able to be hospitalized 
    
    # deaths depend on whether they are hospitalized
    Dt[ ,t] <- hos.no * d.no + hos.yes * d.yes

    # remove those deaths from the I category
    # currently not skewed by time in the infectious class
    It[ , t, ] <- It[ , t, ] - (It[ , t, ] / I.current) * Dt[ ,t]
  }

  ##### organize the output #####
  St <- as.data.frame(t(St))
  names(St) <- paste("age", seq(1,n.age.cats), sep = ".")
  St$day <- 1:n.days
  
  St.long <- melt(St, id.vars = c("day")) %>%
    rename(age = variable, n = value)
  St.long$age <- as.numeric(St.long$age)
  St.long <- St.long[,c("age", "day", "n")]
  
  Rt <- as.data.frame(t(Rt))
  names(Rt) <- paste("age", seq(1,n.age.cats), sep = ".")
  Rt$day <- 1:n.days
  
  Rt.long <- melt(Rt, id.vars = c("day")) %>%
    rename(age = variable, n = value)
  Rt.long$age <- as.numeric(Rt.long$age)
  Rt.long <- Rt.long[,c("age", "day", "n")]
  
  Dt <- as.data.frame(t(Dt))
  names(Dt) <- paste("age", seq(1,n.age.cats), sep = ".")
  Dt$day <- 1:n.days
  
  Dt.long <- melt(Dt, id.vars = c("day")) %>%
    rename(age = variable, n = value)
  Dt.long$age <- as.numeric(Dt.long$age)
  Dt.long <- Dt.long[,c("age", "day", "n")]
  
  Ht <- as.data.frame(t(Ht))
  names(Ht) <- paste("age", seq(1,n.age.cats), sep = ".")
  Ht$day <- 1:n.days
  
  Ht.long <- melt(Ht, id.vars = c("day")) %>%
    rename(age = variable, n = value)
  Ht.long$age <- as.numeric(Ht.long$age)
  Ht.long <- Ht.long[,c("age", "day", "n")]

    # convert array to matrix format
  Et.long <- melt(Et) %>%
    rename(age = Var1, day = Var2, subcat = Var3, n = value)
  It.long <- melt(It) %>%
    rename(age = Var1, day = Var2, subcat = Var3, n = value)
  
  counts <- list(St = St.long, Et = Et.long, It = It.long, Rt = Rt.long)
  outcomes <- list(Dt = Dt.long, Ht = Ht.long)
  output <- list(counts = counts, outcomes = outcomes, R0 = R0)
}

