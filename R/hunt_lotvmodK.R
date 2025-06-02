#' Lot. Voltera Model with Hunting
#'
#' function computes the rate of change of populations in a predictor prey interaction
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values prey=number of prey and pred=number of predictor
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, par$pmort
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators
#'  \emph{alpha} is a interaction coefficient (higher values greater interaction
#'  \emph{pmort}  mortality rate of predictor population
#'  \emph{K} carrying capacity of prey
#'  \emph{hunt_rate} proportion of the prey population that is hunted
#'  \emph{min_prey_thresh} minimum prey population above which hunting is allowed
#'  
#' @examples
#' hunt_lotvmodK(t = 1, pop = list(1, 2), pop = list(0.5, 0.3, 0.2, 0.2)) 
#'
#' pars <- c(rprey = 0.5, alpha = 0.3, eff = 0.2, pmort = 0.2, K = 10, hunt_rate = 0.1, min_prey_thresh = 2)
#' currpop <- c(prey = 1, pred = 1)
#  days = seq(from=1,to=20)
#' res <- ode(func = lotvmod, y = currpop, times = days, parms = pars)
#'
#' @return  hunt_lotvmodK returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey populutation with hunting}
#' \item{dpred}{rate of change of preditor populutation}
#' }

hunt_lotvmodK <- function(t, pop, pars) {
  with(as.list(c(pars, pop)), {
    
    # apply hunting only if hunting take is less than prey pop
    # should never hunt more prey than exist as long as hunt_rate is less than 1
    if (hunt_rate >= 1) {
      stop("Hunting rate cannot equal or exceed prey population size (must be < 1).")
    }
    
    # apply hunting if prey pop is greater than a certain minimum prey pop
    hunting <- 0 # ensures there's always a value for hunting even if the min prey pop is not met
    if (prey > min_prey_thresh) { # ensures we keep a minimum prey population
      hunting <- hunt_rate*prey  # hunt_rate is the percent of the prey population hunted
    }
    
    # Prey and predator rate of change with hunting included
    dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred - hunting
    dpred <- eff * alpha * prey * pred - pmort * pred
    
    return(list(c(dprey, dpred)))
  })
}

