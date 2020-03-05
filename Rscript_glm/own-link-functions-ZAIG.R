# those are own link functions for taking into the account the exposure 
# for ZAIG distribution 
#----------------------------------------------------------------------------------------
# own link function functions
own.linkfun <- function(mu)
                       {            
        eta <- if (any(exposure-mu <= 0)) log(((1-mu))/abs(exposure-(1-mu))) else 
                       log(((1-mu))/(exposure-(1-mu)))
        eta 
                       }
#----------------------------------------------------------------------------------------
own.linkinv <- function(eta) 
            {
            thresh <- -log(.Machine$double.eps)
               eta <- pmin(thresh, pmax(eta, -thresh))
                      (1-exposure)+(exposure/(1 + exp(eta)))
            }
#----------------------------------------------------------------------------------------
own.mu.eta <- function(eta) 
            {
            thresh <- -log(.Machine$double.eps)
               res <- rep(.Machine$double.eps, length(eta))
            res[abs(eta) < thresh] <- ((-exposure*exp(eta))/(1 + 
                           exp(eta))^2)[abs(eta) < thresh]
            res
           }
#----------------------------------------------------------------------------------------
own.valideta <- function(eta) TRUE
#----------------------------------------------------------------------------------------
