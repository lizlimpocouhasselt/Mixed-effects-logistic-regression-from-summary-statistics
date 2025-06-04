#---------------------------------------------------------------
# FUNCTIONS TO TABULATE POINT AND INTERVAL ESTIMATES:
#   model : estimated model using glmer function
#
# OUTPUT: 
#   dataframe consisting of the estimates
#---------------------------------------------------------------

point <- function(model){
    if(is.null(model)){return(rep(NA, 6))} else{
        var.comp <- as.data.frame(VarCorr(model))$sdcor
        coefs <- summary(model)$coefficients
        return(c(var.comp,
                 coefs['(Intercept)', 'Estimate'],
                 coefs['scale(x1)', 'Estimate'],
                 coefs['scale(x2)', 'Estimate'],
                 coefs['x32', 'Estimate'],
                 coefs['x33', 'Estimate']))
    }
}

interval <- function(confint){
    if(is.null(confint)){matrix(NA, nrow = 6, ncol = 2)} else{
        confint
    }
}

point_mis_1 <- function(model){
    if(is.null(model)){return(rep(NA, 5))} else{
        var.comp <- as.data.frame(VarCorr(model))$sdcor
        coefs <- summary(model)$coefficients
        return(c(var.comp,
                 coefs['(Intercept)', 'Estimate'],
                 coefs['scale(x1)', 'Estimate'],
                 coefs['x32', 'Estimate'],
                 coefs['x33', 'Estimate']))
    }
}

interval_mis_1 <- function(confint){
    if(is.null(confint)){matrix(NA, nrow = 5, ncol = 2)} else{
        confint
    }
}