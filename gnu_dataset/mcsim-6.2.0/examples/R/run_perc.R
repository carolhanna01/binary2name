library(deSolve)

rebuild <- function() {
    if (is.loaded("derivs", PACKAGE="perc_delay"))
        dyn.unload(paste0("perc_delay",.Platform$dynlib.ext))
    system("mod -R perc_delay.model perc_delay.c")
    system("R CMD SHLIB perc_delay.c")
    dyn.load(paste0("perc_delay",.Platform$dynlib.ext))
    source("perc_delay_inits.R")
}

buildPerDose <- function(mag, period, inittime, exposuretime, times) {
    mintime <- min(times)
    maxtime <- max(times)
    nperiods <- ceiling(maxtime / period) + 1
    col1 <- rep(inittime, 2 * nperiods)
    I <- 1:length(col1)
    J <- which(I %% 2 == 1)
    col1[J]  <- col1[J] + (ceiling((J / 2)) - 1) * period
    J <- which(I %% 2 == 0)
    col1[J] <- col1[J-1] + exposuretime
    out <- cbind(col1, rep(c(mag,0), length(col1)/2))
    if (inittime > mintime) out <- rbind(c(mintime, 0), out)
    out
}

buildConstDose <- function(mag, times) {
    out <- rbind(c(min(times), mag), c(max(times) + 1, 0))
    out
}

perc <- function(times=NULL, newParms=NULL) {
    if (is.null(times)) stop("give me just a little more time")
    parms <- initParms(newParms=newParms)
    Y <- initStates(parms=parms)
    ## Add code to implement in the inputs built into the model.
    Forcings <- list(buildPerDose(parms["InhMag"], 1e10, 0, 239.99999, times),
                     buildConstDose(0, times))
    out <- dede(Y, times, func="derivs", parms=parms, dllname="perc_delay",
               initfunc="initmod", nout=length(Outputs), outnames=Outputs,
               initforc="initforc",
               fcontrol=list(method="constant", rule=1, f=0),
               forcings=Forcings)
    as.data.frame(out)
}

times <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 
           160, 170, 180, 190, 200, 210, 220, 230, 239.99, 240, 250, 260, 270, 
           280, 290, 300, 310, 320, 330, 339.9, 340, 350)

savedout <- read.delim("perc_delay.out_ref", sep="\t", skip=2)

out <- perc(times, newParms=c(InhMag = 72, Period=1e10, Exposure=240, tau=100))
out2 <- out[,c("time","Q_exh","C_alv","Q_liv","old_Q_liv")]

## Relative error for each of the non-time columns.
## Drop the first row, at time = 0, since it is all 0's
## only use the non-NA values for the mean, because the delayed variable starts
## with a bunch of 0's (which yield NaN's when you compute relative error).
RelErr <- matrix(NA, nrow=ncol(out2) - 1, ncol=1,
                 dimnames=list(colnames(out2)[-1],"RelErr"))
for (i in 2:ncol(out2)) {
    RelErr[(i - 1), 1] <-
      mean(abs((out2[-1,i] - savedout[-1,i])) /
           (0.5*(out2[-1,i] + savedout[-1,i])), na.rm=TRUE)
}

print(RelErr)
    
