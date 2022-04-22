## Benchmark results for some functions involving Data exhibit a fan shape,
## where all the data points lie above the x-axis but below some sloping
## straight line (this is because Data is heterogeneous but we only have a
## single size measure, and objects of the same size can have significantly
## different structures).  The "fit.fan" function is supposed to find an
## approximation t=ax+b to this line, which should provide a conservative upper
## bound for execution time in terms of input size.  We find initial values for
## a and b by using lm to fit a linear model, and then adjust these until we get
## a reasonable fit.  The adjustment process is somewhat complicated.  We want
## our estimate to give an upper bound, but the bound not be absurdly large. To
## achieve this we use a bisection method to adjust the slope until we get a
## line such that 95% of the data points are below it, but not 100%.  This
## should give us a bound which is reasonable for most data while ignoring
## outliers.  It's hard to tell entirely automatically if exceptionally large
## values are random outliers or systematic due to certain input values
## genuinely taking very long times. Visual inspection of the data will help
## with this.

fit.fan1 <- function(f,fname="<unknown>",do.plot=FALSE,lim=1e10, crop=1e8) {
    ## f should be a frame with at least "name", "x", and "t" columns.

    threshold <- 0.9
    fname <- f$name[1]
    x <- f$x
    t <- f$t

#    x <- x[x<lim]
#    t <- t[x<lim]

    ## Given a slope and intercept, return the fraction of the (x,t) points
    ## which lie below the corresponding line, ie the fraction for which the
    ## line is an overestimate.
    overestimated <- function(intercept, slope) {
        predicted <- sapply(x, function(z) {intercept+slope*z})
        return (length(which(predicted > t))/length(x))
    }

    ## Fit a simple linear model for initial estimates.
    m <- lm (t~x)
    if (do.plot) {
        xmax = min(crop,max(x))
        tmax = max(f$t[f$x<=xmax])
        plot(x,t,xlab="Input size",ylab="Time",xlim=c(0,xmax),ylim=c(0,tmax))
        abline(m,col=7)
    }

    min.x <- min(x)             # Minimum x value
    min.ts <- f$t[f$x==min.x]   # t values for minimum x value
    a <- quantile(min.ts,0.8)
    ## ^ Take this as the initial intercept, not the intercept from the model
    ## (that could be negative, for example).  Look at the 80th percentile to
    ## avoid problems when there's a big cloud of points around the minimum x
    ## value, which is the case with our current generator.
    b0 <- m$coefficients["x"]  # Initial slope
    over <- overestimated(a,b0)

    ## Increase the slope until the line overpredicts everything.  If that fails
    ## (which it might if there are a lot of data points near the point where
    ## the line crosses the t-axis), increase the intercept.  If that still
    ## fails, give up.
    n1 <- 1  # Loop counter
    while (over < 1 && n1<50) {
        b <- b0
        n2 <- 1  # Loop counter
        while (over<1 && n2<100) {
            b <- 1.05*b
            over <- overestimated(a,b)
            n2 <- n2+1
        }
        if (over<1) {  ## We've got to the limit without getting everything under the line.
            a <- 1.1*a
            over <- overestimated(a,b0)
            n1 <- n1+1
        }
    }
    if (over<1) stop (sprintf("** ERROR: can't find initial slope and intercept for %s", fname))

    ## Now bisect the slope between b and b0 until we're over the threshold, but not by 100%
    n <- 1  # Loop counter
    limit = 1000
    lower <- b0
    upper <- b
    b <- b0 # Start with a low estimate and work up, so we don't just find a line that throws aeay a few outliers
    over <- overestimated(a,b)
    while (over < .9 || over == 1) {
        cat (sprintf ("slope = %g\n", b))
        if (n>limit) {
            stop (sprintf ("No slope found for %s after %d iterations\n", fname, limit))
        }

        if (over < threshold) {  # increase the slope
            cat ("Increasing slope\n")
            tmp <- b
            b <- (b+upper)/2
            lower <- tmp
        }

        over <- overestimated(a,b)
        cat (sprintf ("overestimate = %g\n", over))
        if (over == 1) { # decrease the slope
            cat ("Decreasing slope\n")
            tmp <- b
            b <- (b+lower)/2
            uppper <- tmp
        }
        n <- n+1
    }
    
    if (do.plot) abline(a,b,col=2) # Display the fitted line in red
    predicted <- sapply(x, function(z) {a+b*z})
    
    cat (sprintf ("INFO: %d points from %d are underpredictions in the final model\n", length(which(t>predicted)), length(x)))
    cat (sprintf ("INFO: maximum underprediction is %.2f%%\n", max((t-predicted)/t*100)))
    over <- ((predicted-t)/t)[t<predicted]
    cat (sprintf ("INFO: average overprediction is %.2f%%\n", mean(over)*100))
    
}


fit.fan2 <- function(f,fname="<unknown>",do.plot=FALSE,lim=1e10, crop=1e8) {
    m1 <- lm(t~x,f)
    f2 <- f[f$t>m1$fitted.values,]
    m2 <- lm(t~x,f2)
    f3 <- f2[f2$t>m2$fitted.values,]
    m3 <- lm(t~x,f2)
    m3
}


## Benchmark results for some functions involving Data exhibit a fan shape where
## all the data points lie above the x-axis but below some sloping straight line
## (this is because Data is heterogeneous but we only have a single size
## measure, and objects of the same size can have significantly different
## structures).  The "fit.fan" function is supposed to find an approximation
## t=ax+b to this line, which should provide a conservative upper bound for
## execution time in terms of input size. We do this by fitting a linear model,
## discarding all of the points below the regression line, and repeating until
## the number of overestimates produced by running the model on the full dataset
## is less than some threshold (default 10%) or until a limit on the number of
## iterations (default 20) is exceeded.  If the process is iterated too many
## times you can end up getting errors because you've discarded all of the data.
## Setting the do.plot argument to TRUE  produces an informative plot, but this
## should only be used interactively.

fit.fan <- function(f, threshold=0.1, limit=20, do.plot=FALSE) {
    fname <- f$name[1]

    ## The benchmark data we currently have is concentrated towards the origin,
    ## which can cause some difficulty because lower values become too
    ## influential.  We force the model to have an intercept which causes most
    ## of the small-x data to lie below the regression line, although this
    ## complicates matters somewhat.

    min.x <- min(f$x)           # Smallest x value
    min.ts <- f$t[f$x==min.x]   # All of the t values for this x value
    t0 <- quantile(min.ts,0.8)  # Fixed intercept

    npoints <- length(f$x)
    g <- f

    loops = 0
    repeat {
        m <- lm(t ~ x, g)
        slope <- m$coefficients["x"]
        pred <- function(v) {
            sapply(v, function(z) { t0 + slope*z })  # Predictions with the fitted slope and our own intercept
        }
        nunder <- length(which(f$t>pred(f$x)))       # Underestimated points in the full dataset.
        loops = loops+1
        if (nunder/npoints <= threshold || loops >= limit) break
##      g <- g[g$t>m$fitted.values,]          # Discard overestimated points (accoridng to the fitted model) and start again
        g <- g[g$t>pred(g$x),]               # Discard overestimated points (according to our adjusted model) and start again
    }

    ## Report some diagnostic information (this will be seen when generate-cost-model is run)
    pred.f <- pred(f$x)
    over = which(pred.f > f$t)
    ratio = mean(pred.f[over]/f$t[over])

    cat (sprintf("INFO (%s): %d/%d points (%.1f%%) are underestimated in final model.\n", fname, nunder, npoints, (nunder/npoints)*100))
    cat (sprintf("INFO (%s): mean overestimate = %.1fx.\n", fname, ratio))
    cat (sprintf("Finished after %d iterations\n", n))

    ## Adjust m's intercept; this is questionable since the rest of the model
    ## data becomes meaningless.
    m$coefficients["(Intercept)"] <- t0  

    if (do.plot) {
        plot(f$x,f$t)
        abline(m,col=2)
    }
    return(m)
}

