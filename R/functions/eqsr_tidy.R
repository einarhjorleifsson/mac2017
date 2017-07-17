eqsr_tidy <- function (fit, n = 5000, x.mult=1.1, y.mult=1.4, Scale = 1, name) {
    #x.mult <- 1.1
    #y.mult <- 1.4
    ## dummy stuff
    Ftarget <- p05 <- p95 <- p50 <- variable <- value <- year <- Model <- rec <- 0

    modset <- fit$sr.sto
    data <- fit$rby[,1:3]

    minSSB <- min(data$ssb, max(data$ssb)*0.0125)
    #maxSSB <- max(data$ssb)*x.mult
    maxSSB <- 4607753 * x.mult
    maxrec <- max(data$rec* y.mult)
    ##############################################################################
    # very strange way to do things
    out <-
      do.call(rbind, lapply(sample(1:nrow(modset), 500),
                            function(i)
                            {
                              fssb <- stats::runif(500, minSSB, maxSSB)
                              FUN <-  match.fun(modset $ model[i])
                              frec <- exp( FUN(modset[i,], fssb) + stats::rnorm(500, sd = modset $ cv[i]) )
                              srModel <- modset$model[i]
                              #points(fssb, frec, pch = 20, col = paste0(grDevices::grey(0), "05"), cex = 0.0625)
                              data.frame(ssb = fssb, rec = frec, model=srModel)
                            }))

    # group the ssbs into 10 bins
    out $ grp <- with(out, floor(20 * (ssb - min(ssb)) / (max(ssb) - min(ssb) + 0.001)))
    # find the midvalue of ssb within each group
    out $ mid.grp <- with(out, (grp + 0.5) / 20 * (max(ssb) - min(ssb)) + min(ssb))
    tmp <- fit$sr.det
    tmp$Model <- paste(tmp$model, round(tmp$prop, 2))
    out <- plyr::join(out,tmp[,c("model","Model")],by="model")
    # calculate the recruitment median and 5th and 95th percentile within each
    # ssb group and then plot the distribution
    summ <- with(out,
                 t(simplify2array( tapply(rec, grp, stats::quantile, c(0.5, .05, .95)) )))
    mid.grp <- sort(unique(out $ mid.grp))

    # For ggplot2
    Percentiles <- data.frame(ssb=mid.grp,p50=summ[,1],p05=summ[,2],p95=summ[,3])

    x <- fit$sr.det
    ssb <- seq(1,round(max(maxSSB)),length=100)
    z <- sapply(1:nrow(x), function(i) rec <- exp(match.fun(as.character(x$model[i])) (x[i,], ssb)))
    modelLines <- as.data.frame(cbind(ssb,z))
    names(modelLines) <- c("ssb",paste(x$model, round(x$prop, 2)))
    modelLines <- reshape2::melt(modelLines,id.var="ssb",variable.name="Model",value.name="rec")

    out$ssb <- out$ssb/Scale
    out$rec <- out$rec/Scale
    out$mid.grp <- out$mid.grp/Scale
    Percentiles$ssb <- Percentiles$ssb/Scale
    Percentiles$p50 <- Percentiles$p50/Scale
    Percentiles$p05 <- Percentiles$p05/Scale
    Percentiles$p95 <- Percentiles$p95/Scale

    modelLines$ssb <- modelLines$ssb/Scale
    modelLines$rec <- modelLines$rec/Scale

    fit$rby$ssb <- fit$rby$ssb/Scale
    fit$rby$rec <- fit$rby$rec/Scale

    if(!missing(name)) {
      out$name <- name
      modelLines$name <- name
      Percentiles$name <- name
    }

    return(list(out = out, modelLines = modelLines, Percentiles = Percentiles, fit = fit$rby))


}



