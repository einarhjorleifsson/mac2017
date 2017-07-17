eqsr_tidy_plot <- function(x, n) {
  i <- sample(nrow(x$out), n)
  p <-
    ggplot2::ggplot(x$out[i,]) +
    ggplot2::theme_bw() +
    geom_ribbon(data = x$Percentiles, aes(ssb, ymin = p05, ymax = p95), fill = "yellow", alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(x=ssb,y=rec, colour=Model),size=0.1, alpha = .3) +
    ggplot2::geom_line(data=x$modelLines,ggplot2::aes(ssb,rec,colour=Model),lwd=1) +
    scale_colour_brewer(palette = "Set1") +
    ggplot2::geom_line(data=x$Percentiles,ggplot2::aes(ssb,p50),col="yellow",lwd=2) +
    ggplot2::coord_cartesian(ylim=c(0, max(stats::quantile(x$out$rec[i],0.99)),x$fit$rec)) +
    ggplot2::geom_path(data=x$fit,ggplot2::aes(ssb,rec),col="black",linetype=2) +
    ggplot2::geom_text(data=x$fit,ggplot2::aes(ssb,rec,label=substr(year,3,4)),size=4,col="black",angle=45) +
    ggplot2::theme(legend.position = c(0.80,0.10)) +
    ggplot2::labs(x="Spawning stock biomass",y="Recruitment",colour="Model")

  return(p)

}

