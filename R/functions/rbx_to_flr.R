#devtools::install_github("ices-tools-prod/msy")
#library(msy)
#library(MASS)
#library(FLCore)
#library(FLSAM)
#library(stockassessment)
#library(tidyverse)
#attach("from_thomas/WKWIDE_2017_final_run/fitAR-2.RData")
#source("R/read_mac.R")
#library(repmis)
#source("https://github.com/fishfollower/SAM/raw/master/stockassessment/R/tables.R")
#rbx <- fit %>% read_mac()
#rbx2012 <- fishvise::read_adcam("/net/hafkaldi/export/home/haf/einarhj/ass/2012/01/05ass/Adcam", "smx", assYear = 2012, calcSurBio = T)

rbx_to_flr <- function(rbx) {

  rby <- rbx$rby
  rbya <- rbx$rbya
  rbya <-
    rbya %>%
    mutate(oC = ifelse(is.na(oC), 0, oC),
           cW = ifelse(is.na(cW), 0, cW),
           dW = ifelse(is.na(dW), 0, dW),
           lW = ifelse(is.na(lW), 0, lW),
           sW = ifelse(is.na(sW), 0, sW),
           f  = ifelse(is.na(f),  0, f),
           mat = ifelse(is.na(mat), 0, mat))


  # convert rbyaa to FLQuants
  x <- FLCore::FLQuants()
  for (i in 3:13) {
    df <- rbya[, c(1, 2, i)]
    nome <- names(rbya[i])
    names(df)[3] <- "data"
    x[[nome]] <- FLCore::as.FLQuant(df)
  }
  macflr <- FLCore::FLStock(name = "mac", desc = "from scratch",
                    catch.n = x$oC,
                    catch.wt = x$cW,
                    landings.n = x$oC,
                    landings.wt = x$cW,
                    stock.n = x$n,
                    stock.wt = x$sW,
                    m = x$m,
                    mat = x$mat,
                    harvest = x$f,
                    harvest.spwn = x$pF,
                    m.spwn = x$pM)
  FLCore::catch(macflr) <- rby$oY
  FLCore::discards.n(macflr) <- 0
  FLCore::discards.wt(macflr) <- 0
  FLCore::discards(macflr) <- 0
  FLCore::landings(macflr) <- rby$oY
  # check this:
  FLCore::units(macflr)[1:17] <- as.list(c(rep(c("tonnes", "thousands", "kg"), 4), "NA",
                                   "NA", "f", "NA", "NA"))
  range(macflr) <- c(0, 12, 12, 1980, 2016, 4, 8)

  return(macflr)
}

