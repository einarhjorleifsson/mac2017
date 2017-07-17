# ------------------------------------------------------------------------------
# Fitting stock recruitments using the msy packages
# ------------------------------------------------------------------------------

# devtools::install_github("fishfollower/SAM/stockassessment", ref="mack")
library(FLCore)
library(stockassessment)
library(msy)
source("https://github.com/fishfollower/SAM/raw/master/stockassessment/R/tables.R")
library(tidyverse)

source("R/functions/read_mac.R")
source("R/functions/rbx_to_flr.R")
source("R/functions/eqsr_tidy.R")
source("R/functions/eqsr_tidy_plot.R")


# ------------------------------------------------------------------------------
# The benchmark

attach("ass/wkwide/eqSim_res2016.RData")

sr.fit <- sr.tidy <- sr.plot <-  list()
cntr <-
  list(yr = list(c(1980:2015), c(1990:2015), c(1980:2013)),
       name = list("sam0 1980:2015", "sam0 1990:2015", "sam0 1980:2013"))

for (i in 1:length(cntr[[1]])) {

  sr.fit[[i]] <-
    Mac %>%
    FLCore::trim(year = cntr$yr[[i]]) %>%
    msy::eqsr_fit(nsamp = 2500, models = c("Bevholt", "Segreg"))
  sr.tidy[[i]] <-
    sr.fit[[i]] %>%
    eqsr_tidy(Scale = 1e6, name = cntr$name[[i]])
  sr.plot[[i]] <-
    sr.tidy[[i]] %>%
    eqsr_tidy_plot(n = 2e4) +
    labs(title = cntr$name[[i]]) +
    coord_cartesian(xlim = c(0, 5.1), ylim = c(0, 10.5)) +
    scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
    scale_y_continuous(breaks = 1:11)

}
sr.sam0 <- list(sr.fit = sr.fit, sr.tidy = sr.tidy, sr.plot = sr.plot)
save(sr.sam0, file = "data/sr_sam0.rda")


# ------------------------------------------------------------------------------
# ass/einar/run_2.R configuration

load("ass/einar/fit_run2.rda")
Mac <-
  fit %>%
  read_mac(path = "ass/einar") %>%
  rbx_to_flr()

sr.fit <- sr.tidy <- sr.plot <-  list()
cntr <-
  list(yr = list(c(1980:2015), c(1990:2015), c(1980:2013)),
       name = list("sam2 1980:2015", "sam2 1990:2015", "sam2 1980:2013"))

for (i in 1:length(cntr[[1]])) {

  sr.fit[[i]] <-
    Mac %>%
    FLCore::trim(year = cntr$yr[[i]]) %>%
    eqsr_fit(nsamp = 2500, models = c("Bevholt", "Segreg"))
  sr.tidy[[i]] <-
    sr.fit[[i]] %>%
    eqsr_tidy(Scale = 1e6, name = cntr$name[[i]])
  sr.plot[[i]] <-
    sr.tidy[[i]] %>%
    eqsr_tidy_plot(n = 2e4) +
    labs(title = cntr$name[[i]]) +
    coord_cartesian(xlim = c(0, 5.1), ylim = c(0, 10.5)) +
    scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
    scale_y_continuous(breaks = 1:11)

}

sr.sam2 <- list(sr.fit = sr.fit, sr.tidy = sr.tidy, sr.plot = sr.plot)
save(sr.sam2, file = "data/sr_sam2.rda")
