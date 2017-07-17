read_mac <- function(fit, path = "ass/wkwide/", scale = 1e6, model = "sam") {

  lh <- function(m) {

    yr <- as.integer(row.names(m))
    m %>%
      as_data_frame() %>%
      mutate(year = yr)

  }

  if (stringr::str_sub(path, nchar(path)) != "/") path = paste0(path, "/")

  n <- fit %>% ntable() %>% lh() %>%  gather(age, value = n, -year, convert = TRUE) %>% mutate(n = n/scale)
  f <- fit %>% faytable() %>% lh() %>%  gather(age, value = f, -year, convert = TRUE)

  rbya <-
    fishvice::read_lowestoft2(paste0(path, "cn.dat"), value.name = "oC") %>%
    mutate(oC = oC/scale) %>%
    full_join(fishvice::read_lowestoft2(paste0(path, "cw.dat"), value.name = "cW"), by = c("year", "age")) %>%
    full_join(fishvice::read_lowestoft2(paste0(path, "dw.dat"), value.name = "dW"), by = c("year", "age")) %>%
    full_join(fishvice::read_lowestoft2(paste0(path, "lw.dat"), value.name = "lW"), by = c("year", "age")) %>%
    full_join(fishvice::read_lowestoft2(paste0(path, "mo.dat"), value.name = "mat"), by = c("year", "age")) %>%
    full_join(fishvice::read_lowestoft2(paste0(path, "nm.dat"), value.name = "m"), by = c("year", "age")) %>%
    full_join(fishvice::read_lowestoft2(paste0(path, "pf.dat"), value.name = "pF"), by = c("year", "age")) %>%
    full_join(fishvice::read_lowestoft2(paste0(path, "pm.dat"), value.name = "pM"), by = c("year", "age")) %>%
    full_join(fishvice::read_lowestoft2(paste0(path, "sw.dat"), value.name = "sW"), by = c("year", "age")) %>%
    full_join(n) %>%
    full_join(f) %>%
    mutate(model = model,
           yc = year - age)

  lh <- function(m) {

    yr <- as.integer(row.names(m))
    m %>%
      as_data_frame() %>%
      mutate(year = yr)
  }

  oY <-
    rbya %>%
    group_by(year) %>%
    summarise(oY = sum(oC * cW))

  rby <-
    catchtable(fit) %>% lh() %>% dplyr::select(year, pY = Estimate) %>%  mutate(pY = pY/scale) %>%
    full_join(ssbtable(fit) %>% lh() %>% dplyr::select(year, ssb = Estimate) %>% mutate(ssb = ssb/scale)) %>%
    full_join(tsbtable(fit) %>% lh() %>% dplyr::select(year, tsb = Estimate) %>% mutate(tsb = tsb/scale)) %>%
    full_join(fbartable(fit) %>% lh() %>% dplyr::select(year, fbar = Estimate)) %>%
    full_join(rectable(fit) %>% lh() %>% dplyr::select(year, r = Estimate) %>% mutate(r = r/scale)) %>%
    full_join(oY) %>%
    tbl_df() %>%
    mutate(model = model)

  u <-
    fishvice::read_lowestoft_survey(paste0(path, "survey2.dat")) %>%
    mutate(age = ifelse(age == -1, NA_integer_, age),
           value = ifelse(value == -1, NA_real_, value))
  ssb <-
    u %>%
    filter(sur %in% "SSB-egg-based-survey") %>%
    dplyr::select(year, oSSB = value)
  rby <-
    rby %>%
    left_join(ssb)
  indices <-
    u %>%
    filter(!sur %in% "SSB-egg-based-survey") %>%
    dplyr::select(year, age, oU = value)
  rbya <-
    rbya %>%
    left_join(indices)

  rbya.p <-
    fishvice::sam_process_error(rbya, plot_it = FALSE)$rbya %>%
    as_tibble() %>%
    mutate(p.b = n.d * cW) %>%
    dplyr::select(year, age, p.m = z.d, p.n = n.d, p.b)

  rbya <- left_join(rbya, rbya.p)


  rbx <- list(rby = rby, rbya = rbya)

  return(rbx)

}

