library(tradepolicy)
library(eflm)
library(bench)

# data ----

ch1_application2 <- agtpa_applications %>%
  select(exporter, importer, pair_id, year, trade, dist, cntg, lang, clny) %>%
  # this filter covers both OLS and PPML
  filter(year %in% seq(1986, 2006, 4)) %>%
  mutate(
    # variables for both OLS and PPML
    exp_year = paste0(exporter, year),
    imp_year = paste0(importer, year),
    year = paste0("log_dist_", year),
    log_trade = log(trade),
    log_dist = log(dist),
    smctry = ifelse(importer != exporter, 0, 1),

    # PPML specific variables
    log_dist_intra = log_dist * smctry,
    intra_pair = ifelse(exporter == importer, exporter, "inter")
  ) %>%
  spread(year, log_dist, fill = 0) %>%
  mutate(across(log_dist_1986:log_dist_2006, ~ .x * (1 - smctry)))

# ols ----

form <- log_trade ~ 0 + log_dist_1986 + log_dist_1990 + log_dist_1994 +
  log_dist_1998 + log_dist_2002 + log_dist_2006 + cntg +
  lang + clny + exp_year + imp_year

fit_ols_lm <- lm(
  form,
  data = filter(ch1_application2, importer != exporter, trade > 0),
  y = FALSE,
  model = FALSE
)

fit_ols_elm <- elm(
  form,
  data = filter(ch1_application2, importer != exporter, trade > 0),
  y = FALSE,
  model = FALSE
)

bench_ols <- mark(
  lm(
    form,
    data = filter(ch1_application2, importer != exporter, trade > 0),
    y = FALSE,
    model = FALSE
  )$coefficients,
  elm(
    form,
    data = filter(ch1_application2, importer != exporter, trade > 0),
    y = FALSE,
    model = FALSE
  )$coefficients
)

# ppml ----

form <- trade ~ 0 + log_dist_1986 + log_dist_1990 +
  log_dist_1994 + log_dist_1998 + log_dist_2002 + log_dist_2006 +
  cntg + lang + clny + exp_year + imp_year

fit_ppml_glm <- glm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = filter(ch1_application2, importer != exporter),
  y = FALSE,
  model = FALSE
)

fit_ppml_eglm <- eglm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = filter(ch1_application2, importer != exporter),
  y = FALSE,
  model = FALSE
)

bench_ppml <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = filter(ch1_application2, importer != exporter),
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = filter(ch1_application2, importer != exporter),
    y = FALSE,
    model = FALSE
  )$coefficients
)

# internal distance ----

form <- trade ~ 0 + log_dist_1986 + log_dist_1990 +
  log_dist_1994 + log_dist_1998 + log_dist_2002 + log_dist_2006 +
  cntg + lang + clny + exp_year + imp_year + log_dist_intra

fit_internal_distance_glm <- glm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = ch1_application2,
  y = FALSE,
  model = FALSE
)

fit_internal_distance_eglm <- eglm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = ch1_application2,
  y = FALSE,
  model = FALSE
)

bench_internal_distance <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = ch1_application2,
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = ch1_application2,
    y = FALSE,
    model = FALSE
  )$coefficients
)

# internal distance and home bias ----

form <- trade ~ 0 + log_dist_1986 + log_dist_1990 +
  log_dist_1994 + log_dist_1998 + log_dist_2002 + log_dist_2006 +
  cntg + lang + clny + exp_year + imp_year + log_dist_intra + smctry

fit_home_bias_glm <- glm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = ch1_application2,
  y = FALSE,
  model = FALSE
)

fit_home_bias_eglm <- eglm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = ch1_application2,
  y = FALSE,
  model = FALSE
)

bench_home_bias <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = ch1_application2,
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = ch1_application2,
    y = FALSE,
    model = FALSE
  )$coefficients
)

# fe ----

form <- trade ~ 0 + log_dist_1986 + log_dist_1990 +
  log_dist_1994 + log_dist_1998 + log_dist_2002 + log_dist_2006 +
  cntg + lang + clny + exp_year + imp_year + intra_pair

fit_fe_glm <- glm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = ch1_application2,
  y = FALSE,
  model = FALSE
)

fit_fe_eglm <- eglm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = ch1_application2,
  y = FALSE,
  model = FALSE
)

bench_fe <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = ch1_application2,
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = ch1_application2,
    y = FALSE,
    model = FALSE
  )$coefficients
)

save.image("benchmarks/02-chapter1-distance-puzzle.RData", compress = "xz")
