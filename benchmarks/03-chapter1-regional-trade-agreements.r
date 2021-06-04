library(tradepolicy)
library(eflm)
library(bench)

# data ----

ch1_application3 <- agtpa_applications %>%
  filter(year %in% seq(1986, 2006, 4)) %>%
  mutate(
    exp_year = paste0(exporter, year),
    imp_year = paste0(importer, year),
    year = paste0("intl_border_", year),
    log_trade = log(trade),
    log_dist = log(dist),
    intl_brdr = ifelse(exporter == importer, pair_id, "inter"),
    intl_brdr_2 = ifelse(exporter == importer, 0, 1),
    pair_id_2 = ifelse(exporter == importer, "0-intra", pair_id)
  ) %>%
  spread(year, intl_brdr_2, fill = 0)

ch1_application3 <- ch1_application3 %>%
  group_by(pair_id) %>%
  mutate(sum_trade = sum(trade)) %>%
  ungroup()

# ols ----

form <- log_trade ~ 0 + log_dist + cntg + lang + clny +
  rta + exp_year + imp_year

fit_ols_lm <- lm(
  form,
  data = filter(ch1_application3, importer != exporter, trade > 0),
  y = FALSE,
  model = FALSE
)

fit_ols_elm <- elm(
  form,
  data = filter(ch1_application3, importer != exporter, trade > 0),
  y = FALSE,
  model = FALSE
)

bench_ols <- mark(
  lm(
    form,
    data = filter(ch1_application3, importer != exporter, trade > 0),
    y = FALSE,
    model = FALSE
  )$coefficients,
  elm(
    form,
    data = filter(ch1_application3, importer != exporter, trade > 0),
    y = FALSE,
    model = FALSE
  )$coefficients
)

# ppml ----

form <- trade ~ 0 + log_dist + cntg + lang + clny +
  rta + exp_year + imp_year

fit_ppml_glm <- glm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = filter(ch1_application3, importer != exporter),
  y = FALSE,
  model = FALSE
)

fit_ppml_eglm <- eglm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = filter(ch1_application3, importer != exporter),
  y = FALSE,
  model = FALSE
)

bench_ppml <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = filter(ch1_application3, importer != exporter),
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = filter(ch1_application3, importer != exporter),
    y = FALSE,
    model = FALSE
  )$coefficients
)

# trade diversion ----

form <- trade ~ 0 + log_dist + cntg + lang + clny +
  rta + exp_year + imp_year + intl_brdr

fit_trade_diversion_glm <- glm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = ch1_application3,
  y = FALSE,
  model = FALSE
)

fit_trade_diversion_eglm <- eglm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = ch1_application3,
  y = FALSE,
  model = FALSE
)

bench_trade_diversion <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = ch1_application3,
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = ch1_application3,
    y = FALSE,
    model = FALSE
  )$coefficients
)

# endogeneity ----

form <- trade ~ 0 + rta + exp_year + imp_year + pair_id_2

fit_endogeneity_glm <- glm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = filter(ch1_application3, sum_trade > 0),
  y = FALSE,
  model = FALSE
)

fit_endogeneity_eglm <- eglm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = filter(ch1_application3, sum_trade > 0),
  y = FALSE,
  model = FALSE
)

bench_endogeneity <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = filter(ch1_application3, sum_trade > 0),
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = filter(ch1_application3, sum_trade > 0),
    y = FALSE,
    model = FALSE
  )$coefficients
)

# reverse causality ----

form <- trade ~ 0 + rta + rta_lead4 + exp_year + imp_year + pair_id_2

fit_reverse_causality_glm <- glm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = filter(ch1_application3, sum_trade > 0),
  y = FALSE,
  model = FALSE
)

fit_reverse_causality_eglm <- eglm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = filter(ch1_application3, sum_trade > 0),
  y = FALSE,
  model = FALSE
)

bench_reverse_causality <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = filter(ch1_application3, sum_trade > 0),
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = filter(ch1_application3, sum_trade > 0),
    y = FALSE,
    model = FALSE
  )$coefficients
)

# non-linear/phasing effects ----

form <- trade ~ 0 + rta + rta_lag4 + rta_lag8 + rta_lag12 +
  exp_year + imp_year + pair_id_2

fit_phasing_glm <- glm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = filter(ch1_application3, sum_trade > 0),
  y = FALSE,
  model = FALSE
)

fit_phasing_eglm <- eglm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = filter(ch1_application3, sum_trade > 0),
  y = FALSE,
  model = FALSE
)

bench_phasing <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = filter(ch1_application3, sum_trade > 0),
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = filter(ch1_application3, sum_trade > 0),
    y = FALSE,
    model = FALSE
  )$coefficients
)

# globalization ----

form <- trade ~ 0 + rta + rta_lag4 + rta_lag8 + rta_lag12 +
  intl_border_1986 + intl_border_1990 + intl_border_1994 +
  intl_border_1998 + intl_border_2002 +
  exp_year + imp_year + pair_id_2

fit_globalization_glm <- glm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = filter(ch1_application3, sum_trade > 0),
  y = FALSE,
  model = FALSE
)

fit_globalization_eglm <- eglm(
  form,
  family = stats::quasipoisson(link = "log"),
  data = filter(ch1_application3, sum_trade > 0),
  y = FALSE,
  model = FALSE
)

bench_globalization <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = filter(ch1_application3, sum_trade > 0),
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = filter(ch1_application3, sum_trade > 0),
    y = FALSE,
    model = FALSE
  )$coefficients
)

save.image("benchmarks/03-chapter1-regional-trade-agreements.RData", compress = "xz")
