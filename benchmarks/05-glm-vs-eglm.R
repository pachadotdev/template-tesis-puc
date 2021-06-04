library(ggplot2)
library(dplyr)
library(bench)

load("~/tesis-magister/benchmarks/01-chapter1-traditional-gravity.RData")

bench_traditional_gravity <- bind_rows(
  bench_fe %>% mutate(model = "TG, FE", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_ols %>% mutate(model = "TG, OLS", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_ols_remoteness %>% mutate(model = "TG, OLS REM", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_ppml %>% mutate(model = "TG, PPML", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc)
)

rm(list = grep("bench_traditional_gravity", ls(), value = TRUE, invert = TRUE))

load("~/tesis-magister/benchmarks/02-chapter1-distance-puzzle.RData")

bench_distance_puzzle <- bind_rows(
  bench_fe %>% mutate(model = "DP, FE", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_ols %>% mutate(model = "DP, OLS", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_home_bias %>% mutate(model = "DP, HB", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_internal_distance %>% mutate(model = "DP, ID", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_ppml %>% mutate(model = "DP, PPML", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc)
)

rm(list = grep("bench_traditional_gravity|bench_distance_puzzle", ls(), value = TRUE, invert = TRUE))

load("~/tesis-magister/benchmarks/03-chapter1-regional-trade-agreements.RData")

bench_rtas <- bind_rows(
  bench_ols %>% mutate(model = "RTAs, OLS", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_endogeneity %>% mutate(model = "RTAs, EN", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_globalization %>% mutate(model = "RTAs, GB", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_phasing %>% mutate(model = "RTAs, PH", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_ppml %>% mutate(model = "RTAs, PPML", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_reverse_causality %>% mutate(model = "RTAs, RC", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_trade_diversion %>% mutate(model = "RTAs, TD", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc)
)

rm(list = grep("bench_traditional_gravity|bench_distance_puzzle|bench_rtas", ls(), value = TRUE, invert = TRUE))

save.image("benchmarks/05-glm-vs-eglm.RData")
