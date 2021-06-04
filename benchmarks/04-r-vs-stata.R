library(tidyverse)
library(readxl)

tidy_benchmarks <- read_excel("data/tidy benchmarks.xlsx") %>%
  select(-expr, -neval) %>%
  gather(Statistic, `Time (Seconds)`, -Hardware, -Function) %>%
  filter(!grepl("2 dedicated|SSD", Hardware)) %>%
  mutate(
    Hardware = gsub(", NVMe disk| dedicated", "", Hardware),
    Statistic = as_factor(Statistic),
    Statistic = fct_relevel(Statistic, "Min", "LQ", "Median",
                            "Mean", "UQ", "Max")
  )

tidy_benchmarks <- tidy_benchmarks %>% 
  filter(Hardware == "8 CPUs, 16 GB RAM", Statistic == "Median") %>%  
  select(Function, `Time (Seconds)`) %>% 
  mutate(Software = "R")

stata_bench <- read_excel("data/stata_bench.xlsx") %>% 
  group_by(command) %>% 
  filter(Obs == max(Obs)) %>% 
  mutate(Function = ifelse(grepl("glm", command), "Stata REG", "Stata GLM")) %>% 
  filter(Function != "Stata REG") %>% 
  rename(`Time (Seconds)` =  Time) %>% 
  ungroup() %>% 
  select(Function, `Time (Seconds)`) %>% 
  mutate(Software = "Stata")

tidy_benchmarks2 <- bind_rows(tidy_benchmarks, stata_bench)

g <- ggplot(tidy_benchmarks2) + 
  geom_col(aes(x = Function, y = `Time (Seconds)`, fill = Software), position = "dodge2") + 
  scale_color_manual(values = c("#3B9AB2", "#E1AF00")) + 
  labs(title = "R vs Stata Fitting Time - Benchmark Results") + 
  theme_minimal() + 
  theme(legend.position = "none")

saveRDS(g, "benchmarks/04-stata-vs-r.rda")
