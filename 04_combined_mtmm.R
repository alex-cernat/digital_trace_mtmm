###################################################
#
# 4. bring together survey and trace data
#
###################################################

# set-up ------------------------------------------------------------------

library(tidyverse)
library(corrplot)
library(lavaan)
library(blavaan)

future::plan("multicore")
options(mc.cores = parallel::detectCores())


topics <- c("call", "msg", "phot", "sm", "web")
vars_int <- str_c("sph_", topics)
vars_int2 <- c(str_c(topics, "_5"), str_c(topics, "_7"),
               str_c(topics, "_dur_log"),
               str_c(topics, "_count_w1b_7d"),
               str_c(topics, "_count_w1b_30d"),
               str_c(topics, "_dur_w1b_7d"),
               str_c(topics, "_dur_w1b_30d"))

survey_clean <- read_rds("./data/clean/survey_mtmm_clean.rds")
trace_agg_data2 <- read_csv("./data/clean/trace_agg_data.csv")



vars_int_full <- c(str_c(vars_int, "_5"),
                   str_c(vars_int, "_7"),
                   str_c(vars_int, "_dur_log"))

survey_clean2 <- survey_clean %>%
  rename_at(vars_int_full,
            ~str_remove(., "sph_")) %>%
  mutate(new_id = as.numeric(new_id))

# merge survey and trace data

full_data <- left_join(survey_clean2, trace_agg_data2, by = "new_id")


full_data %>% select(matches("_dur_")) %>% map(qplot)
  summary()


# explore data ------------------------------------------------------------

full_data %>%
  select(vars_int2) %>%
  rename_all(~str_remove(., "sph_")) %>%
  select(order(names(.))) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot()

