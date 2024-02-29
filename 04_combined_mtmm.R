###################################################
#
# 4. bring together survey and trace data
#
###################################################

# set-up ------------------------------------------------------------------

library(tidyverse)
library(corrplot)
library(lavaan)
library(viridis)
library(blavaan)
library(shinystan)
#library(loo)

future::plan("multicore")
options(mc.cores = parallel::detectCores())


topics <- c("call", "msg", "phot", "sm", "web")
vars_int <- str_c("sph_", topics)
vars_int2 <- c(str_c(topics, "_5"), str_c(topics, "_7"),
               str_c(topics, "_dur_log"),
               str_c(topics, "_count_w1_30d_log"),
               str_c(topics, "_dur_w1_30d_log"))

# survey_full <- read_rds("./data/clean/survey_mtmm_clean.rds")
survey_clean <- read_rds("./data/clean/survey_mtmm_clean.rds")
trace_agg_data2 <- read_rds("./data/clean/trace_agg_data.rds")


data_full <- left_join(
  mutate(survey_clean, new_id = as.numeric(new_id)),
  trace_agg_data2,
  by = "new_id")


vars_int_full <- c(str_c(vars_int, "_5"),
                   str_c(vars_int, "_7"),
                   str_c(vars_int, "_dur_log"))

survey_clean2 <- survey_clean %>%
  rename_at(vars_int_full,
            ~str_remove(., "sph_")) %>%
  mutate(new_id = as.numeric(new_id))

# merge survey and trace data
full_data <- left_join(survey_clean2, trace_agg_data2, by = "new_id")

# select only cases in waves 1 and waves 1b
data_w1_w1b <- full_data %>%
  filter(participation_w1 == "yes" & participation_w1b == "yes")

# rename the variables of interest
mtmm_data <- data_w1_w1b %>%
  select(vars_int2) %>%
  rename_at(vars(matches("w1")),
            ~str_remove(., "_w1_30d_log") %>%
              str_c(., "_d")) %>%
  rename_at(vars(-ends_with("_d")),
            ~str_remove(., "_log") %>%
              str_c(., "_s")) %>%
  rename_all(~str_remove(., "sph_")) %>%
  select(order(names(.)))


# rescale all vars to 0/1

mtmm_data_rescale <- mtmm_data %>%
  mutate_all(~(. - min(., na.rm = T)) /
               (max(., na.rm = T) - min(., na.rm = T)))

summary(mtmm_data_rescale)

# explore data ------------------------------------------------------------

# explore survey data


png(
  height = 1800,
  width = 1800,
  res = 300,
  file = "./output/fig/corplot_survey2.png",
  type = "cairo"
)

mtmm_data %>%
  select(ends_with("_s")) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot(tl.cex = 0.7) %>%
  recordPlot()

dev.off()


# explore digital survey data

png(
  height = 1800,
  width = 1800,
  res = 300,
  file = "./output/fig/corplot_digital.png",
  type = "cairo"
)

mtmm_data %>%
  select(ends_with("_d")) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot(tl.cex = 0.7)

dev.off()


png(
  height = 1800,
  width = 1800,
  res = 300,
  file = "./output/fig/corplot_all.png",
  type = "cairo"
)

new_order <- mtmm_data %>%
  names() %>%
  cbind() %>%
  as_tibble() %>%
  setNames("nms") %>%
  mutate(source = str_sub(nms, -1),
         source_number = ifelse(source == "d", 2, 1),
         topic = str_sub(nms, 1, 3),
         order = row_number()) %>%
  arrange(topic, source_number)

mtmm_data[new_order$order] %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot(tl.cex = 0.7)

dev.off()



# mtmm model --------------------------------------------------------------


# MTMM -
mtmm <- "T_call =~ 1*call_5_s + 1*call_7_s + 1*call_dur_s +
                    1*call_count_d + 1*call_dur_d
        T_msg =~ 1*msg_5_s + 1*msg_7_s + 1*msg_dur_s +
                    1*msg_count_d + 1*msg_dur_d
        T_phot =~ 1*phot_5_s + 1*phot_7_s + 1*phot_dur_s +
                    1*phot_count_d + 1*phot_dur_d
        T_sm =~ 1*sm_5_s + 1*sm_7_s + 1*sm_dur_s +
                    1*sm_count_d + 1*sm_dur_d
        T_web =~ 1*web_5_s + 1*web_7_s + 1*web_dur_s +
                    1*web_count_d + 1*web_dur_d

        M_5 =~ 1*call_5_s + 1*msg_5_s + 1*phot_5_s + 1*sm_5_s + 1*web_5_s
        M_7 =~ 1*call_7_s + 1*msg_7_s + 1*phot_7_s + 1*sm_7_s + 1*web_7_s
        M_dur =~ 1*call_dur_s + 1*msg_dur_s + 1*phot_dur_s
              + 1*sm_dur_s + 1*web_dur_s

        M_count =~ 1*call_count_d + 1*msg_count_d + 1*phot_count_d
              + 1*sm_count_d + 1*web_count_d
        M_ddur =~ 1*call_dur_d + 1*msg_dur_d + 1*phot_dur_d
              + 1*sm_dur_d + 1*web_dur_d

        # estimate variances of latent variables
        M_5 ~~ NA*M_5
        M_7 ~~ NA*M_7
        M_dur ~~ NA*M_dur
        M_ddur ~~ NA*M_ddur
        M_count ~~ NA*M_count

        T_call ~~ NA*T_call
        T_msg ~~ NA*T_msg
        T_phot ~~ NA*T_phot
        T_sm ~~ NA*T_sm
        T_web ~~ NA*T_web

        # fix intercepts and estimate means
        call_5_s + call_7_s + call_dur_s + call_count_d + call_dur_d ~ 0*0
        msg_5_s + msg_7_s + msg_dur_s + msg_count_d + msg_dur_d ~ 0*0
        phot_5_s + phot_7_s + phot_dur_s + phot_count_d + phot_dur_d ~ 0*0
        sm_5_s + sm_7_s + sm_dur_s + sm_count_d + sm_dur_d ~ 0*0
        web_5_s + web_7_s + web_dur_s + web_count_d + web_dur_d ~ 0*0
        T_call + T_msg + T_phot + T_sm + T_web + M_5 + M_7 + M_dur + M_ddur + M_count~ NA*0

        # no correlations between methods
        M_5 ~~ 0*M_7
        M_5 ~~ 0*M_dur
        M_5 ~~ 0*M_ddur
        M_5 ~~ 0*M_count
        M_7 ~~ 0*M_dur
        M_7 ~~ 0*M_ddur
        M_7 ~~ 0*M_count
        M_dur ~~ 0*M_ddur
        M_dur ~~ 0*M_count
        M_ddur ~~ 0*M_count

        # no correlations between methods and traits
        M_5 ~~ 0*T_call
        M_5 ~~ 0*T_msg
        M_5 ~~ 0*T_phot
        M_5 ~~ 0*T_sm
        M_5 ~~ 0*T_web

        M_7 ~~ 0*T_call
        M_7 ~~ 0*T_msg
        M_7 ~~ 0*T_phot
        M_7 ~~ 0*T_sm
        M_7 ~~ 0*T_web

        M_dur ~~ 0*T_call
        M_dur ~~ 0*T_msg
        M_dur ~~ 0*T_phot
        M_dur ~~ 0*T_sm
        M_dur ~~ 0*T_web

        M_ddur ~~ 0*T_call
        M_ddur ~~ 0*T_msg
        M_ddur ~~ 0*T_phot
        M_ddur ~~ 0*T_sm
        M_ddur ~~ 0*T_web

        M_count ~~ 0*T_call
        M_count ~~ 0*T_msg
        M_count ~~ 0*T_phot
        M_count ~~ 0*T_sm
        M_count ~~ 0*T_web"


# shorter default run
# fit_MTMM <- bcfa(mtmm,
#                 data = mtmm_data_rescale,
#                 std.lv = TRUE,
#                 missing = "ml",
#                 auto.fix.first = FALSE,
#                 auto.var = TRUE)


# save(fit_MTMM, file = "./output/MTMM_overall.RData")
load("./output/MTMM_overall.RData")

lavaan::fitmeasures(fit_MTMM)
summary(fit_MTMM, standardized = TRUE)


#
#
#
#
# fit_MTMM_long <- bcfa(mtmm,
#                  data = mtmm_data_rescale,
#                  std.lv = TRUE,
#                  missing = "ml",
#                  auto.fix.first = FALSE,
#                  auto.var = TRUE,
#                  n.chains = 8,
#                  burnin = 2000,
#                  sample = 2000)
#
# lavaan::fitmeasures(fit_MTMM_long)
# summary(fit_MTMM_long, standardized = TRUE)
#
# save(fit_MTMM_long, file = "./output/MTMM_overall_longrun.RData")
load("./output/MTMM_overall_longrun.RData")


# Calculating the validity and reliability for  using its trait and
# factor loading

# get_qual <- function(model, variable) {
#
#   # Showing validity and method effects for each item
#   std_basic <- standardizedsolution(model) %>%
#     filter(str_detect(lhs, "T|M"), op == "=~")
#
#   lambda <- std_basic %>%
#     filter(rhs == variable,  str_detect(lhs, "T")) %>%
#     .$est.std
#
#   gamma <- std_basic %>%
#     filter(rhs == variable, str_detect(lhs, "M")) %>%
#     .$est.std
#
#   r <- sqrt(lambda^2 + gamma^2)
#
#   v <- lambda / r
#
#   m <- gamma / r
#
#   c(r = r, v = v, m = m)
#
# }
#
# qual_mtmm <- map_df(names(mtmm_data),
#                     get_qual,
#                     model = fit_MTMM_long) %>%
#   mutate(var = names(mtmm_data))





mtmm_fit_est <- lavaan::parameterestimates(fit_MTMM_long, standardized = T)

mtmm_est_qual <- mtmm_fit_est %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "T_|M_"))) %>%
  mutate(Trait = case_when(
    str_detect(rhs, "call") ~ "Call",
    str_detect(rhs, "msg") ~ "Messaging",
    str_detect(rhs, "phot") ~ "Photos",
    str_detect(rhs, "sm") ~ "Social Media",
    str_detect(rhs, "web") ~ "Web browsing",
  ),
  group = case_when(
    str_detect(rhs, "5") ~ "5 point scale",
    str_detect(rhs, "7") ~ "7 point scale",
    str_detect(rhs, "dur_s") ~ "Duration survey",
    str_detect(rhs, "dur_d") ~ "Duration digital",
    str_detect(rhs, "count_d") ~ "Count digital"),
  group = fct_relevel(group, "5 point scale", "7 point scale",
                      "Duration survey"),
  source = case_when(str_detect(lhs, "T") ~ "Trait",
                     str_detect(lhs, "M") ~ "Method",
                     TRUE ~ "Random error"),
  qual = ifelse(source != "Random error", std.all^2, std.all)) %>%
  select(-op, -est, -std.lv, -std.nox)


mtmm_est_qual %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  ggplot(aes(group, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Question type",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))


ggsave("./output/fig/mtmm_qual_overall_long.png")





mtmm_est_qual %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  group_by(group, source) %>%
  summarise(group_qual = sum(qual)/5) %>%
  ggplot(aes(group, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
#  facet_wrap(~Trait, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Measurement type",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))

ggsave("./output/fig/mtmm_qual_method_long.png")


mtmm_est_qual %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  group_by(Trait, source) %>%
  summarise(group_qual = sum(qual)/5) %>%
  ggplot(aes(Trait, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  #  facet_wrap(~Trait, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Topic",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))
ggsave("./output/fig/mtmm_qual_topic_long.png")


summary(fit_MTMM_long)
blavInspect(fit_MTMM_long, 'rhat')
blavInspect(fit_MTMM_long, 'neff')
fitMeasures(fit_MTMM_long)
plot(fit_MTMM_long, 5:10, "trace")
plot(fit_MTMM_long, 11:16, "trace")


## Sensitivity analysis -----

### Try ML run with no fixed loadings ---------------


mtmm_nofix <- "T_call =~ 1*call_5_s + call_7_s + call_dur_s +
                    call_count_d + call_dur_d
        T_msg =~ 1*msg_5_s + msg_7_s + msg_dur_s +
                    msg_count_d + msg_dur_d
        T_phot =~ 1*phot_5_s + phot_7_s + phot_dur_s +
                    phot_count_d + phot_dur_d
        T_sm =~ 1*sm_5_s + sm_7_s + sm_dur_s +
                    sm_count_d + sm_dur_d
        T_web =~ 1*web_5_s + web_7_s + web_dur_s +
                    web_count_d + web_dur_d

        M_5 =~ 1*call_5_s + msg_5_s + phot_5_s + sm_5_s + web_5_s
        M_7 =~ 1*call_7_s + msg_7_s + phot_7_s + sm_7_s + web_7_s
        M_dur =~ 1*call_dur_s + msg_dur_s + phot_dur_s
              + sm_dur_s + web_dur_s

        M_count =~ 1*call_count_d + msg_count_d + phot_count_d
              + sm_count_d + web_count_d
        M_ddur =~ 1*call_dur_d + msg_dur_d + phot_dur_d
              + sm_dur_d + web_dur_d

        # estimate variances of latent variables
        M_5 ~~ NA*M_5
        M_7 ~~ NA*M_7
        M_dur ~~ NA*M_dur
        M_ddur ~~ NA*M_ddur
        M_count ~~ NA*M_count

        T_call ~~ NA*T_call
        T_msg ~~ NA*T_msg
        T_phot ~~ NA*T_phot
        T_sm ~~ NA*T_sm
        T_web ~~ NA*T_web

        # no correlations between methods
        M_5 ~~ M_7
        M_5 ~~ M_dur
        M_5 ~~ 0*M_ddur
        M_5 ~~ 0*M_count
        M_7 ~~ M_dur
        M_7 ~~ 0*M_ddur
        M_7 ~~ 0*M_count
        M_dur ~~ 0*M_ddur
        M_dur ~~ 0*M_count
        M_ddur ~~ M_count

        # no correlations between methods and traits
        M_5 ~~ 0*T_call
        M_5 ~~ 0*T_msg
        M_5 ~~ 0*T_phot
        M_5 ~~ 0*T_sm
        M_5 ~~ 0*T_web

        M_7 ~~ 0*T_call
        M_7 ~~ 0*T_msg
        M_7 ~~ 0*T_phot
        M_7 ~~ 0*T_sm
        M_7 ~~ 0*T_web

        M_dur ~~ 0*T_call
        M_dur ~~ 0*T_msg
        M_dur ~~ 0*T_phot
        M_dur ~~ 0*T_sm
        M_dur ~~ 0*T_web

        M_ddur ~~ 0*T_call
        M_ddur ~~ 0*T_msg
        M_ddur ~~ 0*T_phot
        M_ddur ~~ 0*T_sm
        M_ddur ~~ 0*T_web

        M_count ~~ 0*T_call
        M_count ~~ 0*T_msg
        M_count ~~ 0*T_phot
        M_count ~~ 0*T_sm
        M_count ~~ 0*T_web

        msg_count_d ~~  msg_dur_d"



# msg_count_d ~~  msg_dur_d

# improvements
# - add data source specific correlation
# - free loadings
# ?- add correlated errors for msg_count_d ~~  msg_dur_d (otherwise you get a negative)


# ML estimation
fit_MTMM_nofix_ml <- cfa(mtmm_nofix,
                data = mtmm_data_rescale,
                std.lv = TRUE,
                missing = "ml",
                auto.fix.first = FALSE,
                auto.var = TRUE)

# save(fit_MTMM_nofix_ml, file = "./output/fit_MTMM_nofix_ml.RData")
load("./output/fit_MTMM_nofix_ml.RData")

# Bayesian estimation
# fit_MTMM_nofix <- bcfa(mtmm_nofix,
#                 data = mtmm_data_rescale,
#                 std.lv = TRUE,
#                 missing = "ml",
#                 auto.fix.first = FALSE,
#                 auto.var = TRUE,
#                 n.chains = 8,
#                 burnin = 2000,
#                 sample = 2000)


# save(fit_MTMM_nofix, file = "./output/fit_MTMM_nofix.RData")
load("./output/fit_MTMM_nofix.RData")




summary(fit_MTMM_nofix_ml, standardized = TRUE)


lavaan::fitmeasures(fit_MTMM_nofix_ml)
modificationindices(fit_MTMM_nofix_ml, minimum.value = 50) %>%
  arrange(mi)








mtmm_fit_est2 <- lavaan::parameterestimates(fit_MTMM_nofix_ml,
                                            standardized = T)

write.csv(mtmm_fit_est2, file = "./output/tab/mtmm_ml_coefs.csv")

mtmm_est_qual2 <- mtmm_fit_est2 %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "T_|M_"))) %>%
  filter(!(lhs == "msg_count_d" & rhs == "msg_dur_d")) %>%
  mutate(Trait = case_when(
    str_detect(rhs, "call") ~ "Call",
    str_detect(rhs, "msg") ~ "Messaging",
    str_detect(rhs, "phot") ~ "Photos",
    str_detect(rhs, "sm") ~ "Social Media",
    str_detect(rhs, "web") ~ "Web browsing",
  ),
  group = case_when(
    str_detect(rhs, "5") ~ "5 point scale",
    str_detect(rhs, "7") ~ "7 point scale",
    str_detect(rhs, "dur_s") ~ "Duration survey",
    str_detect(rhs, "dur_d") ~ "Duration digital",
    str_detect(rhs, "count_d") ~ "Count digital"),
  group = fct_relevel(group, "5 point scale", "7 point scale",
                      "Duration survey"),
  source = case_when(str_detect(lhs, "T") ~ "Trait",
                     str_detect(lhs, "M") ~ "Method",
                     TRUE ~ "Random error"),
  qual = ifelse(source != "Random error", std.all^2, std.all)) %>%
  select(-op, -est, -std.lv, -std.nox)


mtmm_est_qual2 %>%
  group_by(source) %>%
  summarise(qual = mean(qual))

mtmm_est_qual2 %>%
  group_by(source, group) %>%
  summarise(qual = mean(qual))

mtmm_est_qual2 %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  ggplot(aes(group, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Question type",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))



ggsave("./output/fig/mtmm_qual_overall_long_ml.png")


mtmm_est_qual2 %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  group_by(group, source) %>%
  summarise(group_qual = sum(qual)/5) %>%
  ggplot(aes(group, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  #  facet_wrap(~Trait, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Measurement type",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))



ggsave("./output/fig/mtmm_qual_method_long_ml.png")

mtmm_est_qual2 %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  group_by(Trait, source) %>%
  summarise(group_qual = sum(qual)/5) %>%
  ggplot(aes(Trait, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  #  facet_wrap(~Trait, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Topic",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))

ggsave("./output/fig/mtmm_qual_topic_long_ml.png")


## data source specific models ----

### survey mtmm -------

mtmm_survey <- "T_call =~ 1*call_5_s + call_7_s + call_dur_s
        T_msg =~ 1*msg_5_s + msg_7_s + msg_dur_s
        T_phot =~ 1*phot_5_s + phot_7_s + phot_dur_s
        T_sm =~ 1*sm_5_s + sm_7_s + sm_dur_s
        T_web =~ 1*web_5_s + web_7_s + web_dur_s

        M_5 =~ 1*call_5_s + msg_5_s + phot_5_s + sm_5_s + web_5_s
        M_7 =~ 1*call_7_s + msg_7_s + phot_7_s + sm_7_s + web_7_s
        M_dur =~ 1*call_dur_s + msg_dur_s + phot_dur_s
              + sm_dur_s + web_dur_s

        # estimate variances of latent variables
        M_5 ~~ NA*M_5
        M_7 ~~ NA*M_7
        M_dur ~~ NA*M_dur


        T_call ~~ NA*T_call
        T_msg ~~ NA*T_msg
        T_phot ~~ NA*T_phot
        T_sm ~~ NA*T_sm
        T_web ~~ NA*T_web

        # no correlations between methods
       M_5 ~~ 0*M_7
       M_5 ~~ 0*M_dur
       M_7 ~~ 0*M_dur


        # no correlations between methods and traits
        M_5 ~~ 0*T_call
        M_5 ~~ 0*T_msg
        M_5 ~~ 0*T_phot
        M_5 ~~ 0*T_sm
        M_5 ~~ 0*T_web

        M_7 ~~ 0*T_call
        M_7 ~~ 0*T_msg
        M_7 ~~ 0*T_phot
        M_7 ~~ 0*T_sm
        M_7 ~~ 0*T_web

        M_dur ~~ 0*T_call
        M_dur ~~ 0*T_msg
        M_dur ~~ 0*T_phot
        M_dur ~~ 0*T_sm
        M_dur ~~ 0*T_web
"


# shorter default run
# model works with mtmm-1 but not with all 3 using ML
# quality varies considerably depending on the reference method chosen
# try using Bayesian
# fit_MTMM_survey <- bcfa(mtmm_survey,
#                       data = mtmm_data_rescale,
#                       std.lv = TRUE,
#                       missing = "ml",
#                       auto.fix.first = FALSE,
#                       auto.var = TRUE)

# save(fit_MTMM_survey, file = "./output/MTMM_survey.RData")
load("./output/MTMM_survey.RData")


summary(fit_MTMM_survey, standardized = TRUE)


lavaan::fitmeasures(fit_MTMM_survey)
modificationindices(fit_MTMM_survey, minimum.value = 50) %>%
  arrange(mi)


# loo::pareto_k_table(fit_MTMM_survey)

blavInspect(fit_MTMM_survey, "rhat")

# should be below 1.05
max(blavInspect(fit_MTMM_survey, "psrf"))

# should be at least 300
min(blavInspect(fit_MTMM_survey, "neff"))

blavFitIndices(fit_MTMM_survey)

plot(fit_MTMM_survey, pars = 1:4, plot.type = "trace")

#
# loo1 <- loo::loo(fit_MTMM_survey, save_psis = TRUE)
# coda::gelman.diag(fit_MTMM_survey)

mtmm_fit_est_survey <- lavaan::parameterestimates(fit_MTMM_survey,
                                            standardized = T)

mtmm_est_qual_survey <- mtmm_fit_est_survey %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "T_|M_"))) %>%
  mutate(Trait = case_when(
    str_detect(rhs, "call") ~ "Call",
    str_detect(rhs, "msg") ~ "Messaging",
    str_detect(rhs, "phot") ~ "Photos",
    str_detect(rhs, "sm") ~ "Social Media",
    str_detect(rhs, "web") ~ "Web browsing",
  ),
  group = case_when(
    str_detect(rhs, "5") ~ "5 point scale",
    str_detect(rhs, "7") ~ "7 point scale",
    str_detect(rhs, "dur_s") ~ "Duration survey"),
  group = fct_relevel(group, "5 point scale", "7 point scale",
                      "Duration survey"),
  source = case_when(str_detect(lhs, "T") ~ "Trait",
                     str_detect(lhs, "M") ~ "Method",
                     TRUE ~ "Random error"),
  qual = ifelse(source != "Random error", std.all^2, std.all)) %>%
  select(-op, -est, -std.lv, -std.nox)


mtmm_est_qual_survey %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  ggplot(aes(group, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Question type",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))




mtmm_est_qual_survey %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  group_by(group, source) %>%
  summarise(group_qual = sum(qual)/5) %>%
  ggplot(aes(group, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  #  facet_wrap(~Trait, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Measurement type",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))
ggsave("./output/fig/mtmm_qual_method_long_survey.png")

mtmm_est_qual_survey %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  group_by(Trait, source) %>%
  summarise(group_qual = sum(qual)/3) %>%
  ggplot(aes(Trait, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  #  facet_wrap(~Trait, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Topic",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))

### mtmm digital trace data ----

mtmm_trace <- "T_call =~ 1*call_count_d + 1*call_dur_d
        T_msg =~ 1*msg_count_d + 1*msg_dur_d
        T_phot =~ 1*phot_count_d + 1*phot_dur_d
        T_sm =~ 1*sm_count_d + 1*sm_dur_d
        T_web =~ 1*web_count_d + 1*web_dur_d

        M_count =~ 1*call_count_d + msg_count_d + phot_count_d
              + sm_count_d + web_count_d
        # M_ddur =~ 1*call_dur_d + 1*msg_dur_d + 1*phot_dur_d
        #       + 1*sm_dur_d + 1*web_dur_d

        # estimate variances of latent variables
        # M_ddur ~~ NA*M_ddur
        M_count ~~ NA*M_count

        T_call ~~ NA*T_call
        T_msg ~~ NA*T_msg
        T_phot ~~ NA*T_phot
        T_sm ~~ NA*T_sm
        T_web ~~ NA*T_web

        # no correlations between methods
#        M_ddur ~~ 0*M_count

        # no correlations between methods and traits
        # M_ddur ~~ 0*T_call
        # M_ddur ~~ 0*T_msg
        # M_ddur ~~ 0*T_phot
        # M_ddur ~~ 0*T_sm
        # M_ddur ~~ 0*T_web

        M_count ~~ 0*T_call
        M_count ~~ 0*T_msg
        M_count ~~ 0*T_phot
        M_count ~~ 0*T_sm
        M_count ~~ 0*T_web"


# some small negative variances even with mtmm-1.
# see if bayesian estimation heslp with that
# fit_MTMM_trace <- bcfa(mtmm_trace,
#                       data = mtmm_data_rescale,
#                       std.lv = TRUE,
#                       missing = "ml",
#                       auto.fix.first = FALSE,
#                       auto.var = TRUE)


# save(fit_MTMM_trace, file = "./output/MTMM_trace.RData")
load("./output/MTMM_trace.RData")

# em.iter.max = 20000

summary(fit_MTMM_trace, standardized = TRUE)


lavaan::fitmeasures(fit_MTMM_trace)
modificationindices(fit_MTMM_trace, minimum.value = 50) %>%
  arrange(mi)








mtmm_fit_est_trace <- lavaan::parameterestimates(fit_MTMM_trace,
                                            standardized = T)

mtmm_est_qual_trace <- mtmm_fit_est_trace %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "T_|M_"))) %>%
  mutate(Trait = case_when(
    str_detect(rhs, "call") ~ "Call",
    str_detect(rhs, "msg") ~ "Messaging",
    str_detect(rhs, "phot") ~ "Photos",
    str_detect(rhs, "sm") ~ "Social Media",
    str_detect(rhs, "web") ~ "Web browsing",
  ),
  group = case_when(
    str_detect(rhs, "dur_d") ~ "Duration digital",
    str_detect(rhs, "count_d") ~ "Count digital"),
  source = case_when(str_detect(lhs, "T") ~ "Trait",
                     str_detect(lhs, "M") ~ "Method",
                     TRUE ~ "Random error"),
  qual = ifelse(source != "Random error", std.all^2, std.all)) %>%
  select(-op, -est, -std.lv, -std.nox)


mtmm_est_qual_trace %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  ggplot(aes(group, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Question type",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))




mtmm_est_qual_trace %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  group_by(group, source) %>%
  summarise(group_qual = sum(qual)/5) %>%
  ggplot(aes(group, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  #  facet_wrap(~Trait, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Measurement type",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))

ggsave("./output/fig/mtmm_qual_method_long_digital.png")

mtmm_est_qual_trace %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  group_by(Trait, source) %>%
  summarise(group_qual = sum(qual)/5) %>%
  ggplot(aes(Trait, group_qual, fill = source)) +
  geom_bar(stat = "identity") +
  #  facet_wrap(~Trait, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Topic",
       fill = "Source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45,
                                   vjust = 0.5,
                                   hjust = 0.1))


blavInspect(fit_MTMM_trace, "rhat")

# should be below 1.05
max(blavInspect(fit_MTMM_trace, "psrf"))

# should be at least 300
min(blavInspect(fit_MTMM_trace, "neff"))

blavFitIndices(fit_MTMM_trace)

plot(fit_MTMM_trace,plot.type = "trace")


