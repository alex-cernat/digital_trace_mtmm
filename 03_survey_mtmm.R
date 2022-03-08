# mtmm analysis


# set-up ------------------------------------------------------------------

library(tidyverse)
library(corrplot)
library(lavaan)
library(blavaan)

future::plan("multicore")
options(mc.cores = parallel::detectCores())


topics <- c("call", "msg", "phot", "sm", "web")
vars_int <- str_c("sph_", topics)


survey4 <- read_rds("./data/clean/survey_mtmm_clean.rds")

vars_int_full <- c(str_c(vars_int, "_5"),
                   str_c(vars_int, "_7"),
                   str_c(vars_int, "_dur_log"))

survey5 <- survey4 %>%
  rename_at(vars_int_full,
            ~str_remove(., "sph_"))

# explore -----------------------------------------------------------------


make_cross_tab <- function(data, var, prop = T) {

  tab <- table(data[[str_c(var, "_7_fct")]],
               data[[str_c(var, "_5_fct")]],
               useNA = "always")

  if (prop == T) {
   tab <- tab %>%
     prop.table(2) %>%
     round(2)
    }

  tab
}


# tables categorical vars
map(vars_int, make_cross_tab, data = survey4)

# descriptives of time vars
map(select(survey4, str_c(vars_int, "_dur_log")), qplot)

# corrplot
survey4 %>%
  select(vars_int_full) %>%
  rename_all(~str_remove(., "sph_")) %>%
  select(order(names(.))) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot()



# mtmm --------------------------------------------------------------------



# MTMM -
mtmm <- "T_call =~ 1*call_5 + call_7 + call_dur_log
        T_msg =~ 1*msg_5 + msg_7 + msg_dur_log
        T_phot =~ 1*phot_5 + phot_7 + phot_dur_log
        T_sm =~ 1*sm_5 + sm_7 + sm_dur_log
        T_web =~ 1*web_5 + web_7 + web_dur_log

        M5 =~ 1*call_5 + 1*msg_5 + 1*phot_5 + 1*sm_5 + 1*web_5
        M7 =~ 1*call_7 + 1*msg_7 + 1*phot_7 + 1*sm_7 + 1*web_7
        Mdur =~ 1*call_dur_log + 1*msg_dur_log + 1*phot_dur_log
              + 1*sm_dur_log + 1*web_dur_log

        # estimate variances of latent variables
        M5 ~~ NA*M5
        M7 ~~ NA*M7
        Mdur ~~ NA*Mdur

        T_call ~~ NA*T_call
        T_msg ~~ NA*T_msg
        T_phot ~~ NA*T_phot
        T_sm ~~ NA*T_sm
        T_web ~~ NA*T_web

        # fix intercepts and estimate means
        call_5 + call_7 + call_dur_log ~ 0*0
        msg_5 + msg_7 + msg_dur_log ~ 0*0
        phot_5 + phot_7 + phot_dur_log ~ 0*0
        sm_5 + sm_7 + sm_dur_log ~ 0*0
        web_5 + web_7 + web_dur_log ~ 0*0
        T_call + T_msg + T_phot + T_sm + T_web + M5 + M7 + Mdur ~ NA*0

        # no correlations between methods
        M5 ~~ 0*M7
        M5 ~~ 0*Mdur
        M7 ~~ 0*Mdur

        # no correlations between methods and traits
        M5 ~~ 0*T_call
        M5 ~~ 0*T_msg
        M5 ~~ 0*T_phot
        M5 ~~ 0*T_sm
        M5 ~~ 0*T_web

        M7 ~~ 0*T_call
        M7 ~~ 0*T_msg
        M7 ~~ 0*T_phot
        M7 ~~ 0*T_sm
        M7 ~~ 0*T_web


        Mdur ~~ 0*T_call
        Mdur ~~ 0*T_msg
        Mdur ~~ 0*T_phot
        Mdur ~~ 0*T_sm
        Mdur ~~ 0*T_web"

fit_MTMM <- cfa(mtmm,
                data = survey5,
                std.lv = TRUE,
                missing = "ml",
                auto.fix.first = FALSE,
                auto.var = TRUE)

lavaan::fitmeasures(fit_MTMM)
summary(fit_MTMM, standardized = TRUE)


# Calculating the validity and reliability for  using its trait and
# factor loading

get_qual <- function(model, variable) {

  # Showing validity and method effects for each item
  std_basic <- standardizedsolution(model) %>%
    filter(str_detect(lhs, "T|M"), op == "=~")

  lambda <- std_basic %>%
    filter(rhs == variable,  str_detect(lhs, "T")) %>%
    .$est.std

  gamma <- std_basic %>%
    filter(rhs == variable, str_detect(lhs, "M")) %>%
    .$est.std

  r <- sqrt(lambda^2 + gamma^2)

  v <- lambda / r

  m <- gamma / r

  c(r = r, v = v, m = m)

}

qual_mtmm <- map_df(str_remove(vars_int_full, "sph_"),
                    get_qual,
                    model = fit_MTMM) %>%
  mutate(var = str_remove(vars_int_full, "sph_"))





mtmm_fit_est <- lavaan::parameterestimates(fit_MTMM, standardized = T)

mtmm_est_qual <- mtmm_fit_est %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "T_|M"))) %>%
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
           str_detect(rhs, "log") ~ "Duration"),
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


ggsave("./output/fig/mtmm_qual_overall.png", height = 8)



mtmm <- "T_call =~ 1*call_5 + 1*call_7 + 1*call_dur_log
        T_msg =~ 1*msg_5 + 1*msg_7 + 1*msg_dur_log
        T_phot =~ 1*phot_5 + 1*phot_7 + 1*phot_dur_log
        T_sm =~ 1*sm_5 + 1*sm_7 + 1*sm_dur_log
        T_web =~ 1*web_5 + 1*web_7 + 1*web_dur_log

        M5 =~ 1*call_5 + 1*msg_5 + 1*phot_5 + 1*sm_5 + 1*web_5
        M7 =~ 1*call_7 + 1*msg_7 + 1*phot_7 + 1*sm_7 + 1*web_7
        Mdur =~ 1*call_dur_log + 1*msg_dur_log + 1*phot_dur_log
              + 1*sm_dur_log + 1*web_dur_log

        # estimate variances of latent variables
        M5 ~~ NA*M5
        M7 ~~ NA*M7
        Mdur ~~ NA*Mdur

        T_call ~~ NA*T_call
        T_msg ~~ NA*T_msg
        T_phot ~~ NA*T_phot
        T_sm ~~ NA*T_sm
        T_web ~~ NA*T_web

        # fix intercepts and estimate means
        call_5 + call_7 + call_dur_log ~ 0*0
        msg_5 + msg_7 + msg_dur_log ~ 0*0
        phot_5 + phot_7 + phot_dur_log ~ 0*0
        sm_5 + sm_7 + sm_dur_log ~ 0*0
        web_5 + web_7 + web_dur_log ~ 0*0
        T_call + T_msg + T_phot + T_sm + T_web + M5 + M7 + Mdur ~ NA*0

        # no correlations between methods
        M5 ~~ 0*M7
        M5 ~~ 0*Mdur
        M7 ~~ 0*Mdur

        # no correlations between methods and traits
        M5 ~~ 0*T_call
        M5 ~~ 0*T_msg
        M5 ~~ 0*T_phot
        M5 ~~ 0*T_sm
        M5 ~~ 0*T_web

        M7 ~~ 0*T_call
        M7 ~~ 0*T_msg
        M7 ~~ 0*T_phot
        M7 ~~ 0*T_sm
        M7 ~~ 0*T_web


        Mdur ~~ 0*T_call
        Mdur ~~ 0*T_msg
        Mdur ~~ 0*T_phot
        Mdur ~~ 0*T_sm
        Mdur ~~ 0*T_web"

fit_MTMM2 <- bcfa(mtmm,
                data = survey5,
                std.lv = TRUE,
                missing = "ml",
                auto.fix.first = FALSE,
                auto.var = TRUE)

lavaan::fitmeasures(fit_MTMM2)
summary(fit_MTMM2, standardized = TRUE)



save(fit_MTMM2, file = "./data/clean/mtmm_survey_bayes1.RData")



mtmm_fit_est2 <- lavaan::parameterestimates(fit_MTMM2, standardized = T)

mtmm_est_qual2 <- mtmm_fit_est2 %>%
  filter(op == "=~" | op == "~~") %>%
  filter(!(op == "~~" & str_detect(lhs, "T_|M"))) %>%
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
    str_detect(rhs, "log") ~ "Duration"),
  source = case_when(str_detect(lhs, "T") ~ "Trait",
                     str_detect(lhs, "M") ~ "Method",
                     TRUE ~ "Random error"),
  qual = ifelse(source != "Random error", std.all^2, std.all)) %>%
  select(-op, -est, -std.lv, -std.nox)


mtmm_est_qual2 %>%
  mutate(source = fct_relevel(source, "Trait")) %>%
  ggplot(aes(group, qual, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Trait, nrow = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  labs(y = "Proportion variance",
       x = "Question type",
       fill = "Source") +
  theme_bw()
