
# Admin -------------------------------------------------------------------

# load library
library(tidyverse)
library(corrplot)

# load data
survey_raw <- read.csv("./data/raw/survey_allwaves_final.csv")



# Descriptives ------------------------------------------------------------


survey_raw %>%
  select(starts_with("w1b")) %>%
  colnames()


count(survey_raw, w1b_sph)
count(survey_raw, w1b_pc)
count(survey_raw, w1b_pc)


vars_use <- c("w1b_sph", "w1b_pc", "w1b_tabl",
             "w1b_ebk", "w1b_swa", "w1b_fit")

map(vars_use, function(x) table(survey_raw[x]))

count(survey_raw, w1b_sph, w1b_pc)

count(survey_raw, w1b_quest_type_devices, w1b_sph_msg_5)
count(survey_raw, w1b_quest_type_devices, w1b_sph_msg_7)
count(survey_raw, w1b_quest_type_devices, w1b_sph_call_hr)
count(survey_raw, w1b_quest_type_devices, w1b_sph_call_min)

count(survey_raw, w1_quest_type_devices, w1_sph_web_5)
count(survey_raw, w1_quest_type_devices, w1_sph_web_7)
count(survey_raw, w1_quest_type_devices, w1_sph_msg_5)
count(survey_raw, w1_quest_type_devices, w1_sph_msg_7)


count(survey_raw, w1_quest_type_devices, w1_sph_web_hr)



count(survey_raw, w1b_quest_type_devices,
      w1_sph_msg_5, w1b_sph_msg_5)

# clean survey data -------------------------------------------------------

# 7 point scale
# ein- bis zweimal im Monat  = once or twice a month
# ein- bis zweimal in der Woche = once or twice a week
# ein- bis zweimal taglich = once or twice a day
# mehrmals im Monat = several times a month
# mehrmals in der Woche = several times a week
# mehrmals taglich = several times a day
# seltener als einmal im Monat = less than once a month

# 5 point scale
# einmal im Monat oder seltener = once a month or less often
# mehrmals im Monat = several times a month
# mehrmals in der Woche = several times a week
# mehrmals taglich = several times a day
# taglich = every day

survey <- survey_raw %>%
  mutate_all(~str_replace(., "ä|Ã¤", "a")) %>%
  mutate_all(~str_replace(., "Ü|ü", "u")) %>%
  as_tibble()


count(survey, w1_quest_type_devices, w1_sph_web_5)
count(survey, w1_quest_type_devices, w1_sph_web_7)
count(survey, w1_quest_type_devices, w1_sph_msg_5)
count(survey, w1_quest_type_devices, w1_sph_msg_7)



# "shop" questions excluded due to issue with experimentla allocation
vars_int_prefix <- c("call", "msg", "web", "phot", "sm")




# clean 5 and 7 point scales

survey2 <- survey %>%
  mutate_at(
    vars(ends_with("_5")),
    ~case_when(. == "einmal im Monat oder seltener" ~
                 "once a month or less often",
               . == "mehrmals im Monat" ~ "several times a month",
               . == "mehrmals in der Woche" ~ "several times a week",
               . == "taglich" ~ "every day",
               . == "mehrmals taglich" ~ "several times a day",
               TRUE ~ .) %>%
      ifelse(. == ".", NA, .) %>%
      as.factor() %>%
      fct_relevel("once a month or less often",
                  "several times a month",
                  "several times a week",
                  "every day",
                  "several times a day")) %>%
  mutate_at(
    vars(ends_with("_7")),
    ~case_when(. == "ein- bis zweimal im Monat" ~ "once or twice a month",
               . == "ein- bis zweimal in der Woche" ~ "once or twice a week",
               . == "ein- bis zweimal taglich" ~ "once or twice a day",
               . == "mehrmals im Monat" ~ "several times a month",
               . == "mehrmals in der Woche" ~ "several times a week",
               . == "mehrmals taglich" ~ "several times a day",
               . == "seltener als einmal im Monat" ~ "less than once a month",
               TRUE ~ .) %>%
      ifelse(. == ".", NA, .) %>%
      as.factor() %>%
      fct_relevel("less than once a month",
                  "once or twice a month",
                  "several times a month",
                  "once or twice a week",
                  "several times a week",
                  "once or twice a day",
                  "several times a day"))


count(survey2, w1_sph_web_5)
count(survey2, w1_sph_web_7)
count(survey2, w1_sph_msg_5)
count(survey2, w1_sph_msg_7)



# list of new names with combined data
comb_vars_name <- c(str_c("sph_", vars_int_prefix, "_5"),
                    str_c("sph_", vars_int_prefix, "_7"))

# function to create combined data for factors
make_comb_var_fct <- function(data, var){

  var_data <- ifelse(is.na(data[[str_c("w1_", var)]]),
         data[[str_c("w1b_", var)]] %>% as.character(),
         data[[str_c("w1_", var)]] %>% as.character())

  if (str_detect(var, "_5")) {
    factor(var_data,
           levels = c("once a month or less often",
                      "several times a month",
                      "several times a week",
                      "every day",
                      "several times a day"))
  } else {
    factor(var_data,
           levels = c("less than once a month",
                      "once or twice a month",
                      "several times a month",
                      "once or twice a week",
                      "several times a week",
                      "once or twice a day",
                      "several times a day"))
  }

}

# combine data
comb_data <- map_dfc(comb_vars_name, make_comb_var_fct, data = survey2) %>%
  setNames(comb_vars_name)

# some checks
table(survey2$w1_sph_call_5,
      comb_data$sph_call_5, useNA = "always")


table(survey2$w1_sph_call_7,
      comb_data$sph_call_7, useNA = "always")



# make duration measures
dur_vars <- c(str_c("w1_sph_", vars_int_prefix),
              str_c("w1b_sph_", vars_int_prefix))



dur_df <- survey2 %>%
  select(matches(dur_vars)) %>%
  select(ends_with("hr"), ends_with("min")) %>%
  mutate_all(~as.numeric(.))


make_dur_var <- function(data, var){
  data[[str_c(var, "_hr")]] + data[[str_c(var, "_min")]]/60
}

# function to create combined data for duration
make_comb_var_dur <- function(data, var){

  ifelse(is.na(data[[str_c("w1_", var)]]),
         data[[str_c("w1b_", var)]],
         data[[str_c("w1_", var)]])

}



dur_df2 <- map(dur_vars, make_dur_var, data = dur_df) %>%
  reduce(cbind) %>%
  as_tibble() %>%
  setNames(dur_vars) %>%
  rename_all(~str_c(., "_dur"))


comb_vars_name_dur <- str_c("sph_", vars_int_prefix, "_dur")


dur_df3 <- map_dfc(comb_vars_name_dur, make_comb_var_dur, data = dur_df2) %>%
  set_names(comb_vars_name_dur) %>%
  mutate_all(list(log = ~log(. + 0.1)))

summarise_all(dur_df3, ~mean(is.na(.))) %>% cbind()
summarise_all(dur_df2, ~mean(is.na(.))) %>% cbind()


# bring all the data together

survey3 <- cbind(survey2, comb_data, dur_df2, dur_df3) %>%
  as_tibble()

# make numeric vars
survey4 <- survey3 %>%
  mutate_at(comb_vars_name,
            list("fct" = ~ .)) %>%
  mutate_at(comb_vars_name,
            ~ as.numeric(.))

count(survey4, w1_quest_type_devices, w1b_quest_type_devices,
      w1_sph_msg_5, w1b_sph_msg_5,
      sph_msg_5_fct, sph_msg_5) %>%
  print(n = 70)


count(survey4, w1_quest_type_devices, w1b_quest_type_devices,
      w1_sph_call_5, w1b_sph_call_5,
      sph_call_5_fct, sph_call_5) %>%
  print(n = 70)


count(survey_raw, w1b_quest_type_devices, w1b_sph_msg_7)

count(survey4, sph_msg_7, sph_msg_7_fct)
count(survey4, sph_msg_5, sph_msg_5_fct)


# export data -------------------------------------------------------------

write_rds(survey4, "./data/clean/survey_mtmm_clean.rds")

