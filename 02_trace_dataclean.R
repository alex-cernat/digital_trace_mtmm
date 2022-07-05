


# TO DO
# check duration calculation
# check coding of data and distributions


# Admin -------------------------------------------------------------------



# load library
library(tidyverse)
library(lubridate)

app_raw <- read_rds("./data/raw/app_tracking_combined_Mar21.rds")
mobile_raw <- read_rds("./data/raw/mobile_tracking_combined_Mar21.rds")

app_select_info <- read_csv("./data/cat_select.csv")

vars_int_prefix <- c("call", "msg", "web", "phot", "sm")



survey4 <- read_rds("./data/clean/survey_mtmm_clean.rds")

# Descriptives ------------------------------------------------------------


glimpse(mobile_raw)
glimpse(app_raw)
count(app_raw, app_cat)


app_raw %>%
  filter(app_cat == "Email, Messaging & Telephone") %>%
  count(app_n) %>%
  arrange(desc(n)) %>%
  View()

app_raw %>%
  filter(app_cat == "Social & dating") %>%
  count(app_n) %>%
  arrange(desc(n)) %>%
  View()



app_raw %>%
  count(app_n) %>%
  arrange(desc(n)) %>%
  View()


# make categories ---------------------------------------------------------


# apps of interest

call_app_vct <- filter(app_select_info,
                       category == "calling" & fit == "yes") %>%
  .[["app_n"]]
msg_app_vct <- filter(app_select_info,
                      category == "messaging" & fit == "yes") %>%
  .[["app_n"]]
web_app_vct <- filter(app_select_info,
                         category == "browsing" & fit == "yes") %>%
  .[["app_n"]]
phot_app_vct <- filter(app_select_info,
                        category == "photo" & fit == "yes") %>%
  .[["app_n"]]
sm_app_vct <- filter(app_select_info,
                     category == "sm" & fit == "yes") %>%
  .[["app_n"]]


# code events -------------------------------------------------------------


app <- app_raw %>%
  mutate(
    call = ifelse(app_n %in% call_app_vct, 1, 0),
    msg = ifelse(app_n %in% msg_app_vct, 1, 0),
    web = ifelse(app_n %in% web_app_vct, 1, 0),
    phot = ifelse(app_n %in% phot_app_vct, 1, 0),
    sm = ifelse(app_n %in% sm_app_vct, 1, 0))

app %>%
  filter(sm == 1) %>%
  count(app_n) %>%
  arrange(desc(n))


# aggregate data ----------------------------------------------------------

glimpse(app)

count(app, survey)
qplot(app$duration %>% log())
summary(app$duration)
qplot(app$duration)
quantile(app$duration,  0.99)

app <- app %>%
  mutate(date = lubridate::as_date(used_at),
         call_dur = ifelse(call > 0, duration, 0),
         msg_dur = ifelse(msg > 0, duration, 0),
         web_dur = ifelse(web > 0, duration, 0),
         phot_dur = ifelse(phot > 0, duration, 0),
         sm_dur = ifelse(sm > 0, duration, 0))

agg_data <-  app %>%
  group_by(new_id, date) %>%
  summarise(call_count = sum(call),
            msg_count = sum(msg),
            web_count = sum(web),
            phot_count = sum(phot),
            sm_count = sum(sm),
            call_dur = sum(call_dur),
            msg_dur = sum(msg_dur),
            web_dur = sum(web_dur),
            phot_dur = sum(phot_dur),
            sm_dur = sum(sm_dur)
            ) %>%
  ungroup()


glimpse(agg_data)
View(agg_data)

map(agg_data, qplot)

# aggregate web browsing --------------------------------------------------

glimpse(mobile_raw)

# aggregate by id and date and make summary statistics for web and social media

agg_browse_data <- mobile_raw %>%
  mutate(date = lubridate::as_date(used_at)) %>%
  group_by(new_id, date) %>%
  summarise(web_main_count = max(row_number()),
            web_main_dur = sum(duration, na.rm = T),
            sm_web_count = sum(
              str_detect(url, "facebook|twitter|instagram"), na.rm = T),
            sm_web_dur = sum(
              ifelse(str_detect(url, "facebook|twitter|instagram"),
                     duration,
                     0), na.rm = T)) %>%
  ungroup() %>%
  arrange(new_id, date)

glimpse(agg_browse_data)
View(agg_browse_data)
summary(agg_browse_data)

# top_sites <- count(mobile_raw, url) %>% arrange(desc(n))
#
# top_sites %>%
#   mutate() %>%
#   View()

# put together aggregate data ---------------------------------------------


agg_all_data <- full_join(agg_data, agg_browse_data,
                          by = c("new_id", "date")) %>%
  mutate_all(~ifelse(is.na(.), 0, .))

agg_all_data2 <- agg_all_data %>%
  mutate(web_count2 = web_count + web_main_count,
         web_dur2 = web_dur + web_main_dur,
         sm_count2 = sm_count + sm_web_count,
         sm_dur2 = sm_dur + sm_web_dur) %>%
  select(-web_count, -web_dur, -web_main_dur, -web_main_count,
         -sm_web_count, -sm_web_dur, -sm_count, -sm_dur) %>%
  rename_all(~str_remove(., "2"))

View(agg_all_data2)

mean(agg_all_data$web_count, use = "compete.obs")
mean(agg_all_data2$web_count, use = "compete.obs")

mean(agg_all_data$sm_count, use = "compete.obs")
mean(agg_all_data2$sm_count, use = "compete.obs")



# save aggregate date by date and individual

write_rds(agg_all_data2, "./data/clean/agg_phone_id_day.rds")


# aggregate 7/30 day period -----------------------------------------------

# load clean survey data

survey_full <- read_rds("./data/clean/survey_mtmm_clean.rds")

# make date thresholds
survey_dates <- survey_full %>%
  select(new_id, w1_datetime, w1b_datetime) %>%
  mutate(w1_date = as_date(mdy_hm(w1_datetime)),
         w1b_date = as_date(mdy_hm(w1b_datetime)),
         w1_date_7 = w1_date - ddays(7),
         w1_date_30 = w1_date - ddays(30),
         w1b_date_7 = w1b_date - ddays(7),
         w1b_date_30 = w1b_date - ddays(30),
         new_id = as.numeric(new_id))

count(survey_dates, w1_date)
count(survey_full, w1_datetime) %>% arrange(desc(n))


# merge thresholds with aggregate date

agg_all_data3 <- left_join(agg_all_data2, survey_dates, by = "new_id") %>%
  mutate(date = as_date(date))


agg_all_data3 %>%
  mutate_at(vars(matches("count"), matches("dur")),
            list("7d_w1" = ~ifelse(date > w1_date_7 & date < w1_date,
                                  ., 0))) %>%
  View()

trace_data_diff <- agg_all_data3  %>%
  group_by(new_id) %>%
  mutate(day_before_w1 = difftime(min(date), w1_date, units = "days") %>%
           round(),
         day_before_w1b = difftime(min(date), w1b_date, units = "days") %>%
           round(),
         more_7_days_w1 = ifelse(day_before_w1 < -7, "Yes", "No"),
         more_7_days_w1b = ifelse(day_before_w1b < -7, "Yes", "No")
         ) %>%
  filter(row_number() == 1) %>%
  ungroup()

write_rds(trace_data_diff, "./data/clean/trace_data_diff.rds")


qplot(trace_data_diff$day_before_w1)
qplot(trace_data_diff$day_before_w1b)

count(trace_data_diff, more_7_days_w1, more_7_days_w1b)





# make 7d and 30d aggregates ----------------------------------------------




agg_all_data3 <- mutate(agg_all_data3,
                        day_before_w1 = difftime(date,
                                                 w1_date,
                                                 units = "days") %>%
                          round() %>% as.numeric(),
                        day_before_w1b = difftime(date,
                                                  w1b_date,
                                                  units = "days") %>%
                          round() %>% as.numeric(),
                        w1_7d_window = ifelse(day_before_w1 %in% -6:0, 1, 0),
                        w1b_7d_window = ifelse(day_before_w1b %in% -6:0, 1, 0),
                        w1_30d_window = ifelse(day_before_w1 %in% -29:0, 1, 0),
                        w1b_30d_window = ifelse(day_before_w1b %in% -29:0, 1, 0))

w1_7d_agg <- agg_all_data3 %>%
  filter(w1_7d_window == 1) %>%
  group_by(new_id) %>%
  mutate(days_count = max(row_number())) %>%
  summarise_at(vars(ends_with("count"), ends_with("dur")),
               ~ sum(., na.rm = T)) %>%
  rename_at(vars(-new_id),
            ~str_c(., "_w1_7d"))


w1b_7d_agg <- agg_all_data3 %>%
  filter(w1b_7d_window == 1) %>%
  group_by(new_id) %>%
  mutate(days_count = max(row_number())) %>%
  summarise_at(vars(ends_with("count"), ends_with("dur")),
               ~ sum(., na.rm = T)) %>%
  rename_at(vars(-new_id),
            ~str_c(., "_w1b_7d"))

w1_30d_agg <- agg_all_data3 %>%
  filter(w1_30d_window == 1) %>%
  group_by(new_id) %>%
  mutate(days_count = max(row_number())) %>%
  summarise_at(vars(ends_with("count"), ends_with("dur")),
               ~ sum(., na.rm = T)) %>%
  rename_at(vars(-new_id),
            ~str_c(., "_w1_30d"))


w1b_30d_agg <- agg_all_data3 %>%
  filter(w1b_30d_window == 1) %>%
  group_by(new_id) %>%
  mutate(days_count = max(row_number())) %>%
  summarise_at(vars(ends_with("count"), ends_with("dur")),
               ~ sum(., na.rm = T)) %>%
  rename_at(vars(-new_id),
            ~str_c(., "_w1b_30d"))


# bring all the data together

trace_agg_data <- full_join(w1_7d_agg, w1b_7d_agg, by = "new_id") %>%
  full_join(w1_30d_agg, by = "new_id") %>%
  full_join(w1b_30d_agg, by = "new_id")



# clean duration ----------------------------------------------------------


# make duration minutes

trace_agg_data2 <- trace_agg_data %>%
  mutate_at(vars(matches("_dur_")),
            ~./60) %>%
  mutate_at(vars(matches("_dur_"), matches("_count_")),
            list("log" = ~log(.))) %>%
  mutate_all(~ifelse(is.infinite(.), NA, .))


summary(trace_agg_data2)


# join survey and agg data ------------------------------------------------


data_full <- left_join(
  mutate(survey_full, new_id = as.numeric(new_id)),
  trace_agg_data2,
  by = "new_id")



data_w1_w1b <- data_full %>%
  filter(!(participation_w1 == "no" & participation_w1b == "no"))

data_w1_w1b %>%
  count(participation_w1, participation_w1b)


summ_agg <- data_w1_w1b %>%
  select(matches("days_count")) %>%
  summarise_all(list(
    miss_n = ~sum(is.na(.)),
    miss_prop = ~mean(is.na(.)) * 100,
    avg_nr_days = ~mean(., na.rm = T)))

rbind(summ_agg[, 1:4] %>% as.numeric(),
      summ_agg[, 5:8] %>% as.numeric(),
      summ_agg[, 9:12] %>% as.numeric()) %>%
  as_tibble() %>%
  setNames(c("w1_7d", "w1b_7d", "w1_30d", "w1b_30d")) %>%
  mutate(stat = c("Number of cases with missing",
                  "Average % cases missing",
                  "Average number of days")) %>%
  select(stat, everything())

data_full

survey_dates %>%
  select(w1_date, w1b_date) %>%
  summarise_all(~mean(is.na(.)) * 100)



# understand who has data in both survey and digital trace data --------------


count(app_raw, new_id) %>% nrow()
count(mobile_raw, new_id) %>% nrow()

small_data <- survey4 %>%
  filter(participation_w1 == "yes" & participation_w1b == "yes") %>%
  select(new_id) %>%
  mutate(new_id = as.numeric(new_id))

semi_join(small_data, mobile_raw) %>% nrow()
semi_join(small_data, app_raw) %>% nrow()


full_join(count(mobile_raw, new_id),
          count(mobile_raw, new_id), by = "new_id") %>%
  semi_join(small_data) %>% nrow()


# "call" "msg"  "web"  "phot" "sm"

trace_agg_data2 %>%
  select(new_id, matches("count_w1_30d")) %>%
  group_by(new_id) %>%
  summarise_all(~sum(., na.rm = T)) %>%
  ungroup() %>%
  summarise_all(~mean(. == 0, na.rm = T)) %>%
  gather()



app %>%
  select(new_id, vars_int_prefix) %>%
  group_by(new_id) %>%
  summarise_all(~sum(., na.rm = T)) %>%
  ungroup() %>%
  summarise_all(~mean(. == 0, na.rm = T))

trace_agg_data2 %>%
  select(matches("w1_30d_log")) %>%
  mutate_all(~ifelse(. < 0, NA, .)) %>%
  map(qplot)


# export data -------------------------------------------------------------

write_rds(trace_agg_data2, "./data/clean/trace_agg_data.rds")


