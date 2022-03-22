
# Admin -------------------------------------------------------------------


# check coding of data and distributions

# load library
library(tidyverse)
library(lubridate)

app_raw <- read_rds("./data/raw/app_tracking_combined_Mar21.rds")
mobile_raw <- read_rds("./data/raw/mobile_tracking_combined_Mar21.rds")

app_select_info <- read_csv("./data/cat_select.csv")

vars_int_prefix <- c("call", "msg", "web", "phot", "sm")


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

agg_data <- app %>%
  mutate(date = lubridate::as_date(used_at)) %>%
  group_by(new_id, date) %>%
  summarise_at(vars(call, msg, web, phot, sm),
               list("count" = ~ sum(.),
                    "dur" = ~ ifelse(. > 0.9, sum(duration), 0))) %>%
  filter(row_number() == 1) %>%
  ungroup()


glimpse(agg_data)
View(agg_data)


# aggregate web browsing --------------------------------------------------

glimpse(mobile_raw)

agg_browse_data <- mobile_raw %>%
  mutate(date = lubridate::as_date(used_at)) %>%
  group_by(new_id, date) %>%
  summarise(web_main_count = max(row_number()),
            web_main_dur = sum(duration)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  arrange(new_id, date)

glimpse(agg_browse_data)
View(agg_browse_data)


# put together aggregate data ---------------------------------------------


agg_all_data <- full_join(agg_data, agg_browse_data,
                          by = c("new_id", "date")) %>%
  mutate_all(~ifelse(is.na(.), 0, .))

agg_all_data2 <- agg_all_data %>%
  mutate(web_count2 = web_count + web_main_count,
         web_dur2 = web_dur + web_main_dur) %>%
  select(-web_count, -web_dur, -web_main_dur, -web_main_count) %>%
  rename_all(~str_remove(., "2"))

View(agg_all_data2)

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
               ~ mean(.)) %>%
  rename_at(vars(-new_id),
            ~str_c(., "_w1_7d"))


w1b_7d_agg <- agg_all_data3 %>%
  filter(w1b_7d_window == 1) %>%
  group_by(new_id) %>%
  mutate(days_count = max(row_number())) %>%
  summarise_at(vars(ends_with("count"), ends_with("dur")),
               ~ mean(.)) %>%
  rename_at(vars(-new_id),
            ~str_c(., "_w1b_7d"))

w1_30d_agg <- agg_all_data3 %>%
  filter(w1_30d_window == 1) %>%
  group_by(new_id) %>%
  mutate(days_count = max(row_number())) %>%
  summarise_at(vars(ends_with("count"), ends_with("dur")),
               ~ mean(.)) %>%
  rename_at(vars(-new_id),
            ~str_c(., "_w1_30d"))


w1b_30d_agg <- agg_all_data3 %>%
  filter(w1b_30d_window == 1) %>%
  group_by(new_id) %>%
  mutate(days_count = max(row_number())) %>%
  summarise_at(vars(ends_with("count"), ends_with("dur")),
               ~ mean(.)) %>%
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
  mutate_at(vars(matches("_dur_")),
            list("log" = ~log(. + 0.1)))

trace_agg_data2 %>%
  select(ends_with("w1_7d")) %>%
  map(qplot)

# take log

trace_agg_data2 %>%
  select(matches("days")) %>%
  summary()

# export data

write_csv(trace_agg_data2, "./data/clean/trace_agg_data.csv")
