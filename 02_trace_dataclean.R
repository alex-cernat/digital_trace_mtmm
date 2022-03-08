
# Admin -------------------------------------------------------------------

### TO DO:
### 1. make activity variables
### 2. update with student info later
### 3. aggregate date/person in duration and number of times
### 4. aggregate web browsing and add
### 5. make 7 days and 30 days period
### 6. link with survey data and exploratory analysis
### 7. MTMM model



# load library
library(tidyverse)
library(lubridate)

app_raw <- read_rds("./data/raw/app_tracking_combined.rds")
mobile_raw <- read_rds("./data/raw/mobile_tracking_combined.rds")



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

# call
app_raw %>%
  filter(str_detect(app_n, "dialer|contacts|Constacts|^Phone$")) %>%
  filter(!str_detect(app_n, "recover|Recover|Spontacts")) %>%
  count(app_n) %>%
  arrange(desc(n)) %>%
  print()



# messaging

app_raw %>%
  filter(
  str_detect(app_n,
  "Messenger|Telegram|WhatsApp|Snapchat|Jodel|Skype|Chat|Hangouts|
Signal - Private Messenger|Discord")) %>%
  filter(!str_detect(app_n, "Flirt|flirt|Date|date|dating|Dating|Gay|gay")) %>%
  count(app_n) %>%
  arrange(desc(n)) %>%
  View()


# web

app_raw %>%
  filter(str_detect(app_n, "web|browser|Web|Browser|Chrome|Firefox")) %>%
  filter(!str_detect(app_n, "File|file")) %>%
  count(app_n) %>%
  arrange(desc(n)) %>%
  View()


# photo
app_raw %>%
  filter(str_detect(app_n, "photo|Photo|Galerij|Cámara|Camera")) %>%
  filter(!str_detect(app_n, "bank|Bank")) %>%
  count(app_n) %>%
  arrange(desc(n)) %>%
  View()

# social media
app_raw %>%
  filter(
    str_detect(app_n,
               "Instagram|Facebook|Twitter|LinkedIn|Pinterest")) %>%
  filter(!str_detect(app_n, "Messenger|Flirt|flirt|Date|date|dating|Dating|Gay|gay")) %>%
  count(app_n) %>%
  arrange(desc(n)) %>%
  View()


# code events -------------------------------------------------------------


app <- app_raw %>%
  mutate(
    call = ifelse(
      str_detect(app_n, "dialer|contacts|Constacts|^Phone$") &
        !str_detect(app_n, "recover|Recover|Spontacts"),
      1,
      0
    ),
    msg = ifelse(
      str_detect(
        app_n,
        "Messenger|Telegram|WhatsApp|Snapchat|Jodel|Skype|Chat|Hangouts|
Signal - Private Messenger|Discord"
      ) &
        !str_detect(app_n, "Flirt|flirt|Date|date|dating|Dating|Gay|gay"),
      1,
      0
    ),
    browse = ifelse(
      str_detect(app_n,
                 "web|browser|Web|Browser|Chrome|Firefox") &
        !str_detect(app_n, "File|file"),
      1,
      0
    ),
    photo =
      ifelse(
        str_detect(app_n, "photo|Photo|Galerij|Cámara|Camera") &
          !str_detect(app_n, "bank|Bank"),
        1,
        0
      ),
    social = ifelse(
      str_detect(app_n,
                 "Instagram|Facebook|Twitter|LinkedIn|Pinterest") &
        !str_detect(
          app_n,
          "Messenger|Flirt|flirt|Date|date|dating|Dating|Gay|gay"
        ),
      1,
      0
    )
  )



# aggregate data ----------------------------------------------------------

glimpse(app)

count(app, survey)
count(app, browse)
qplot(app$duration %>% log())
summary(app$duration)

agg_data <- app %>%
  mutate(date = lubridate::as_date(used_at)) %>%
  group_by(id, date) %>%
  summarise_at(vars(call, msg, browse, photo, social),
               list("count" = ~ sum(.),
                    "dur" = ~ ifelse(. == 1, sum(duration), 0))) %>%
  filter(row_number() == 1) %>%
  ungroup()


glimpse(agg_data)
View(agg_data)


# aggregate web browsing --------------------------------------------------

glimpse(mobile_raw)

agg_browse_data <- mobile_raw %>%
  mutate(date = lubridate::as_date(used_at)) %>%
  group_by(id, date) %>%
  summarise(browse_main_count = max(row_number()),
            browse_main_dur = sum(duration)) %>%
  ungroup() %>%
  arrange(id, date)

glimpse(agg_browse_data)
View(agg_browse_data)


# put together aggregate data ---------------------------------------------

agg_all_data <- full_join(
  mutate_at(agg_data, vars(id, date), ~as.character(.)),
  mutate_at(agg_browse_data, vars(id, date), ~as.character(.)),
  by = c("id", "date")) %>%
  mutate_all(~ifelse(is.na(.), 0, .))

agg_all_data2 <- agg_all_data %>%
  mutate(browse_count2 = browse_count + browse_main_count,
         browse_dur2 = browse_dur + browse_main_dur) %>%
  select(-browse_count, -browse_dur, -browse_main_dur, -browse_main_count) %>%
  rename_all(~str_remove(., "2"))

View(agg_all_data2)

# save aggregate date by date and individual

write_rds(agg_all_data2, "./data/clean/agg_phone_id_day.rds")


# aggregate 7/30 day period -----------------------------------------------

# load clean survey data

survey_full <- read_rds("./data/clean/survey_mtmm_clean.rds")

# make date thresholds
survey_dates <- survey_full %>%
  rename(id = ID) %>%
  select(id, w1_datetime, w1b_datetime) %>%
  mutate(w1_date = as_date(mdy_hm(w1_datetime)),
         w1b_date = as_date(mdy_hm(w1b_datetime)),
         w1_date_7 = w1_date - ddays(7),
         w1_date_30 = w1_date - ddays(30),
         w1b_date_7 = w1b_date - ddays(7),
         w1b_date_30 = w1b_date - ddays(30))

count(survey_dates, w1_date)
count(survey_full, w1_datetime) %>% arrange(desc(n))


# merge thresholds with aggregate date

agg_all_data3 <- left_join(agg_all_data2, survey_dates, by = "id")


agg_all_data3 %>%
  mutate_at(vars(matches("count"), matches("dur")),
            list("7d_w1" = ~ifelse(date > w1_date_7 & date < w1_date,
                                  ., 0))) %>%
  View()

trace_data_diff <- agg_all_data3 %>%
  group_by(id) %>%
  mutate(day_before_w1 = difftime(min(date), w1_date, units = "days") %>%
           round(),
         day_before_w1b = difftime(min(date), w1b_date, units = "days") %>%
           round(),
         more_7_days_w1 = ifelse(day_before_w1 < -7, "Yes", "No"),
         more_7_days_w1b = ifelse(day_before_w1b < -7, "Yes", "No")) %>%
  filter(row_number() == 1) %>%
  ungroup()

write_rds(trace_data_diff, "./data/clean/trace_data_diff.rds")


qplot(trace_data_diff$day_before_w1)
qplot(trace_data_diff$day_before_w1b)

count(trace_data_diff, more_7_days_w1, more_7_days_w1b)







