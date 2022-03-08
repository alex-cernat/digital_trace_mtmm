
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
  View()

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



# are these social media?
# YouTube
# TikTok - Make Your Day
# Twitch
# PlanetRomeo: Gay Dating
# Discord - Chat for Games Messenger
#  Tinder
# LOVOO - Live Dating & Friends
# MeWe
# Grindr - Gay, bi & curious guy
# mingle
