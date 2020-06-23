# This is a sample of the code that produces all numbers and figures reported in EDA

## Set dates
START_DATE <- "2019-06-21"
END_DATE <- "2019-10-21"
DIR <- getwd()

## Packages
require(mongolite)
require(tidyverse)
require(ggplot2)
require(reshape2)
require(lubridate)
require(dplyr)
require(xtable)

## Scripts
source("functions.R")

## Connect to database
elections_connect <- col_conn("Elections")
parties_connect <- col_conn("Parties")
leaders_connect <- col_conn("Leaders")
users_connect <- col_conn("Users")

## Collected users and tweets
start_week <- paste0(substr(START_DATE,1,4),"-",isoweek(START_DATE))
end_week <- paste0(substr(END_DATE,1,4),"-",isoweek(END_DATE))
search_by_dates <- paste0('{"timestamp_CAD":{"$gte":"',START_DATE,'","$lte":"',END_DATE,'"}}')
MINIMAL_FIELDS <- '{ "timestamp_CAD":1, "week":1, "screen_name":1 }'

elections_df <- elections_connect$find(query = search_by_dates, fields = MINIMAL_FIELDS)
parties_df <- parties_connect$find(query = search_by_dates,fields = MINIMAL_FIELDS)
leaders_df <- leaders_connect$find(query = search_by_dates,
                                   fields = '{"timestamp_CAD":1,"week":1,"screen_name":1,
                                   "tg_jt":1,"tg_as":1,"tg_js":1,"tg_yfb":1,"tg_em":1,"tg_mb":1}')
elections_connect$disconnect()
parties_connect$disconnect()
leaders_connect$disconnect()

## TOTAL USERS
users_df <- users_connect$find(
  query = paste0('{"$or":[{"week":{"$gte":"',start_week,'","$lte":"',end_week,'"}},
                 {"timestamp_CAD":{"$gte":"',START_DATE,'","$lte":"',END_DATE,'"}}]}'),
  fields = '{"week":1, "timestamp_CAD":1,"screen_name":1, "scores_english":1, "scores_universal":1}')
users_connect$disconnect()


## TWEETS PER QUERY
tpq <- matrix(c(dim(elections_df)[1],
                dim(parties_df)[1],
                dim(leaders_df)[1],
                dim(elections_df)[1] + dim(parties_df)[1] + dim(leaders_df)[1]), ncol = 1)
colnames(tpq) <- c("Number of tweets")
rownames(tpq) <- c("Elections","Parties","Leaders","Total")

print("Tweets per query")
xtable(tpq, digits = 0)

## Historic trends

### Elections
# Percentage
print("Elections: ")
round((tpq[1]/tpq[4])*100,2)

# maximum
length(which(elections_df$timestamp_CAD == "2019-10-21"))

# plot
daily_election <- data.frame(table(elections_df$timestamp_CAD))
colnames(daily_election) <- c("Date","Tweets")
daily_election$Date <- as.Date(as.character(daily_election$Date))

save_pdf(DIR,"elections_trend")
ggplot(daily_election, aes(x = Date, y = Tweets, group = 1)) +
  geom_line(color='#2AB7CA') +
  geom_point(size = 1, color='#2AB7CA', alpha = 0.5) +
  xlab("") + 
  ylab("Number of tweets") +
  ggtitle("Daily tweets count about\n 43rd Canadian Elections") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 7)) +
  geom_label(aes(x = as.Date("2019-09-19"), 
                 y = daily_election[which(daily_election$Date == "2019-09-19"),2]+1100), 
             label = "Pictures of the JT in\n blackface are published", size = 2) +
  geom_label(aes(x = as.Date("2019-10-07"), 
                 y = daily_election[which(daily_election$Date == "2019-10-07"),2]+850), 
             label = "First official debate", size = 2) +
  geom_label(aes(x = as.Date("2019-10-16"), 
                 y = daily_election[which(daily_election$Date == "2019-10-16"),2]+850), 
             label = "Elections circumstances", size = 2) +
  geom_label(aes(x = as.Date("2019-10-21"), 
                 y = daily_election[which(daily_election$Date == "2019-10-21"),2]+850), 
             label = "Election day", size = 2)
dev.off()


## Presence of bots
table_users <- table(users_final_df$screen_name)
selected_users <- names(table_users[table_users>18])

# To select account with high ans low probability of being a bot
users_final_df %>% 
  filter(screen_name %in% selected_users) %>% 
  group_by(screen_name) %>% 
  summarise( prob = max(scores_english)) %>% 
  arrange(prob) %>% 
  tail(20)

users_final_df %>% 
  filter(screen_name %in% selected_users) %>% 
  group_by(screen_name) %>% 
  summarise( prob = max(scores_english)) %>% 
  arrange(prob) %>% 
  head(5)


selected_users <- c("MichelleCasti","DianneM65725926","dominic_apold",#"ImmoralReport","CRMirror",
                    "mclickster13","BriApp","jjgpden",#"peterdiane01","AJWite"
                    "saskboy","M_A_Davies","GrannaBaker")#,"garbee7160","MaxontheCoast")

save_pdf(DIR,"probabilities_trend")
users_final_df %>% filter(screen_name %in% selected_users) %>%
  mutate(week_n = substr(full_week,6,7)) %>% 
  ggplot(aes(x = week_n, y = scores_english, group = screen_name ,color = screen_name)) +
  geom_line() +
  xlab("Week number on 2019") + 
  ylab("Probability of being a bot") +
  ggtitle("Weekly estimated probabilities of being a bot") +
  labs(color='Twitter account') +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=7))
dev.off()
