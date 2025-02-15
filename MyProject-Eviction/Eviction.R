#Title: Studying Eviction Patterns in U.S.
#Author: Yu-Jing Chen
#ychen10@swarthmore.edu

dir.create(path = "data")
dir.create(path = "output")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(naniar)

eviction <- read.csv(file = "data/tract_proprietary.csv")

#Checking data
glimpse(eviction)
summary(eviction)

#Checking missing data
colSums(is.na(eviction))
#Most columns have no missing value, except judgements (29533), judgement_rate(29703),
#which is reasonable as some cases though filed might not reached the stage of court judgement. 
#investigate how missing data is distributed
eviction %>%
  summarise(across(c(judgements, judgement_rate), ~mean(is.na(.))))

#investigate missing by state
missing <- eviction %>%
  group_by(state) %>%
  summarise(missing_judgements = sum(is.na(judgements))) %>%
  arrange(desc(missing_judgements))
write.table(missing, file = "output/missing_judgements.txt", sep = "\t", row.names = FALSE, quote = FALSE)

##############
#1.Investigate correlation between eviction filings and threatened cases
# Apply log transformation (adding 1 to avoid log(0))
eviction <- eviction %>%
  mutate(log_filings = log(filings + 1),
         log_threatened = log(threatened + 1))

FvsT_log <- lm(log_threatened ~ log_filings, data = eviction)
sink(file = "output/Linear-Filing_Threatened.txt")
summary(FvsT_log)
sink()

eviction %>%
  ggplot(aes(x = log_filings, y = log_threatened)) +
  geom_point(alpha = 0.3, color = "blue3") +
  geom_smooth(method = "lm", color = "red3", linetype = "dashed") +
  labs(title = "Log Linear Regression: Filings vs. Threatened Cases",
       x = "Log of Eviction Filings", y = "Log of Threatened Cases") +
  theme_minimal()
ggsave(filename = "output/Linear-Filing_Threatened.png")
################################
#2.Investigate data change by year
eviction_yearly <- eviction %>%
  group_by(year) %>%
  summarise(total_filings = sum(filings, na.rm = TRUE),
            total_judgements = sum(judgements, na.rm = TRUE))
summary(eviction_yearly)

#Visualize the trend (eviction activity vs. year)
Evi_yr <-ggplot(eviction_yearly, aes(x = year)) +
  geom_line(aes(y = total_filings, color = "Filings"), size = 1) +
  geom_line(aes(y = total_judgements, color = "Judgements"), size = 1, linetype = "dashed") +
  labs(title = "Eviction Filings and Judgements Over Time",
       x = "Year", y = "Count",
       color = "Legend") +
  theme_minimal()
ggsave(filename = "output/Evi_yr.png", plot = Evi_yr, width = 8, height = 6, dpi = 300)

########################3
#3. Eviction Activity by State
eviction_state <- eviction %>%
  group_by(state) %>%
  summarise(total_filings = sum(filings, na.rm = TRUE),
            total_judgements = sum(judgements, na.rm = TRUE),
            total_threatened = sum(threatened,na.rm = TRUE)) %>%
  arrange(desc(total_filings))
#Decide to only show the 10 most eviction-active states to make the plot less crowded.
Evi_state <-ggplot(eviction_state %>% slice_max(total_filings, n = 10), aes(x = reorder(state, total_filings), y = total_filings)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = total_filings), hjust = 1.1, size = 4, color = "white")+
  coord_flip() +
  labs(title = "Total Eviction Filings by State",
       x = "State", y = "Total Filings") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),  
    axis.title.y = element_text(size = 14, face = "bold"), 
  )
ggsave(filename = "output/Evi_State.png", plot = Evi_state, width = 8, height = 6, dpi = 300)
####
#Temporary end of the analysis.


