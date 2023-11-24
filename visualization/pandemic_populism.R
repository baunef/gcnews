# R
# Baune, Ferdinand
# 20231108 
# Visualizing Pandemic Populism

rm(list = ls())
setwd("./")

# Packages ----------------------------------------------------------------

library(tidyverse)
library(pammtools)
print("========== Packages loaded ==========")


# Functions and Constants -------------------------------------------------

online_media <- c("Süddeutsche Zeitung Online", 
                  "Spiegel Online", 
                  "Focus Online", 
                  "Der Tagesspiegel Online", 
                  "bild.de")

print_media  <- c("Süddeutsche Zeitung", 
                  "Der Tagesspiegel", 
                  "Die Welt", 
                  "BILD", 
                  "taz - die tageszeitung")

# Load Data ---------------------------------------------------------------

gcnews <- read.csv("./data/gcnews/gc_news_labeled.csv") %>%
  mutate(
    Date = as.Date(Date)
  )

print("========== Data read ==========")
# Visualization -----------------------------------------------------------

covid_months_df <- gcnews %>% 
  group_by(covid, timeframe = lubridate::floor_date(Date, "month")) %>% 
  summarize(
    n_par = n(),
    n_pop = mean( (PopBERT_AE + PopBERT_PC > 1) * 100 ),
    cf_pop = qt(0.975,df=n()-1)*sd((PopBERT_AE + PopBERT_PC > 1) * 100)/sqrt(n())
  ) %>% ungroup %>% arrange(timeframe)

covid_months_populism <- rbind(
  covid_months_df, 
  tail(covid_months_df %>% dplyr::filter(covid=="Non-COVID-19"), 1) %>% dplyr::mutate(timeframe= as.Date("2023-01-01")),
  tail(dplyr::filter(covid_months_df, n_par > 1000, covid=="COVID-19"), 1) %>% dplyr::mutate(timeframe = timeframe + months(1))
) %>% 
  dplyr::filter(n_par > 1000) %>% # To remove pre- and post-pandemic outlier months.
  ggplot(aes(x=date, y=n_pop)) +
  geom_step(aes(x=timeframe, y=n_pop, color=covid), direction="hv") + 
  geom_stepribbon(aes(x=timeframe, fill=covid,
                      ymin = n_pop - cf_pop,
                      ymax = n_pop + cf_pop), alpha=.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0)) +
  theme(legend.position = "bottom") +
  ylim(0,15) +
  geom_vline(xintercept=as.Date("2022-02-24")) + # Russian War
  geom_vline(xintercept=as.Date("2020-03-11")) + # Pandemic Announcement
  geom_vline(xintercept=as.Date("2021-03-01")) + # Mask Affair
  geom_vline(xintercept=as.Date("2021-08-10")) +   # End of Free Tests
  geom_vline(xintercept=as.Date("2021-11-18")) +   # End of Free Tests
  annotate(
    "text",
    x = as.Date(c("2020-03-11", "2021-03-01", "2021-08-10", "2021-11-18", "2022-02-24")), 
    y = 2.5 + (5-2.5)/2, 
    label = c("Pandemic", "Mask Affair", "Tests End", " IfSG\nChanges", "War Starts"),
    hjust=-.1
  ) +
  annotate("rect", 
           xmin=as.Date(c("2020-03-16", "2020-12-16")), xmax=as.Date(c("2020-05-06", "2021-03-03")), 
           ymin=c(0,0) , ymax=c(15,15), 
           alpha=0.15, color=FALSE, fill="black") +
  labs(x="Time (Months)", y="Mean Populism Score (%)", 
       col="", fill="") +
  xlim(as.Date("2020-01-01"),as.Date("2023-01-01")) +
  theme(legend.position = c(.88, .88)) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 11))

plot(covid_months_populism)
pdf(file = "./figures/covid_months_populism.pdf",  
    width = 12, 
    height = 4) 
plot(covid_months_populism)
dev.off()



econ_months_df <- gcnews %>% 
  group_by(econ, timeframe = lubridate::floor_date(Date, "month")) %>% 
  summarize(
    n_par = n(),
    n_pop = mean( (PopBERT_AE + PopBERT_PC > 1) * 100 ),
    cf_pop = qt(0.975,df=n()-1)*sd((PopBERT_AE + PopBERT_PC > 1) * 100)/sqrt(n())
  ) %>% ungroup %>% arrange(timeframe)

econ_months_populism <- rbind(
  econ_months_df, 
  tail(econ_months_df %>% dplyr::filter(econ=="Non-Economy"), 1) %>% dplyr::mutate(timeframe= as.Date("2023-01-01")),
  tail(dplyr::filter(econ_months_df, econ=="Economy"), 1) %>% dplyr::mutate(timeframe = timeframe + months(1))
) %>% 
  ggplot(aes(x=date, y=n_pop)) +
  geom_step(aes(x=timeframe, y=n_pop, color=econ), direction="hv") + 
  geom_stepribbon(aes(x=timeframe, fill=econ,
                      ymin = n_pop - cf_pop,
                      ymax = n_pop + cf_pop), alpha=.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0)) +
  theme(legend.position = "bottom") +
  ylim(0,20) +
  geom_vline(xintercept=as.Date("2022-02-24")) + # Russian War
  geom_vline(xintercept=as.Date("2020-03-11")) + # Pandemic Announcement
  geom_vline(xintercept=as.Date("2021-03-01")) + # Mask Affair
  geom_vline(xintercept=as.Date("2021-08-10")) +   # End of Free Tests
  geom_vline(xintercept=as.Date("2021-11-18")) +   # End of Free Tests
  annotate(
    "text",
    x = as.Date(c("2020-03-11", "2021-03-01", "2021-08-10", "2021-11-18", "2022-02-24")), 
    y = 2.5 + (5-2.5)/2, 
    label = c("Pandemic", "Mask Affair", "Tests End", " IfSG\nChanges", "War Starts"),
    hjust=-.1
  ) +
  annotate("rect", 
           xmin=as.Date(c("2020-03-16", "2020-12-16")), xmax=as.Date(c("2020-05-06", "2021-03-03")), 
           ymin=c(0,0) , ymax=c(20,20), 
           alpha=0.15, color=FALSE, fill="black") +
  labs(x="Time (Months)", y="Mean Populism Score (%)", 
       col="", fill="") +
  xlim(as.Date("2020-01-01"),as.Date("2023-01-01")) +
  theme(legend.position = c(.88, .88)) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 11)) +
  scale_fill_manual(values=c("#7CAE00", "#C77CFF")) + 
  scale_color_manual(values=c("#7CAE00", "#C77CFF"))

plot(econ_months_populism)
pdf(file = "./figures/econ_months_populism.pdf",
    width = 12, 
    height = 4) 
plot(econ_months_populism)
dev.off()

