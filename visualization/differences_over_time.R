# R
# Baune, Ferdinand
# 20231108 
# Visualizing Differences over Time

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
# Step Plots --------------------------------------------------------------

time_df <- gcnews %>% 
  group_by(timeframe = lubridate::floor_date(Date, "month")) %>% 
  summarize(
    n_par = n(),
    n_pop = mean( (PopBERT_AE + PopBERT_PC > 1) * 100 ),
    cf_pop = qt(0.975,df=n()-1)*sd((PopBERT_AE + PopBERT_PC > 1) * 100)/sqrt(n())
  ) %>% ungroup %>% arrange(timeframe)

time_trends <- rbind(
  time_df, 
  tail(time_df, 1) %>% dplyr::mutate(timeframe= as.Date("2023-01-01"))
) %>%
  ggplot(aes(x=date, y=n_pop)) +
  geom_step(aes(x=timeframe, y=n_pop), direction="hv") + 
  geom_stepribbon(aes(x=timeframe,
                      ymin = n_pop - cf_pop,
                      ymax = n_pop + cf_pop), alpha=.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0)) +
  theme(legend.position = "bottom") +
  ylim(0,15) + xlim(as.Date("2020-01-01"), as.Date("2022-12-31")) +
  xlab("Time (Months)") + ylab("Mean Populism Score (%)") +
  geom_vline(xintercept=as.Date("2022-02-24")) + # Russian War
  geom_vline(xintercept=as.Date("2020-03-11")) + # Pandemic Announcement
  geom_vline(xintercept=as.Date("2021-03-01")) + # Mask Affair
  geom_vline(xintercept=as.Date("2021-08-10")) +   # End of Free Tests
  geom_vline(xintercept=as.Date("2021-11-18")) +   # IFSG Changes
  annotate(
    "text",
    x = as.Date(c("2020-03-11", "2021-03-01", "2021-08-10", "2021-11-18", "2022-02-24")), 
    y = 2.5 + (5-2.5)/2,
    label = c("Pandemic", "Mask Affair", " Tests\n End", " IfSG\nChanges", "War Starts"),
    hjust=-.1,
    vjust=.5
  ) +
  annotate("rect", 
           xmin=as.Date(c("2020-03-16", "2020-12-16")), xmax=as.Date(c("2020-05-06", "2021-03-03")), 
           ymin=c(0,0) , ymax=c(15,15), 
           alpha=0.15, fill="black") +
scale_x_continuous(
    breaks=c(as.Date("2020-01-01"), as.Date("2021-01-01"), as.Date("2022-01-01")), 
    labels=c("2020", "2021","2022")
  )

plot(time_trends)
pdf(file = "./figures/populism_months_percent.pdf",   
    width = 12, 
    height = 4)
plot(time_trends)
dev.off()

# Covid split: Months -> # of Paragraphs ----------------------------------

covid_bars <- gcnews %>% 
  group_by(covid, timeframe = lubridate::floor_date(Date, "month")) %>% 
  summarize(
    n_paragraphs = n(),
  ) %>% ungroup %>%
  mutate(
    covid = factor(covid, levels=c("Non-COVID-19", "COVID-19")),
    time_x = month(timeframe) + (year(timeframe) - 2020) * 12 - 1
  ) %>%
  ggplot(aes(x=time_x, y=n_paragraphs, fill=covid)) +
  geom_bar(stat='identity', aes(fill=covid), position="stack", width=1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0)) +
  theme(legend.position = "bottom") +
  xlab("Time (Months)") + ylab("# of Paragraphs") +
  scale_fill_manual(values = c("COVID-19" = "#F8766D", "Non-COVID-19" = "#00BFC4")
  ) + 
  ylim(0,12000) +
  scale_x_continuous(
    breaks = c(0,12,24,36)-.5, minor_breaks = c(0:36 - .5),
    labels = setNames(c("2020", "2021", "2022", ""), c(0,12,24,36)-.5)
  ) +
  geom_vline(xintercept=c(0,12,24,36)-.5) +
  theme(legend.position = c(31.5/36, 9.1/10)) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 11))

plot(covid_bars)
pdf(file = "./figures/covid_months_paragraphs.pdf",  
    width = 12, 
    height = 4) 
plot(covid_bars)
dev.off()

# Economy Paragraphs: Months -> # of Paragraphs
econ_bars <- gcnews %>% 
  group_by(econ, timeframe = lubridate::floor_date(Date, "month")) %>% 
  summarize(
    n_paragraphs = n(),
  ) %>% ungroup %>%
  mutate(
    econ = factor(econ, levels=c("Non-Economy", "Economy")),
    time_x = month(timeframe) + (year(timeframe) - 2020) * 12 - 1
  ) %>%
  ggplot(aes(x=time_x, y=n_paragraphs, fill=econ)) +
  geom_bar(stat='identity', aes(fill=econ), position="stack", width=1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0)) +
  theme(legend.position = "bottom") +
  xlab("Time (Months)") + ylab("# of Paragraphs") +
  scale_fill_manual(values = c("Economy" = "#7CAE00", "Non-Economy" = "#C77CFF")
  ) + 
  ylim(0,12000) +
  scale_x_continuous(
    breaks = c(0,12,24,36)-.5, minor_breaks = c(0:36 - .5),
    labels = setNames(c("2020", "2021", "2022", ""), c(0,12,24,36)-.5)
  ) +
  geom_vline(xintercept=c(0,12,24,36)-.5) +
  theme(legend.position = c(31.5/36, 9.1/10)) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 11))

plot(econ_bars)
pdf(file = "./figures/econ_months_paragraphs.pdf",  
    width = 12, 
    height = 4) 
plot(econ_bars)
dev.off()

