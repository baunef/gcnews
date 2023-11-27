# R
# Baune, Ferdinand
# 20231109 
# Visualizing Dimensions of Populism 

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
# Statistics --------------------------------------------------------------

mean_scores <- gcnews %>%
  summarise(
    mean_anti.elitism = mean(PopBERT_AE) * 100,
    mean_people.centrism = mean(PopBERT_PC) * 100,
    mean_left.wing = mean(PopBERT_LW) * 100,
    mean_right.wing = mean(PopBERT_RW) * 100,
    cf_anti.elitism = qt(0.975,df=n()-1)*sd( PopBERT_AE )/sqrt(n()) * 100,
    cf_people.centrism = qt(0.975,df=n()-1)*sd( PopBERT_PC )/sqrt(n()) * 100,
    cf_left.wing = qt(0.975,df=n()-1)*sd( PopBERT_LW )/sqrt(n()) * 100,
    cf_right.wing = qt(0.975,df=n()-1)*sd( PopBERT_RW )/sqrt(n()) * 100,
  ) %>% 
  pivot_longer(
    cols = 1:8,
    names_pattern = "(.*)_(.*)$",
    names_to = c("type", "dimension"),
    values_to = "values"
  ) %>%
  pivot_wider(
    id_cols = dimension,
    names_from = type,
    values_from = values,
    names_repair = "check_unique"
  ) %>% ungroup %>% as.data.frame()

print(mean_scores)


# Plot Dimensions of Populism ---------------------------------------------

relative_scores_df <- gcnews %>% 
  group_by(timeframe = lubridate::floor_date(Date, "month")) %>% 
  summarize(
    popB_anti = mean( PopBERT_AE ),
    popB_peop = mean( PopBERT_PC ),
    popB_left = mean( PopBERT_LW ),
    popB_rght = mean( PopBERT_RW ),
    cf_anti = qt(0.975,df=n()-1)*sd( PopBERT_AE )/sqrt(n()),
    cf_peop = qt(0.975,df=n()-1)*sd( PopBERT_PC )/sqrt(n()),
    cf_left = qt(0.975,df=n()-1)*sd( PopBERT_LW )/sqrt(n()),
    cf_rght = qt(0.975,df=n()-1)*sd( PopBERT_RW )/sqrt(n()),
  ) %>% ungroup %>%
  mutate(
    popB_anti = popB_anti / first(popB_anti) * 100,
    popB_peop = popB_peop / first(popB_peop) * 100,
    popB_left = popB_left / first(popB_left) * 100,
    popB_rght = popB_rght / first(popB_rght) * 100,
    cf_anti = cf_anti / first(popB_anti) * 100,
    cf_peop = cf_peop / first(popB_peop) * 100,
    cf_left = cf_left / first(popB_left) * 100,
    cf_rght = cf_rght / first(popB_rght) * 100,
  ) %>%
  pivot_longer(
    cols = -1, 
    names_pattern = "(.*)_(.*)$",
    names_to = c("method", "dimension")
  ) %>%
  mutate(method=ifelse(method=="", "value", method)) %>%
  pivot_wider(
    id_cols = c(timeframe, dimension),
    names_from = method,
    values_from = value,
    names_repair = "check_unique"
  ) %>% ungroup %>% arrange(timeframe)

popB_dimensions_rel_months_score <- rbind(
  relative_scores_df, 
  tail(relative_scores_df, 4) %>% dplyr::mutate(timeframe = as.Date("2023-01-01"))
  ) %>%
  ggplot(aes(x=date, y=popB)) +
  geom_vline(xintercept=as.Date("2022-02-24"), color="darkgray") + # Russian War
  geom_vline(xintercept=as.Date("2020-03-11"), color="darkgray") + # Pandemic Announcement
  geom_vline(xintercept=as.Date("2021-03-01"), color="darkgray") + # Mask Affair
  geom_vline(xintercept=as.Date("2021-08-10"), color="darkgray") +   # End of Free Tests
  geom_vline(xintercept=as.Date("2021-11-18"), color="darkgray") +   # End of Free Tests
  geom_stepribbon(aes(x=timeframe,
                      ymin = popB - cf,
                      ymax = popB + cf), alpha=.2) +
  geom_step(aes(x=timeframe, y=popB, color=dimension), 
            direction="hv", linewidth=.6) + 
  facet_grid(factor(dimension, levels=c('anti', 'peop', 'left', 'rght')) ~ .,
             labeller = labeller('anti'='Anti-Elitism', 
                                 'peop'='People-Centrism', 
                                 'left'='Left-Wing', 
                                 'rght'='Right-Wing')) +
  geom_hline(yintercept=100, linewidth=.6) + # Horizontal bar at 100
  theme_minimal() +
  theme(strip.text.y = element_blank()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0)) +
  theme(legend.position = "bottom") +
  labs(x="Time (Months)", y="Relative Mean Score (%)", col="Dimension") +
  scale_colour_discrete(breaks=c('anti', 'peop', 'left', 'rght'),
                        labels=c('Anti-Elitism', 'People-Centrism', 'Left-Wing', 'Right-Wing'))+
  xlim(as.Date("2020-01-01"),as.Date("2023-01-01")) +
  scale_x_continuous(
    breaks=c(as.Date("2020-01-01"), as.Date("2021-01-01"), as.Date("2022-01-01")), 
    labels=c("2020", "2021","2022")
  ) +
  theme(legend.text=element_text(size=12), legend.title = element_blank()) +
  theme(axis.title = element_text(size=12))

plot(popB_dimensions_rel_months_score)
pdf(file = "./figures/popbert_dimensions_months_relative.pdf",   
    width = 9, 
    height = 4.5)
plot(popB_dimensions_rel_months_score)
dev.off()

# Plot Dimensions of Populism with absolute Numbers -----------------------

absolute_scores_df <- gcnews %>% 
  group_by(timeframe = lubridate::floor_date(Date, "month")) %>% 
  summarize(
    popB_anti = mean( PopBERT_AE ) * 100,
    popB_peop = mean( PopBERT_PC ) * 100,
    popB_left = mean( PopBERT_LW ) * 100,
    popB_rght = mean( PopBERT_RW ) * 100,
    cf_anti = qt(0.975,df=n()-1)*sd( PopBERT_AE )/sqrt(n()) * 100,
    cf_peop = qt(0.975,df=n()-1)*sd( PopBERT_PC )/sqrt(n()) * 100,
    cf_left = qt(0.975,df=n()-1)*sd( PopBERT_LW )/sqrt(n()) * 100,
    cf_rght = qt(0.975,df=n()-1)*sd( PopBERT_RW )/sqrt(n()) * 100,
  ) %>% ungroup %>%
  pivot_longer(
    cols = -1, 
    names_pattern = "(.*)_(.*)$",
    names_to = c("method", "dimension")
  ) %>%
  mutate(method=ifelse(method=="", "value", method)) %>%
  pivot_wider(
    id_cols = c(timeframe, dimension),
    names_from = method,
    values_from = value,
    names_repair = "check_unique"
  ) %>% ungroup %>% arrange(timeframe)

popB_dimensions_abs_months_score <- rbind(
  absolute_scores_df, 
  tail(absolute_scores_df, 4) %>% dplyr::mutate(timeframe = as.Date("2023-01-01"))
) %>%
  ggplot(aes(x=date, y=popB)) +
  geom_stepribbon(aes(x=timeframe, fill=dimension,
                      ymin = popB - cf,
                      ymax = popB + cf), alpha=.2) +
  geom_step(aes(x=timeframe, y=popB, color=dimension), 
            direction="hv", linewidth=.6) + 
  facet_grid(factor(dimension, levels=c('anti', 'peop', 'left', 'rght')) ~ .,
             labeller = labeller('anti'='Anti-Elitism', 
                                 'peop'='People-Centrism', 
                                 'left'='Left-Wing', 
                                 'rght'='Right-Wing')) +
  theme_minimal() +
  theme(strip.text.y = element_blank()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0)) +
  theme(legend.position = "bottom") +
  geom_vline(xintercept=as.Date("2022-02-24")) + # Russian War
  geom_vline(xintercept=as.Date("2020-03-11")) + # Pandemic Announcement
  geom_vline(xintercept=as.Date("2021-03-01")) + # Mask Affair
  geom_vline(xintercept=as.Date("2021-08-10")) +   # End of Free Tests
  geom_vline(xintercept=as.Date("2021-11-18")) +   # End of Free Tests
  labs(x="Time (Months)", y="Absolute Mean Score (%)", col="Dimension") +
  scale_colour_discrete(breaks=c('anti', 'peop', 'left', 'rght'),
                        labels=c('Anti-Elitism', 'People-Centrism', 'Left-Wing', 'Right-Wing')) +
  scale_fill_discrete(guide ='none') +
  xlim(as.Date("2020-01-01"),as.Date("2023-01-01")) +
  scale_x_continuous(
    breaks=c(as.Date("2020-01-01"), as.Date("2021-01-01"), as.Date("2022-01-01")), 
    labels=c("2020", "2021","2022")
  ) +
  theme(legend.text=element_text(size=12), legend.title = element_blank()) +
  theme(axis.title = element_text(size=12)) +
  ylim(0,31) + theme(panel.spacing = unit(2, "lines"))

plot(popB_dimensions_abs_months_score)
pdf(file = "./figures/popbert_dimensions_months_absolute.pdf",   
    width = 9, 
    height = 9)
plot(popB_dimensions_abs_months_score)
dev.off()


