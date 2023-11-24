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
# Statistics and Numbers --------------------------------------------------

media_scores_par <- gcnews %>% 
  group_by(Source) %>% 
  summarize(
    par_mean = mean( PopBERT_AE + PopBERT_PC > 1 ) * 100,
    par_cf = qt(0.975,df=n()-1)*sd(PopBERT_AE + PopBERT_PC > 1)/sqrt(n()) * 100
  ) %>% ungroup %>% arrange(desc(par_mean)) %>%
  mutate(
    par_mean = round(par_mean, digits=2),
    par_cf = round(par_cf, digits=2)
  )

media_scores_art <- gcnews %>%
  group_by(Title, Date, Source, Author) %>%
  reframe(
    pop_score = (PopBERT_AE + PopBERT_PC > 1) / n()
  ) %>%
  group_by(Source) %>% 
  summarize(
    article_mean = mean( pop_score ) * 100,
    article_cf = qt(0.975,df=n()-1)*sd( pop_score )/sqrt(n()) * 100
  ) %>% ungroup %>% arrange(desc(article_mean)) %>%
  mutate(
    article_mean = round(article_mean, digits=4),
    article_cf = round(article_cf, digits=4)
  )

media_score <- merge(media_scores_par, media_scores_art) %>%
  arrange(desc(par_mean))

media_score %>% 
  ggplot(aes(x=par_mean, y=Source)) + 
  geom_point() +
  geom_bar(stat = "identity") + 
  geom_point(aes(x=article_mean), color="black") +
  geom_bar(stat = "identity", aes(x=article_mean, y=Source), color="navy") + 
  theme_minimal()

print(media_score)


# Visualization -----------------------------------------------------------

source_df <- gcnews %>% 
  group_by(Source, timeframe = lubridate::floor_date(Date, "quarter")) %>% 
  summarize(
    n_par = n(),
    n_pop = mean( PopBERT_AE + PopBERT_PC > 1 ),
    cf_pop_upper = ifelse(
      qt(0.975,df=n()-1)*sd(PopBERT_AE + PopBERT_PC > 1)/sqrt(n()) < 1,
      qt(0.975,df=n()-1)*sd(PopBERT_AE + PopBERT_PC > 1)/sqrt(n()),
      1
    ) ,
    cf_pop_lower = ifelse(
      mean( PopBERT_AE + PopBERT_PC > 1 ) - qt(0.975,df=n()-1)*sd(PopBERT_AE + PopBERT_PC > 1)/sqrt(n()) > 0,
      qt(0.975,df=n()-1)*sd(PopBERT_AE + PopBERT_PC > 1)/sqrt(n()),
      mean( PopBERT_AE + PopBERT_PC > 1 )
    )
  ) %>% ungroup %>% arrange(timeframe)

source_trends <- rbind(
  source_df, 
  tail(source_df, 10) %>% dplyr::mutate(timeframe= as.Date("2023-01-01"))
) %>%
  dplyr::filter(n_par > 50) %>%
  ggplot(aes(x=timeframe, y=n_pop)) +
  geom_step(aes(x=timeframe, y=n_pop*100), direction="hv") + 
  geom_stepribbon(aes(x=timeframe,
                      ymin = n_pop*100 - cf_pop_lower*100,
                      ymax = n_pop*100 + cf_pop_upper*100), alpha=.2) +
  facet_wrap( ~ factor(
    Source,
    levels=c("taz - die tageszeitung", "Süddeutsche Zeitung","Der Tagesspiegel","Die Welt", "BILD",
             "Spiegel Online", "Süddeutsche Zeitung Online", "Der Tagesspiegel Online", "Focus Online", "bild.de")),
    ncol = 5
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0)) +
  theme(legend.position = "bottom") +
  ylim(0,35) + xlim(as.Date("2020-01-01"), as.Date("2022-12-31")) +
  xlab("Time (Quarters)") + ylab("Quarterly Additive Populism Index (%)") +
  scale_x_continuous(
    breaks=c(as.Date("2020-01-01"), as.Date("2021-01-01"), as.Date("2022-01-01")), 
    labels=c("2020", "2021","2022")
  )

plot(source_trends)
pdf(file = "./figures/sources_quarters_populism.pdf",   
    width = 12, 
    height = 4)
plot(source_trends)
dev.off()
