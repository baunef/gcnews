# R
# Baune, Ferdinand
# 20231020 
# Regression Model

rm(list = ls())
setwd("./")

# Packages ----------------------------------------------------------------

library(tidyverse)
library(gridExtra)
library(margins)
print("========== Packages loaded ==========")


# Functions and Constants -------------------------------------------------

# Build formula from character vectors of LHS (DV) and RHS (IV)
build_formula <- function(dv, iv) {
  specification <- paste(dv, paste(iv, collapse = " + "), sep = " ~ ")
  
  as.formula(specification)
}

floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

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

gcnews <- read.csv("./data/gcnews/gc_news_labeled.csv", row.names=1) %>%
  mutate(
    Date = as.Date(Date)
  )

inc_data <- read.csv("./data/external/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv") %>%
  filter(Altersgruppe == "00+") %>%
  dplyr::select(-c("Altersgruppe")) %>%
  mutate(
    Meldedatum = as.Date(Meldedatum)
  )

death_data <- read.csv("./data/external/COVID-19-Todesfaelle_Deutschland.csv") %>%
  mutate(
    Berichtsdatum = as.Date(Berichtsdatum)
  )

print("========== Data read ==========")
# Explanatory Variables ---------------------------------------------------

gcnews$covid <- ifelse(gcnews$Dict_Thiele_Covid > 0, "COVID-19", "Non-COVID-19")
gcnews$econ <- ifelse(gcnews$Dict_Thiele_Economy > 0, "Economy", "Non-Economy")
gcnews$source_group <- ifelse(gcnews$Source %in% online_media, "online", "print")

lockdown <- c(
  seq(as.Date("2020-03-16"), as.Date("2020-05-06"), by="days"),
  seq(as.Date("2020-12-16"), as.Date("2021-03-03"), by="days")
)
gcnews$lockdown <- ifelse(gcnews$Date %in% lockdown, "Lockdown", "No Lockdown")
gcnews$pandemic <- ifelse(gcnews$Date < "2022-02-24" & gcnews$Date > "2020-03-11", "Pandemic", "No Pandemic")

gcnews$inc <- -1
for (i in 1:nrow(gcnews)) {
  if (gcnews$Date[i] %in% inc_data$Meldedatum) {
    gcnews$inc[i] <- inc_data$Inzidenz_7.Tage[which(inc_data$Meldedatum == gcnews$Date[i])]
  } else {
    gcnews$inc[i] <- 0
  }
}

gcnews$covid_deaths <- -1
for (i in 1:nrow(gcnews)) {
  if (gcnews$Date[i] %in% death_data$Berichtsdatum) {
    gcnews$covid_deaths[i] <- death_data$Todesfaelle_neu[which(death_data$Berichtsdatum == gcnews$Date[i])]
  } else {
    gcnews$covid_deaths[i] <- 0
  }
}


# Summary Stats -----------------------------------------------------------

par_df <- gcnews %>%
  group_by(Source) %>%
  summarise(
    n_paragraphs = n()
  )

art_df <- gcnews %>% 
  group_by(Title, Date, Source, Author, source_group, inc, covid_deaths, lockdown) %>%
  mutate(
    add_pop = (PopBERT_AE + PopBERT_PC) > 1,
    springer = (Source %in% c("BILD", "bild.de", "Die Welt")) * 1,
    cons = (Source %in% c("BILD", "bild.de", "Die Welt", "Focus Online")) * 1,
    left = (Source %in% c("Süddeutsche Zeitung", "Süddeutsche Zeitung Online", "Spiegel Online", "taz - die tageszeitung")) * 1,
    tabloid = (Source %in% c("BILD", "bild.de")) * 1,
  ) %>%
  summarise(
    pop_score = sum(add_pop) / n(),
  ) %>% ungroup %>%
  group_by(Source) %>%
  summarise(
    n_articles = n()
  )

stats_online <- left_join(par_df, art_df, "Source") %>%
  filter(Source %in% online_media)

stats_print <- left_join(par_df, art_df, "Source") %>%
  filter(Source %in% print_media)

stats_complete <- rbind(stats_online, stats_print) %>% 
  mutate(
    pct_paragraphs = n_paragraphs / sum(rbind(stats_online, stats_print)$n_paragraphs)*100, 
    pct_articles = n_articles / sum(rbind(stats_online, stats_print)$n_articles)*100, 
    diff = -pct_paragraphs + pct_articles
    )



# Regression Data ---------------------------------------------------------

# paragraph-level
reg_df <- gcnews %>%
  reframe(
    pop_score = (PopBERT_AE + PopBERT_PC > 1) * 1,
    var_covid = (covid == "COVID-19") * 1,
    var_econ = (econ == "Economy") * 1,
    var_lockdown = (lockdown == "Lockdown") * 1,
    var_war = (Date > as.Date("2022-02-24")) * 1,
    var_date = as.integer(Date - as.Date("2020-03-11")),
    var_inc = inc,
    var_death = covid_deaths,
    var_source = (source_group == "print") * 1,
    var_springer = (Source %in% c("BILD", "bild.de", "Die Welt")) * 1,
    var_cons = (Source %in% c("BILD", "bild.de", "Die Welt", "Focus Online")) * 1,
    var_left = (Source %in% c("Süddeutsche Zeitung", "Süddeutsche Zeitung Online", "Spiegel Online", "taz - die tageszeitung")) * 1,
    var_tabloid = (Source %in% c("BILD", "bild.de")) * 1,
  )

# article-level
reg_art_df <- gcnews %>% 
  group_by(Title, Date, Source, Author, source_group, inc, covid_deaths, lockdown) %>%
  mutate(
    add_pop = (PopBERT_AE + PopBERT_PC) > 1,
    springer = (Source %in% c("BILD", "bild.de", "Die Welt")) * 1,
    cons = (Source %in% c("BILD", "bild.de", "Die Welt", "Focus Online")) * 1,
    left = (Source %in% c("Süddeutsche Zeitung", "Süddeutsche Zeitung Online", "Spiegel Online", "taz - die tageszeitung")) * 1,
    tabloid = (Source %in% c("BILD", "bild.de")) * 1,
  ) %>%
  summarise(
    pop_score = sum(add_pop) / n(),
    var_covid = (sum(covid == "COVID-19") > 0) * 1,
    var_econ = (sum(econ == "Economy") > 0) * 1,
    var_lockdown = (sum(lockdown == "Lockdown") > 0) * 1,
    var_war = (min(Date) > as.Date("2022-02-24")) * 1,
    var_date = as.integer(min(Date) - as.Date("2020-03-11")),
    var_inc = mean(inc),
    var_death = mean(covid_deaths),
    var_source = (sum(str_count(source_group, "print")) > 0) * 1,
    var_springer = (sum(springer) > 0) * 1,
    var_cons = (sum(cons) > 0) * 1,
    var_left = (sum(left) > 0) * 1,
    var_tabloid = (sum(tabloid) > 0) * 1,
  ) %>% ungroup %>%
  dplyr::select(-c(
    "Title",
    "Author",
    "Source",
    "source_group",
    "inc", "covid_deaths",
    "lockdown"
  ))


# Multivariable Model (Paragraph) -----------------------------------------

controls <- c(
  "var_covid" = "Covid-19", 
  "var_econ" = "Economy", 
  "var_lockdown" = "Lockdown",
  "var_war" = "War", 
  "var_date" = "Date", 
  "var_source" = "Print", 
  "var_inc" = "7-Day Incidence",
  "var_death" = "Casualty Rate",
  "var_springer" = "Axel Springer SE", 
  "var_cons" = "Conservative", 
  "var_left" = "Left", 
  "var_tabloid" = "Tabloid"
)

multivar_par_model <- glm(
  build_formula(dv = "pop_score", iv = names(controls)),
  family = binomial(),
  data = reg_df
)

multivar_par_summary <- as.data.frame(margins_summary(multivar_par_model))

mv_par_model <- multivar_par_summary %>%
  mutate(
    "Variable" = controls[factor],
    "Significance" = p > .05
  ) %>%
  ggplot(aes(x=AME, y=factor(Variable, 
                             rev(c("Date", "7-Day Incidence", "Casualty Rate",
                                   "Lockdown", "War", "Tabloid", 
                                   "Axel Springer SE", "Conservative", 
                                   "Left", "Print", "Covid-19", 
                                   "Economy"
                             ))
  )
  )
  ) + 
  geom_vline(xintercept=0) +
  geom_linerange(aes(x=AME, y=factor(Variable,
                                     rev(c("Date", "7-Day Incidence", "Casualty Rate",
                                           "Lockdown", "War", "Tabloid",
                                           "Axel Springer SE", "Conservative",
                                           "Left", "Print", "Covid-19",
                                           "Economy"
                                     ))
  ),
  xmin=lower, xmax=upper,
  color=Significance,
  ),
  linewidth=2
  ) +
  geom_point(aes(x=AME, y=Variable)) +
  xlim(floor_dec(min(multivar_par_summary$lower), 2),ceiling_dec(max(multivar_par_summary$upper), 2)) +
  theme_minimal() +
  labs(x = "Average Marginal Effects (95 % CI)", 
       y = "", 
       title = "Paragraph-level") +
  theme(legend.position = "none", axis.title.x=element_text(size=9),
        plot.title = element_text(hjust = .5))

# Multivariable Model (Article) -------------------------------------------

controls <- c(
  "var_covid" = "Covid-19", 
  "var_econ" = "Economy", 
  "var_lockdown" = "Lockdown",
  "var_war" = "War", 
  "var_date" = "Date", 
  "var_source" = "Print", 
  "var_inc" = "7-Day Incidence",
  "var_death" = "Casualty Rate",
  "var_springer" = "Axel Springer SE", 
  "var_cons" = "Conservative", 
  "var_left" = "Left", 
  "var_tabloid" = "Tabloid"
)

multivar_art_model <- glm(
  build_formula(dv = "pop_score", iv = names(controls)),
  family = quasibinomial(),
  data = reg_art_df
)

multivar_art_summary <- as.data.frame(margins_summary(multivar_art_model))

mv_art_model <- multivar_art_summary %>%
  mutate(
    "Variable" = controls[factor],
    "Significance" = p > .05
  ) %>%
  ggplot(aes(x=AME, y=factor(Variable, 
                             rev(c("Date", "7-Day Incidence", "Casualty Rate",
                                   "Lockdown", "War", "Tabloid", 
                                   "Axel Springer SE", "Conservative", 
                                   "Left", "Print", "Covid-19", 
                                   "Economy"
                                   ))
                             )
             )
         ) + 
  geom_vline(xintercept=0) +
  geom_linerange(aes(x=AME, y=factor(Variable,
                                          rev(c("Date", "7-Day Incidence", "Casualty Rate",
                                                "Lockdown", "War", "Tabloid",
                                                "Axel Springer SE", "Conservative",
                                                "Left", "Print", "Covid-19",
                                                "Economy"
                                          ))
                                     ),
                     xmin=lower, xmax=upper,
                     color=Significance,
                     ),
                 linewidth=2
                 ) +
  geom_point(aes(x=AME, y=Variable)) +
  xlim(floor_dec(min(multivar_art_summary$lower), 2),ceiling_dec(max(multivar_art_summary$upper), 2)) +
  theme_minimal() +
  labs(x = "Average Marginal Effects (95 % CI)", 
       y = "", 
       title = "Article-level") +
  theme(legend.position = "none", axis.title.x=element_text(size=9),
        plot.title = element_text(hjust = .5))

# Combined Plot -----------------------------------------------------------

grid.arrange(mv_par_model, mv_art_model, ncol=2)

pdf(file = "./figures/multi_models_ame_variable.pdf",   
    width = 9, 
    height = 3)
grid.arrange(mv_par_model, mv_art_model, ncol=2)
dev.off()

tab_stats <- rbind(stats_online, stats_print) %>% 
  mutate(
    pct_paragraphs = n_paragraphs / sum(rbind(stats_online, stats_print)$n_paragraphs)*100, 
    pct_articles = n_articles / sum(rbind(stats_online, stats_print)$n_articles)*100, 
    diff = -pct_paragraphs + pct_articles)

print(tab_stats) # GC News Statistics


# Export Results ----------------------------------------------------------

mv_par_df <- as.data.frame(summary(multivar_par_model)$coefficients)
mv_art_df <- as.data.frame(summary(multivar_art_model)$coefficients)

colnames(mv_par_df) <- paste0("par_", names(mv_par_df))
colnames(mv_art_df) <- paste0("art_", names(mv_art_df))

mv_df <- cbind(
  mv_par_df, 
  mv_art_df
  ) %>% 
  mutate(
    Variable = c("(Intercept)", unname(controls[rownames(mv_par_df)])[2:13]),
    par_Estimate = sprintf("%.2e",par_Estimate) %>% as.character(), 
    par_SE = sprintf("%.2e",`par_Std. Error`) %>% as.character(), 
    par_p = sprintf("%.2e",`par_Pr(>|z|)`) %>% as.character(),
    art_Estimate = sprintf("%.2e",art_Estimate) %>% as.character(), 
    art_SE = sprintf("%.2e",`art_Std. Error`) %>% as.character(),
    art_p = sprintf("%.2e",`art_Pr(>|t|)`) %>% as.character()
  ) %>%
  select(
    Variable, 
    par_Estimate,
    par_SE,
    # par_p,
    art_Estimate,
    art_SE,
    # art_p
  ) %>%
  arrange(Variable)

# LaTeX Output
for (i in 1:nrow(mv_df)) {
  cat(str_c(mv_df[i,], collapse = "\t & "), "\\\\ \n")
}

colnames(multivar_par_summary) <- paste0("par_", names(multivar_par_summary))
colnames(multivar_art_summary) <- paste0("art_", names(multivar_art_summary))

ame_df <- cbind(
  multivar_par_summary, 
  multivar_art_summary
) %>% 
  mutate(
    Variable = controls[par_factor],
    par_AME = sprintf("%.2e",par_AME) %>% as.character(), 
    par_SE = sprintf("%.2e",`par_SE`) %>% as.character(), 
    par_p = sprintf("%.2e",`par_p`) %>% as.character(),
    art_AME = sprintf("%.2e",art_AME) %>% as.character(), 
    art_SE = sprintf("%.2e",`art_SE`) %>% as.character(),
    art_p = sprintf("%.2e",`art_p`) %>% as.character()
  ) %>%
  select(
    Variable, 
    par_AME,
    par_SE,
    # par_p,
    art_AME,
    art_SE,
    # art_p
  ) %>%
  arrange(Variable)

# Latex tabular output
for (i in 1:nrow(ame_df)) {
  cat(str_c(ame_df[i,], collapse = "\t & "), "\\\\ \n")
}

