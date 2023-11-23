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

# Sigmoid
sigmoid <- function(x) {
  exp(x)/(1+exp(x))
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

gc_news <- read.csv("./data/gcnews/gc_news_labeled.csv", row.names=1) %>%
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

gc_news$covid <- ifelse(gc_news$Dict_Thiele_Covid > 0, "COVID-19", "Non-COVID-19")
gc_news$econ <- ifelse(gc_news$Dict_Thiele_Economy > 0, "Economy", "Non-Economy")
gc_news$source_group <- ifelse(gc_news$Source %in% online_media, "online", "print")

lockdown <- c(
  seq(as.Date("2020-03-16"), as.Date("2020-05-06"), by="days"),
  seq(as.Date("2020-12-16"), as.Date("2021-03-03"), by="days")
)
gc_news$lockdown <- ifelse(gc_news$Date %in% lockdown, "Lockdown", "No Lockdown")
gc_news$pandemic <- ifelse(gc_news$Date < "2022-02-24" & gc_news$Date > "2020-03-11", "Pandemic", "No Pandemic")

gc_news$inc <- -1
for (i in 1:nrow(gc_news)) {
  if (gc_news$Date[i] %in% inc_data$Meldedatum) {
    gc_news$inc[i] <- inc_data$Inzidenz_7.Tage[which(inc_data$Meldedatum == gc_news$Date[i])]
  } else {
    gc_news$inc[i] <- 0
  }
}

gc_news$covid_deaths <- -1
for (i in 1:nrow(gc_news)) {
  if (gc_news$Date[i] %in% death_data$Berichtsdatum) {
    gc_news$covid_deaths[i] <- death_data$Todesfaelle_neu[which(death_data$Berichtsdatum == gc_news$Date[i])]
  } else {
    gc_news$covid_deaths[i] <- 0
  }
}


# Summary Stats -----------------------------------------------------------

par_df <- gc_news %>%
  group_by(Source) %>%
  summarise(
    n_paragraphs = n()
  )

art_df <- gc_news %>% 
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
reg_df <- gc_news %>%
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
reg_art_df <- gc_news %>% 
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


# Single Models (Paragraph) -----------------------------------------------

controls <- c("var_covid", "var_econ", "var_lockdown", 
              "var_war", "var_date", "var_source", "var_inc", 
              "var_death",
              "var_springer", "var_cons", "var_left", "var_tabloid"
)
new_names <- c("COVID-19", "Economy", "Lockdown", 
               "War", "Date", "Print/Online", "7-day Incidence",
               "Fatalities",
               "Axel Springer SE", "Conservative", "Left", "Tabloid"
)
model_sum <- data.frame(
  Estimate=double(),
  Std..Error=double(),
  t.value=double(),
  Pr...t..=double(),
  stringsAsFactors=FALSE
) 
for (c in controls) {
  temp_model <- glm(
    build_formula(dv = "pop_score", iv = c),
    family = binomial(),
    data = reg_df
  )
  temp_df <-  data.frame(summary(temp_model)$coefficients)[2,]
  model_sum <- rbind(model_sum, temp_df)
}
model_sum <- cbind(Name = rownames(model_sum), model_sum)
rownames(model_sum) <- 1:nrow(model_sum)
model_sum$Significance <- ifelse(model_sum$Pr...z.. < 0.05, "p < 0.05", "p > 0.05")

par_model <- model_sum %>% 
  mutate(
    Variable = new_names[which(controls == Name)]
  ) %>%
  ggplot(aes(x=Estimate, y=Variable)) + 
  geom_vline(xintercept=0) +
  geom_linerange(aes(x=Estimate, y=factor(Variable, 
                                          rev(c("Date", "7-day Incidence", 
                                                "Fatalities",
                                                "Lockdown", "War", "Tabloid", 
                                                "Axel Springer SE", "Conservative", 
                                                "Left", "Print/Online", "COVID-19", 
                                                "Economy"
                                          ))
                                          ), 
                      xmin=Estimate-qt(0.975,df=nrow(reg_df)-1)*Std..Error, xmax=Estimate+qt(0.975,df=nrow(reg_df)-1)*Std..Error,
                      color=Significance,
                      ),
                  linewidth=2
  ) +
  geom_point(aes(x=Estimate, y=Variable)) +
  xlim(-.3,1.06) +
  theme_minimal() +
  labs(x = "Coefficient (95 % CI)", 
       y = "", 
       title = "Paragraph-level") +
  theme(legend.position = "none", axis.title.x=element_text(size=9),
        plot.title = element_text(hjust = .5))

tab_paragraph <- model_sum %>% 
  mutate(
    Variable = new_names[which(controls == Name)]
  ) %>% select(
    c(Variable, Estimate, Std..Error, Pr...z..)
  )

cat("Paragraph-level Regression: \n\n")
tab_paragraph


# Single Models (Article) -------------------------------------------------

controls <- c("var_covid", "var_econ", "var_lockdown", 
              "var_war", "var_date", "var_source", "var_inc", 
              "var_death",
              "var_springer", "var_cons", "var_left", "var_tabloid"
)
new_names <- c("COVID-19", "Economy", "Lockdown", 
               "War", "Date", "Print/Online", "7-day Incidence",
               "Fatalities",
               "Axel Springer SE", "Conservative", "Left", "Tabloid"
)
art_model_sum <- data.frame(
  Estimate=double(),
  Std..Error=double(),
  t.value=double(),
  Pr...t..=double(),
  stringsAsFactors=FALSE
) 
for (c in controls) {
  temp_model <- glm(
    build_formula(dv = "pop_score", iv = c),
    family = quasibinomial(),
    data = reg_art_df
  )
  temp_df <-  data.frame(summary(temp_model)$coefficients)[2,]
  art_model_sum <- rbind(art_model_sum, temp_df)
}
art_model_sum <- cbind(Name = rownames(art_model_sum), art_model_sum)
rownames(art_model_sum) <- 1:nrow(art_model_sum)
art_model_sum$Significance <- ifelse(art_model_sum$Pr...t.. < 0.05, "p < 0.05", "p > 0.05")

art_model <- art_model_sum %>% 
  mutate(
    Variable = controls[which(controls == Name)]
  ) %>%
  ggplot(aes(x=Estimate, y=Variable)) + 
  geom_vline(xintercept=0) +
  geom_linerange(aes(x=Estimate, y=factor(Variable, 
                                          rev(c("Date", "7-day Incidence", 
                                                "Fatalities",
                                                "Lockdown", "War", "Tabloid", 
                                                "Axel Springer SE", "Conservative", 
                                                "Left", "Print/Online", "COVID-19", 
                                                "Economy"
                                            ))
                                          ), 
                     xmin=Estimate-qt(0.975,df=nrow(reg_art_df)-1)*Std..Error, xmax=Estimate+qt(0.975,df=nrow(reg_art_df)-1)*Std..Error,
                     color=Significance,
  ),
  linewidth=2
  ) +
  geom_point(aes(x=Estimate, y=Variable)) +
  theme_minimal() +
  # xlim(-.3,1.06) + 
  labs(x = "Coefficient (95 % CI)", 
       y = "", 
       title = "Article-level") +
  theme(legend.position = "none", axis.title.x=element_text(size=9),
        plot.title = element_text(hjust = .5))

tab_article <- art_model_sum %>% 
  mutate(
    Variable = new_names[which(controls == Name)]
  ) %>% select(
    c(Variable, Estimate, Std..Error, Pr...t..)
  )

cat("Article-level Regression: \n\n")
tab_article


# Combined Plot -----------------------------------------------------------

library(gridExtra)
grid.arrange(par_model, art_model, ncol=2,
             top = "Individual Models"
             )
grid.arrange(mv_par_model, mv_art_model, ncol=2, 
             top = "Combined Model"
             )

tab_complete <- left_join(tab_paragraph, tab_article, by="Variable", suffix=c("_par", "_art"))

rbind(stats_online, stats_print) %>% mutate(pct_paragraphs = n_paragraphs / sum(rbind(stats_online, stats_print)$n_paragraphs)*100, pct_articles = n_articles / sum(rbind(stats_online, stats_print)$n_articles)*100, diff = -pct_paragraphs + pct_articles)



# LaTeX Output ------------------------------------------------------------

library(texreg)

mytable <- texreg(list(multivar_par_model, multivar_art_model, model_sum, art_model_sum), label = "tab:regression_results",
                  caption = "Bolded coefficients, custom notes, three digits.",
                  float.pos = "!h", return.string = TRUE, bold = 0.05, stars = 0,
                  custom.note = "Coefficients with $p < 0.05$ in \\textbf{bold}.",
                  digits = 3, leading.zero = FALSE, omit.coef = "Inter")








