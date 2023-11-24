# R
# Baune, Ferdinand
# 20231123 
# Dictionary Classification 

rm(list = ls())
setwd("./")

# Packages ----------------------------------------------------------------

library(tidyverse)
library(quanteda)
library(popdictR)
library(multidictR)
print("========== Packages loaded ==========")

# Load CSV ----------------------------------------------------------------

# gcnews <- read.csv("./data/gcnews/gc_news_unlabeled.csv") # Uncomment, if full unlabeled file is available. 
gcnews <- read.csv("./data/gcnews/gc_news_unlabeled_sample1000.csv")


covid_terms <- read.csv("./text_classification/dictionaries/thiele_covid_terms.csv")$terms %>% 
  as.vector() %>% 
  str_replace_all(pattern = "\\*", replacement = "(.*)")

econ_terms <- read.csv("./text_classification/dictionaries/thiele_econ_terms.csv")$terms %>% 
  as.vector() %>% 
  str_replace_all(pattern = "\\*", replacement = "(.*)")

corp_news <- corpus(gcnews, text_field = "Content")
corp_title <- corpus(gcnews, text_field = "Title")


print("========== Data read ==========")
# popdictR ----------------------------------------------------------------

start <- Sys.time()
cat("\t\t", format(start, "%H:%M:%S"), "\n")
grundl_results <- run_popdict(corp_news, 
                              at_level = "documents")
cat("\t\t", format(Sys.time(), "%H:%M:%S"), "\n")
cat("Gründl done in ", as.character.Date((Sys.time() - start)), ".\n", sep="")

start <- Sys.time()
cat("\t\t", format(start, "%H:%M:%S"), "\n")
covid_results <- multidictR::run_multidict(
  corpus = corp_news,
  dict = covid_terms,
  at_level = "documents",
  return_value = "count",
  include_totals = FALSE,
  return_result_only = FALSE,
  pattern_type = "regex",
  case_insensitive = TRUE,
  regex_optimize = TRUE,
  regex_make_greedy = FALSE,
  regex_make_lazy = TRUE,
  dict_name = "thiele_covid_terms_par",
  tolower = TRUE,
  stem = FALSE,
  remove = NULL,
  what = "word",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE,
  split_hyphens = FALSE,
  include_docvars = TRUE
)

cat("\t\t", format(Sys.time(), "%H:%M:%S"), "\n")
cat("Covid Pars done in ", as.character.Date((Sys.time() - start)), ".\n", sep="")

start <- Sys.time()
cat("\t\t", format(start, "%H:%M:%S"), "\n")
covid_titles <- multidictR::run_multidict(
  corpus = corp_title,
  dict = covid_terms,
  at_level = "documents",
  return_value = "count",
  include_totals = FALSE,
  return_result_only = FALSE,
  pattern_type = "regex",
  case_insensitive = TRUE,
  regex_optimize = TRUE,
  regex_make_greedy = FALSE,
  regex_make_lazy = TRUE,
  dict_name = "thiele_covid_terms_title",
  tolower = TRUE,
  stem = FALSE,
  remove = NULL,
  what = "word",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE,
  split_hyphens = FALSE,
  include_docvars = TRUE
)

cat("\t\t", format(Sys.time(), "%H:%M:%S"), "\n")
cat("Covid Titles done in ", as.character.Date((Sys.time() - start)), ".\n", sep="")

start <- Sys.time()
cat("\t\t", format(start, "%H:%M:%S"), "\n")
econ_results <- multidictR::run_multidict(
  corpus = corp_news,
  dict = econ_terms,
  at_level = "documents",
  return_value = "count",
  include_totals = FALSE,
  return_result_only = FALSE,
  pattern_type = "regex",
  case_insensitive = TRUE,
  regex_optimize = TRUE,
  regex_make_greedy = FALSE,
  regex_make_lazy = TRUE,
  dict_name = "thiele_econ_terms_par",
  tolower = TRUE,
  stem = FALSE,
  remove = NULL,
  what = "word",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE,
  split_hyphens = FALSE,
  include_docvars = TRUE
)

cat("\t\t", format(Sys.time(), "%H:%M:%S"), "\n")
cat("Econ Pars done in ", as.character.Date((Sys.time() - start)), ".\n", sep="")

start <- Sys.time()
cat("\t\t", format(start, "%H:%M:%S"), "\n")
econ_titles <- multidictR::run_multidict(
  corpus = corp_title,
  dict = econ_terms,
  at_level = "documents",
  return_value = "count",
  include_totals = FALSE,
  return_result_only = FALSE,
  pattern_type = "regex",
  case_insensitive = TRUE,
  regex_optimize = TRUE,
  regex_make_greedy = FALSE,
  regex_make_lazy = TRUE,
  dict_name = "thiele_econ_terms_title",
  tolower = TRUE,
  stem = FALSE,
  remove = NULL,
  what = "word",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE,
  split_hyphens = FALSE,
  include_docvars = TRUE
)

cat("\t\t", format(Sys.time(), "%H:%M:%S"), "\n")
cat("Econ Titles done in ", as.character.Date((Sys.time() - start)), ".\n", sep="")

print("========== Dictionary search completed ==========")
# Labeled Set -------------------------------------------------------------

grundl_results <- convert(grundl_results, to = "data.frame")
covid_results <- convert(covid_results, to = "data.frame")
covid_titles <- convert(covid_titles, to = "data.frame")
econ_results <- convert(econ_results, to = "data.frame")
econ_titles <- convert(econ_titles, to = "data.frame")

all_results <- cbind(
  grundl_results, 
  Dict_Thiele_Covid = covid_titles$thiele_covid_terms_title + covid_results$thiele_covid_terms_par,
  Dict_Thiele_Economy = econ_titles$thiele_econ_terms_title + econ_results$thiele_econ_terms_par
) %>% rename(Dict_Gruendl = dict_gruendl_2020)

# write.csv(all_results, "./text_classification/dictionaries/gc_news_dictionary_labels.csv", row.names = FALSE)

