# R
# Baune, Ferdinand
# 20231110 
# Classifier Performance and Prediction Stability

rm(list = ls())
setwd("./")

# Packages ----------------------------------------------------------------

library(tidyverse)
library(cowplot)
cat("========== Packages loaded ==========\n")


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

cat("========== Data read ==========\n")
# Scores and Statistics ---------------------------------------------------

source_level_stats <- gcnews %>% 
  group_by(Source) %>%
  summarize(
    adict_norm  = mean( Dict_Gruendl/Sentences/max(gcnews$Dict_Gruendl / gcnews$Sentences) )  * 100,
    bembed_norm = mean(W2V_Labels > .5) * 100,
    cgzip_norm = mean(Gzip_Labels) * 100,
    dsetfit_norm = mean(SetFit_Labels > .5) * 100,
    epopBa_norm = mean( PopBERT_AE + PopBERT_PC > 1 ) * 100,
    epopBm_norm = mean( PopBERT_AE * PopBERT_PC > .19 ) * 100,
    
    adict_cf = qt(0.975,df=n()-1) * sd(Dict_Gruendl/Sentences/max(gcnews$Dict_Gruendl / gcnews$Sentences) )/sqrt(n()) * 100,
    bembed_cf = qt(0.975,df=n()-1) * sd(W2V_Labels > .5)/sqrt(n()) * 100,
    cgzip_cf = qt(0.975,df=n()-1) * sd(Gzip_Labels)/sqrt(n()) * 100,
    dsetfit_cf = qt(0.975,df=n()-1) * sd(SetFit_Labels > .5)/sqrt(n()) * 100,
    epopBm_cf = qt(0.975,df=n()-1) * sd(PopBERT_AE + PopBERT_PC > 1)/sqrt(n()) * 100,
    epopBa_cf = qt(0.975,df=n()-1) * sd(PopBERT_AE * PopBERT_PC > .19)/sqrt(n()) * 100,
  ) %>%
  pivot_longer(
    cols = -1, 
    names_pattern = "(.*)_(.*)$",
    names_to = c("method", "name")
  ) %>%
  mutate(method=ifelse(method=="", "value", method)) %>%
  pivot_wider(
    id_cols = c(Source, method),
    names_from = name,
    values_from = value,
    names_repair = "check_unique"
  ) %>% 
  pivot_wider(
    names_from = method,
    values_from = c(norm, cf),
    names_vary = "slowest"
  )

methods_par_values <- gcnews %>% 
  summarize(
    adict_norm  = mean( Dict_Gruendl/Sentences/max(gcnews$Dict_Gruendl / gcnews$Sentences) > .19 )  * 100,
    bembed_norm = mean(W2V_Labels > .5) * 100,
    cgzip_norm = mean(Gzip_Labels) * 100,
    dsetfit_norm = mean(SetFit_Labels > .5) * 100,
    epopBa_norm = mean( PopBERT_AE + PopBERT_PC > 1 ) * 100,
    epopBm_norm = mean( PopBERT_AE * PopBERT_PC > .19 ) * 100,
    
    adict_cf = qt(0.975,df=n()-1) * sd(Dict_Gruendl/Sentences/max(gcnews$Dict_Gruendl / gcnews$Sentences) )/sqrt(n()) * 100,
    bembed_cf = qt(0.975,df=n()-1) * sd(W2V_Labels > .5)/sqrt(n()) * 100,
    cgzip_cf = qt(0.975,df=n()-1) * sd(Gzip_Labels)/sqrt(n()) * 100,
    dsetfit_cf = qt(0.975,df=n()-1) * sd(SetFit_Labels > .5)/sqrt(n()) * 100,
    epopBa_cf = qt(0.975,df=n()-1) * sd(PopBERT_AE + PopBERT_PC > 1)/sqrt(n()) * 100,
    epopBm_cf = qt(0.975,df=n()-1) * sd(PopBERT_AE * PopBERT_PC > .19)/sqrt(n()) * 100,
  ) %>%
  pivot_longer(
    cols=1:12,
    names_pattern = "(.*)_(.*)$",
    names_to = c("method", "name"),
  ) %>%
  mutate(method=ifelse(method=="", "value", method)) %>%
  pivot_wider(
    id_cols = c(method),
    names_from = name,
    values_from = value,
    names_repair = "check_unique"
  )



methods_art_values <- gcnews %>% 
  group_by(Title, Date, Source, Author) %>%
  summarise(
    art_dict = sum(Dict_Gruendl/Sentences/max(gcnews$Dict_Gruendl / gcnews$Sentences) > .19),
    art_w2v  = sum(W2V_Labels > .5),
    art_gzip = sum(Gzip_Labels),
    art_sfit = sum(SetFit_Labels > .5),
    art_popBa = sum(PopBERT_AE + PopBERT_PC > 1),
    art_popBm = sum(PopBERT_AE * PopBERT_PC > .19),
    art_n_par = n(),
    art_n_sen = sum(Sentences),
  ) %>% ungroup %>%
  summarise(
    mean_aDict = mean(art_dict / art_n_par) * 100,
    mean_bW2V  = mean(art_w2v / art_n_par) * 100,
    mean_cGzip = mean(art_gzip / art_n_par) * 100,
    mean_dSetFit = mean(art_sfit / art_n_par) * 100,
    mean_ePopBERTa = mean(art_popBa / art_n_par) * 100,
    mean_ePopBERTm = mean(art_popBm / art_n_par) * 100,

    ci_aDict = qt(0.975,df=n()-1) * sd(art_dict / art_n_par)/sqrt(n()) * 100,
    ci_bW2V  = qt(0.975,df=n()-1) * sd(art_w2v / art_n_par)/sqrt(n()) * 100,
    ci_cGzip = qt(0.975,df=n()-1) * sd(art_gzip / art_n_par)/sqrt(n()) * 100,
    ci_dSetFit = qt(0.975,df=n()-1) * sd(art_sfit / art_n_par)/sqrt(n()) * 100,
    ci_ePopBERTa = qt(0.975,df=n()-1) * sd(art_popBa / art_n_par)/sqrt(n()) * 100,
    ci_ePopBERTm = qt(0.975,df=n()-1) * sd(art_popBm / art_n_par)/sqrt(n()) * 100,
  ) %>%
  pivot_longer(
    cols=1:12,
    names_pattern = "(.*)_(.*)$",
    names_to = c("name", "method"),
  ) %>%
  mutate(method=ifelse(method=="", "value", method)) %>%
  pivot_wider(
    id_cols = c(method),
    names_from = name,
    values_from = value,
    names_repair = "check_unique"
  )

cat("\nMethod means for paragraphs:\n")
print(as.data.frame(methods_par_values))
cat("\nMethod means for articles:\n")
print(as.data.frame(methods_art_values))




# Methods Plots -----------------------------------------------------------

methods_par_plot <- gcnews %>% 
  group_by(Source) %>%
  summarize(
    adict_norm  = mean( Dict_Gruendl/Sentences/max(gcnews$Dict_Gruendl / gcnews$Sentences) > 0.19 )  * 100,
    bembed_norm = mean(W2V_Labels > .5) * 100,
    cgzip_norm = mean(Gzip_Labels) * 100,
    dsetfit_norm = mean(SetFit_Labels > .5) * 100,
    epopB_norm = mean( PopBERT_AE + PopBERT_PC > 1 ) * 100,

    adict_cf = qt(0.975,df=n()-1) * sd(Dict_Gruendl/Sentences/max(gcnews$Dict_Gruendl / gcnews$Sentences) )/sqrt(n()) * 100,
    bembed_cf = qt(0.975,df=n()-1) * sd(W2V_Labels > .5)/sqrt(n()) * 100,
    cgzip_cf = qt(0.975,df=n()-1) * sd(Gzip_Labels)/sqrt(n()) * 100,
    dsetfit_cf = qt(0.975,df=n()-1) * sd(SetFit_Labels > .5)/sqrt(n()) * 100,
    epopB_cf = qt(0.975,df=n()-1) * sd(PopBERT_AE + PopBERT_PC > 1)/sqrt(n()) * 100,
  ) %>%
  pivot_longer(
    cols = -1, 
    names_pattern = "(.*)_(.*)$",
    names_to = c("method", "name")
  ) %>%
  mutate(method=ifelse(method=="", "value", method)) %>%
  pivot_wider(
    id_cols = c(Source, method),
    names_from = name,
    values_from = value,
    names_repair = "check_unique"
  ) %>%
  ggplot(aes(x=Populist, y=Source)) +
  geom_crossbar(fatten=2, aes(x=norm, y=Source, xmin=(norm-cf), xmax=(norm+cf), color=method)) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(0,86) + # Zoom
  labs(x="Paragraph-level Mean Populism Score (%, 95%-CI)", y=element_blank(), 
       col="Classification Method:") +
  scale_colour_discrete(labels = c("Gründl's Dict", 
                                   "W2V",
                                   "GZIP", 
                                   "SetFit",
                                   "PopBERT"
  )) +
  guides(colour = guide_legend(nrow = 1)) 

methods_art_plot <- gcnews %>% 
  group_by(Title, Date, Source, Author) %>%
  summarise(
    art_dict = sum(Dict_Gruendl/Sentences/max(gcnews$Dict_Gruendl / gcnews$Sentences) > .19),
    art_w2v  = sum(W2V_Labels > .5),
    art_gzip = sum(Gzip_Labels),
    art_sfit = sum(SetFit_Labels > .5),
    art_popB = sum(PopBERT_AE + PopBERT_PC > 1),
    art_n_par = n(),
    art_n_sen = sum(Sentences),
  ) %>% ungroup %>%
  group_by(Source) %>%
  summarise(
    mean_aDict = mean(art_dict / art_n_par) * 100,
    mean_bW2V  = mean(art_w2v / art_n_par) * 100,
    mean_cGzip = mean(art_gzip / art_n_par) * 100,
    mean_dSetFit = mean(art_sfit / art_n_par) * 100,
    mean_ePopBERT = mean(art_popB / art_n_par) * 100,
    
    ci_aDict = qt(0.975,df=n()-1) * sd(art_dict / art_n_par)/sqrt(n()) * 100,
    ci_bW2V  = qt(0.975,df=n()-1) * sd(art_w2v / art_n_par)/sqrt(n()) * 100,
    ci_cGzip = qt(0.975,df=n()-1) * sd(art_gzip / art_n_par)/sqrt(n()) * 100,
    ci_dSetFit = qt(0.975,df=n()-1) * sd(art_sfit / art_n_par)/sqrt(n()) * 100,
    ci_ePopBERT = qt(0.975,df=n()-1) * sd(art_popB / art_n_par)/sqrt(n()) * 100,
  ) %>% 
  pivot_longer(
    cols = -1, 
    names_pattern = "(.*)_(.*)$",
    names_to = c("name", "method")
  ) %>%
  mutate(method=ifelse(method=="", "value", method)) %>%
  pivot_wider(
    id_cols = c(Source, method),
    names_from = name,
    values_from = value,
    names_repair = "check_unique"
  ) %>%
  ggplot(aes(x=mean, y=Source)) +
  geom_crossbar(fatten=2, aes(x=mean, y=Source, xmin=(mean-ci), xmax=(mean+ci), color=method)) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(0,86) + # Zoom
  labs(x="Article-level Mean Populism Score (%, 95%-CI)", y=element_blank(),
       col="Classification Method:") +
  scale_colour_discrete(labels = c("Gründl's Dict",
                                   "W2V",
                                   "GZIP",
                                   "SetFit",
                                   "PopBERT"
                                   )) + 
  guides(colour = guide_legend(nrow = 1))


# Multiplicative Plot -----------------------------------------------------

multiplicative_par_plot <- gcnews %>% 
  group_by(Source) %>%
  summarize(
    adict_norm  = mean( Dict_Gruendl/Sentences/max(gcnews$Dict_Gruendl / gcnews$Sentences) > 0.19 )  * 100,
    bembed_norm = mean(W2V_Labels > .5) * 100,
    cgzip_norm = mean(Gzip_Labels) * 100,
    dsetfit_norm = mean(SetFit_Labels > .5) * 100,
    epopB_norm = mean( PopBERT_AE * PopBERT_PC > .19 ) * 100,
    
    adict_cf = qt(0.975,df=n()-1) * sd(Dict_Gruendl/Sentences/max(gcnews$Dict_Gruendl / gcnews$Sentences) )/sqrt(n()) * 100,
    bembed_cf = qt(0.975,df=n()-1) * sd(W2V_Labels > .5)/sqrt(n()) * 100,
    cgzip_cf = qt(0.975,df=n()-1) * sd(Gzip_Labels)/sqrt(n()) * 100,
    dsetfit_cf = qt(0.975,df=n()-1) * sd(SetFit_Labels > .5)/sqrt(n()) * 100,
    epopB_cf = qt(0.975,df=n()-1) * sd(PopBERT_AE * PopBERT_PC > .19)/sqrt(n()) * 100,
  ) %>%
  pivot_longer(
    cols = -1, 
    names_pattern = "(.*)_(.*)$",
    names_to = c("method", "name")
  ) %>%
  mutate(method=ifelse(method=="", "value", method)) %>%
  pivot_wider(
    id_cols = c(Source, method),
    names_from = name,
    values_from = value,
    names_repair = "check_unique"
  ) %>%
  ggplot(aes(x=Populist, y=Source)) +
  geom_crossbar(fatten=2, aes(x=norm, y=Source, xmin=(norm-cf), xmax=(norm+cf), color=method)) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(0,86) + # Zoom
  labs(x="Paragraph-level Mean Populism Score (%, 95%-CI)", y=element_blank(), 
       col="Classification Method:") +
  scale_colour_discrete(labels = c("Gründl's Dict", 
                                   "W2V",
                                   "GZIP", 
                                   "SetFit",
                                   "PopBERT"
  )) +
  guides(colour = guide_legend(nrow = 1)) 

multiplicative_art_plot <- gcnews %>% 
  group_by(Title, Date, Source, Author) %>%
  summarise(
    art_dict = sum(Dict_Gruendl/Sentences/max(gcnews$Dict_Gruendl / gcnews$Sentences) > .19),
    art_w2v  = sum(W2V_Labels > .5),
    art_gzip = sum(Gzip_Labels),
    art_sfit = sum(SetFit_Labels > .5),
    art_popB = mean( PopBERT_AE * PopBERT_PC > .19 ),
    art_n_par = n(),
    art_n_sen = sum(Sentences),
  ) %>% ungroup %>%
  group_by(Source) %>%
  summarise(
    mean_aDict = mean(art_dict / art_n_par) * 100,
    mean_bW2V  = mean(art_w2v / art_n_par) * 100,
    mean_cGzip = mean(art_gzip / art_n_par) * 100,
    mean_dSetFit = mean(art_sfit / art_n_par) * 100,
    mean_ePopBERT = mean(art_popB / art_n_par) * 100,
    
    ci_aDict = qt(0.975,df=n()-1) * sd(art_dict / art_n_par)/sqrt(n()) * 100,
    ci_bW2V  = qt(0.975,df=n()-1) * sd(art_w2v / art_n_par)/sqrt(n()) * 100,
    ci_cGzip = qt(0.975,df=n()-1) * sd(art_gzip / art_n_par)/sqrt(n()) * 100,
    ci_dSetFit = qt(0.975,df=n()-1) * sd(art_sfit / art_n_par)/sqrt(n()) * 100,
    ci_ePopBERT = qt(0.975,df=n()-1) * sd(art_popB / art_n_par)/sqrt(n()) * 100,
  ) %>% 
  pivot_longer(
    cols = -1, 
    names_pattern = "(.*)_(.*)$",
    names_to = c("name", "method")
  ) %>%
  mutate(method=ifelse(method=="", "value", method)) %>%
  pivot_wider(
    id_cols = c(Source, method),
    names_from = name,
    values_from = value,
    names_repair = "check_unique"
  ) %>%
  ggplot(aes(x=mean, y=Source)) +
  geom_crossbar(fatten=2, aes(x=mean, y=Source, xmin=(mean-ci), xmax=(mean+ci), color=method)) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(0,86) + # Zoom
  labs(x="Article-level Mean Populism Score (%, 95%-CI)", y=element_blank(),
       col="Classification Method:") +
  scale_colour_discrete(labels = c("Gründl's Dict",
                                   "W2V",
                                   "GZIP",
                                   "SetFit",
                                   "PopBERT"
  )) + 
  guides(colour = guide_legend(nrow = 1))


# Combining Plots ---------------------------------------------------------

legend_plot <- gcnews %>% 
  group_by(Source) %>%
  summarize(
    adict_norm  = mean( Dict_Gruendl/Sentences/max(gcnews$Dict_Gruendl / gcnews$Sentences) )  * 100,
    bembed_norm = mean(W2V_Labels > .5) * 100,
    cgzip_norm = mean(Gzip_Labels) * 100,
    dsetfit_norm = mean(SetFit_Labels > .5) * 100,
    epopB_norm = mean( PopBERT_AE + PopBERT_PC > 1 ) * 100,
    
    adict_cf = qt(0.975,df=n()-1) * sd(Dict_Gruendl/Sentences/max(gcnews$Dict_Gruendl / gcnews$Sentences) )/sqrt(n()) * 100,
    bembed_cf = qt(0.975,df=n()-1) * sd(W2V_Labels > .5)/sqrt(n()) * 100,
    cgzip_cf = qt(0.975,df=n()-1) * sd(Gzip_Labels)/sqrt(n()) * 100,
    dsetfit_cf = qt(0.975,df=n()-1) * sd(SetFit_Labels > .5)/sqrt(n()) * 100,
    epopB_cf = qt(0.975,df=n()-1) * sd(PopBERT_AE + PopBERT_PC > 1)/sqrt(n()) * 100,
  ) %>%
  pivot_longer(
    cols = -1, 
    names_pattern = "(.*)_(.*)$",
    names_to = c("method", "name")
  ) %>%
  mutate(method=ifelse(method=="", "value", method)) %>%
  pivot_wider(
    id_cols = c(Source, method),
    names_from = name,
    values_from = value,
    names_repair = "check_unique"
  ) %>%
  ggplot(aes(norm, Source, color = method)) + 
  geom_point() +
  labs(x=element_blank(), y=element_blank(),
       col="Classification Method:") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_colour_discrete(labels = c("Gründl's Dict",
                                   "W2V",
                                   "GZIP",
                                   "SetFit",
                                   "PopBERT"
  )) + 
  guides(colour = guide_legend(nrow = 1))

legend <- get_legend(legend_plot)

plot_grid(legend, methods_par_plot, methods_art_plot, ncol=1, axis=1, rel_heights = c(.1, .45,.45), align="v")
pdf(file = "./figures/both_methods_populism_sources.pdf",   
    width = 9, 
    height = 6)
plot_grid(legend, methods_par_plot, methods_art_plot, ncol=1, axis=1, rel_heights = c(.1, .45,.45), align="v")
dev.off()


plot_grid(
  legend, 
  multiplicative_par_plot,
  multiplicative_art_plot, 
  ncol=1, axis=1, rel_heights = c(.1, .45,.45), align="v")


# Zoom Plots --------------------------------------------------------------

plot_grid(
  legend, 
  methods_par_plot + 
    xlim(0,25) + labs(title = "(a)"), 
  methods_art_plot + 
    xlim(0,25) + labs(title = "(b)"), 
  multiplicative_par_plot + 
    xlim(0,25) + labs(title = "(c)"), 
  multiplicative_art_plot + 
    xlim(0,25) + labs(title = "(d)"), 
  ncol=1, axis=1, rel_heights = c(.1, .45, .45, .45, .45), align="v")
pdf(file = "./figures/all_methods_populism_sources_zoom.pdf",   
    width = 6.3, 
    height = 8.6)
plot_grid(
  legend, 
  methods_par_plot + 
    xlim(0,25) + labs(title = "(a)"), 
  methods_art_plot + 
    xlim(0,25) + labs(title = "(b)"), 
  multiplicative_par_plot + 
    xlim(0,25) + labs(title = "(c)"), 
  multiplicative_art_plot + 
    xlim(0,25) + labs(title = "(d)"), 
  ncol=1, axis=1, rel_heights = c(.1, .45, .45, .45, .45), align="v")
dev.off()
