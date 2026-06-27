# Intentions de votes pour la présidentielle française 2027
# Flavien Ganter

# Created on June 27, 2026
# Last modified on June 27, 2026




# PRELIMINARIES -----------------------------------------------------------------------------------------

# Clear working space
rm(list = ls())

# Set working directory
options(mc.cores = parallel::detectCores())
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

# Packages
library(tidyverse)
library(tidylog)
library(cmdstanr)
library(readxl)
library(HDInterval)
library(splines)
library(zoo)
subset(systemfonts::system_fonts(), grepl("Open Sans Condensed", family))

## Commands & Functions ----

  # Commands
  "%nin%" <- Negate("%in%")
  logit <- function(x) log(x/(1-x))
  
  # Rounding
  round2 <- function(x) {
    diff <- c(abs(x-trunc(x)), abs(x-trunc(x)-.5), abs(x-trunc(x)-1))
    if (length(which(min(diff) == diff)) == 1) {
      c(trunc(x), trunc(x)+.5, trunc(x)+1)[which(min(diff) == diff)]
    } else {
      c(trunc(x), trunc(x)+.5, trunc(x)+1)[which(min(diff) == diff)[2]]
    }
  }
  
  # Function to center and standardize continuous and categorical variables
  # (helpful for Bayesian prior, see Gelman et al. 2008)
  scale_cont <- function(variable) (variable - mean(variable)) / (2 * weighted.sd(variable))
  scale_factor <- function(variable) {
    scale_dummy <- function(dummy) ifelse(dummy == 1, 1 - mean(dummy == 1), - mean(dummy == 1))
    L <- length(unique(variable))
  
    if (L == 2) {
      out_variable <- scale_dummy(variable)
    } else {
      out_variable <- vector(mode = "list", length = L-1)
      for (l in 1:(L-1)) {
        dummy <- ifelse(variable == unique(variable)[l], 1, 0)
        out_variable[[l]] <- scale_dummy(dummy)
      }
    }
    return(out_variable)
  }
  
  # Function to extract draws
  get_draws <- function(candidate) {
    spline_draws <- readRDS(paste0("model_output/model_aggregator_", candidate, ".rds"))
    spline_draws <- as.data.frame(spline_draws) %>% select(-.chain, -.iteration, -.draw)
    colnames(spline_draws) <- paste0("prob[", 1:ncol(spline_draws), ",", candidate, "]")
    return(spline_draws)
  }
  

  
# SHAPE DATA --------------------------------------------------------------------------------------------

data <- read_excel("polls_data_2027.xlsx") %>% 
  
  # Create hypothesis ID
  mutate(id_hyp = 1:n()) %>% 
    
  # Remove irrelavant scenarios
  filter(n_wgt == 1) %>%
    
  # Remove Hollande, de Villepin, Le Pen, Attal
  filter(
    is.na(c_hlld),
    is.na(c_attl),
    is.na(c_mlp)
  ) %>%
  select(-c_hlld, -c_villpn, -c_mlp, -c_attl) %>% 
  
  # Wide to long
  gather(candidate, share, c_arthd:c_zemmr) %>% 
  
  # Remove rows corresponding to untested candidates
  filter(!is.na(share)) %>% 
    
  # Rounding indicator
  mutate(
    rounding_ind = case_when(
      share == "T_0.5" ~ 6,
      share == "T_1.5" ~ 5,
      share == "T_1" ~ 4,
      rounding == 1 ~ 3,
      rounding == 0.5 ~ 2,
      rounding == 0.1 ~ 1,
      rounding == 0.01 ~ 0
    )
  ) %>% 
  
  # Recode truncated values and switch to share
  mutate(
    vshare_raw = case_when(
      rounding_ind %in% c(4:6) ~ 0,
      TRUE ~ as.numeric(share) / 100
    )
  ) %>%
    
  # Renumber polls (to account for polls that have been removed)
  group_by(id) %>% 
  mutate(id_poll = cur_group_id()) %>% 
  ungroup() %>% 
    
  # Rename and weight total n variable
  mutate(tot_eff = round(n * n_wgt)) %>% 
  
  # Create covariates
  mutate(
    gunie_sc = scale_factor(variable = g_unie),
    #rolling = (poll_type == "rolling") * 1L,
    #rolling_sc = scale_factor(variable = rolling),
    unsure_1 = scale_factor(variable = unsure)[[1]],
    unsure_2 = scale_factor(variable = unsure)[[2]]
  ) %>% 
  
  # Create a candidate ID
  mutate(
    id_candidate = case_when(
      candidate == "c_arthd" ~ 1,
      candidate == "c_melch" ~ 2,
      candidate == "c_roussl" ~ 3,
      candidate == "c_tondl" ~ 4,
      candidate == "c_glcks" ~ 5,
      candidate == "c_philp" ~ 6,
      candidate == "c_rtll" ~ 7,
      candidate == "c_nda" ~ 8,
      candidate == "c_bardella" ~ 9,
      candidate == "c_zemmr" ~ 10
    )
  ) %>% 
  
  # Create a house ID
  group_by(house) %>% 
  mutate(id_house = cur_group_id()) %>% 
  ungroup() %>% 
    
  # Create date IDs
  mutate(
    id_date_start = as.numeric(as.Date(paste(year, month_start, day_start, sep = "-"))) - 20510 + 1,
    id_date_end = as.numeric(as.Date(paste(year, month_end, day_end, sep = "-"))) - 20510 + 1,
    id_date = round(id_date_start + (id_date_end - id_date_start) / 2)
  )
  
  
# Export
saveRDS(data, file = "polls_data_2027.rds")



# MODEL -------------------------------------------------------------------------------------------------

for (i in 1:10) {
  
# Keep data for one candidate
data_i <- data %>% 
  filter(id_candidate == i) %>% 
  group_by(id_poll) %>% mutate(id_poll = cur_group_id()) %>% ungroup() %>% 
  group_by(id_house) %>% mutate(id_house = cur_group_id()) %>% ungroup() %>% 
  group_by(rounding_ind) %>%
  mutate(rr = 1:n(),
         r_0 = ifelse(rounding_ind == 0, rr, 0),
         r_1 = ifelse(rounding_ind == 1, rr, 0),
         r_2 = ifelse(rounding_ind == 2, rr, 0),
         r_3 = ifelse(rounding_ind == 3, rr, 0),
         r_4 = ifelse(rounding_ind == 4, rr, 0),
         r_5 = ifelse(rounding_ind == 5, rr, 0),
         r_6 = ifelse(rounding_ind == 6, rr, 0))

# Define splines 
num_knots     <- 3
spline_degree <- 3
num_basis     <- num_knots + spline_degree - 1
B             <- t(bs(1:max(data_i$id_date_end), df = num_basis, degree = spline_degree, intercept = TRUE))

# Gather data to feed the model
data_spline_model <- list(
  N               = nrow(data_i),
  id_cand         = i,
  tot_eff         = data_i$tot_eff,
  vshare_raw      = data_i$vshare_raw,
  rounding_ind    = data_i$rounding_ind,
  r_0             = data_i$r_0,
  N_0             = max(data_i$r_0),
  r_1             = data_i$r_1,
  N_1             = max(data_i$r_1),
  r_2             = data_i$r_2,
  N_2             = max(data_i$r_2),
  r_3             = data_i$r_3,
  N_3             = max(data_i$r_3),
  r_4             = data_i$r_4,
  N_4             = max(data_i$r_4),
  r_5             = data_i$r_5,
  N_5             = max(data_i$r_5),
  r_6             = data_i$r_6,
  N_6             = max(data_i$r_6),
  id_date         = data_i$id_date,
  id_date_end     = data_i$id_date_end,
  id_poll         = data_i$id_poll,
  P               = length(unique(data_i$id_poll)),
  id_house        = data_i$id_house,
  F               = length(unique(data_i$id_house)),
  X               = data_i[, c("unsure_1", "unsure_2", "gunie_sc")],
  g_unie_b         = max(data_i$gunie_sc),
  #rolling_b       = min(data_i$rolling_sc),
  num_knots       = num_knots,
  knots           = unname(quantile(1:max(data_i$id_date_end), probs = seq(from = 0, to = 1, length.out = num_knots))),
  spline_degree   = spline_degree,
  num_basis       = num_basis,
  D               = ncol(B),
  S               = as.matrix(B)
)
  
# Compile model
model_code <- cmdstan_model("model_bycand.stan")

# Estimate model
estimated_spline_model <- model_code$sample(
  data            = data_spline_model,
  seed            = 94836,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 2000,
  iter_sampling   = 2000,
  adapt_delta     = .99,
  max_treedepth   = 15,
  refresh         = 1000,
  save_warmup     = FALSE
)

# Get posterior draws
spline_draws <- estimated_spline_model$draws(variables = "prob", format = "draws_df")
saveRDS(spline_draws, file = paste0("model_output/model_aggregator_", i, ".rds"))

}



# GENERATE GRAPHS ---------------------------------------------------------------------------------------


## Get estimates ----

spline_draws <- get_draws(candidate = 1) %>% 
  add_column(get_draws(candidate = 2)) %>% 
  add_column(get_draws(candidate = 3)) %>% 
  add_column(get_draws(candidate = 4)) %>% 
  add_column(get_draws(candidate = 5)) %>% 
  add_column(get_draws(candidate = 6)) %>% 
  add_column(get_draws(candidate = 7)) %>% 
  add_column(get_draws(candidate = 8)) %>% 
  add_column(get_draws(candidate = 9)) %>% 
  add_column(get_draws(candidate = 10)) 



## Evolution plot ----

### Prepare table for plot

# Calculate statistics of interest
plot_spline_estimates <- apply(
  spline_draws, 2, 
  function(x) c(
    hdi(x), 
    hdi(x, .9), 
    hdi(x, .8), 
    hdi(x, .5), 
    median(x)
  )
) %>%
  t() %>% 
  as.data.frame()

# Rename names and columns
names(plot_spline_estimates) <- c("lower95", 
                                  "upper95", 
                                  "lower90", 
                                  "upper90", 
                                  "lower80", 
                                  "upper80", 
                                  "lower50", 
                                  "upper50", 
                                  "median")
plot_spline_estimates$coef <- row.names(plot_spline_estimates)

# Name estimates: identify the date and candidate associated with each estimate
plot_spline_estimates <- plot_spline_estimates %>%
  mutate(
    date = substr(coef, 6, 8),
    date = case_when(
      substr(date, 3, 3) == "," ~ substr(date, 1, 2),
      substr(date, 2, 2) == "," ~ substr(date, 1, 1),
      TRUE ~ substr(date, 1, 3)
    ),
    date = as.Date(as.numeric(date)-2, origin = as.Date("2026-02-26")),
    candidate = substr(coef, 7, 11),
    candidate = case_when(
      substr(candidate, 1, 1) == "," ~ substr(candidate, 2, 3),
      substr(candidate, 2, 2) == "," ~ substr(candidate, 3, 4),
      TRUE ~ substr(candidate, 4, 5)
    ),
    candidate = ifelse(substr(candidate, 2, 2) == "]", substr(candidate, 1, 1), candidate),
    label_candidate = as.factor(case_when(
      candidate == 1 ~ "Arthaud",
      candidate == 2 ~ "Mélenchon",
      candidate == 3 ~ "Roussel",
      candidate == 4 ~ "Tondelier",
      candidate == 5 ~ "Glucksmann",
      candidate == 6 ~ "Philippe",
      candidate == 7 ~ "Retailleau",
      candidate == 8 ~ "Dupont-Aignan",
      candidate == 9 ~ "Bardella",
      candidate == 10 ~ "Zemmour"
      )),
    candidate = as.factor(case_when(
      candidate == 1 ~ "Nathalie Arthaud",
      candidate == 2 ~ "Jean-Luc Mélenchon",
      candidate == 3 ~ "Fabien Roussel",
      candidate == 4 ~ "Marine Tondelier",
      candidate == 5 ~ "Raphaël Glucksmann",
      candidate == 6 ~ "Édouard Philippe",
      candidate == 7 ~ "Bruno Retailleau",
      candidate == 8 ~ "Nicolas Dupont-Aignan",
      candidate == 9 ~ "Jordan Bardella",
      candidate == 10 ~ "Éric Zemmour"
      ))
    )


### Create plot

# Define candidate colors
candidate_colors <- c(
  "#ff1300",
  "#b30d00",
  "#ff0099", 
  "#00b050",
  "#ff6600",
  "#4e91f7",
  "#0070c0",
  "#0000008b",
  "#002060",
  "#000000"
)

# Generate plot: day to day
poll_plot <- plot_spline_estimates %>% 
  mutate(
    label_candidate = candidate,
    label = if_else(
      date == max(date), 
      paste0(
        as.character(label_candidate), 
        " (", unlist(lapply(median*1000, round))/10, "%)"
      ),
      NA_character_
    ),
    median_label = case_when(
      #candidate == "Liste LFI" ~ median + .002,
      #candidate == "Liste LR" ~ median - .005,
      #candidate == "Liste DLF" ~ median - .003,
      !is.na(candidate) ~ median)
    ) %>% 
  group_by(candidate) %>% 
  mutate(
    lower50_l = zoo::rollmean(lower50, k = 2, align = "left", fill = NA),
    lower50_r = zoo::rollmean(lower50, k = 2, align = "right", fill = NA),
    upper50_l = zoo::rollmean(upper50, k = 2, align = "left", fill = NA),
    upper50_r = zoo::rollmean(upper50, k = 2, align = "right", fill = NA),
    lower95_l = zoo::rollmean(lower95, k = 2, align = "left", fill = NA),
    lower95_r = zoo::rollmean(lower95, k = 2, align = "right", fill = NA),
    upper95_l = zoo::rollmean(upper95, k = 2, align = "left", fill = NA),
    upper95_r = zoo::rollmean(upper95, k = 2, align = "right", fill = NA),
    lower50_s = zoo::rollmean(lower50, k = 3, fill = NA),
    upper50_s = zoo::rollmean(upper50, k = 3, fill = NA),
    lower95_s = zoo::rollmean(lower95, k = 3, fill = NA),
    upper95_s = zoo::rollmean(upper95, k = 3, fill = NA),
    lower50_s = ifelse(is.na(lower50_s), lower50_l, lower50_s),
    lower50_s = ifelse(is.na(lower50_s), lower50_r, lower50_s),
    upper50_s = ifelse(is.na(upper50_s), upper50_l, upper50_s),
    upper50_s = ifelse(is.na(upper50_s), upper50_r, upper50_s),
    lower95_s = ifelse(is.na(lower95_s), lower95_l, lower95_s),
    lower95_s = ifelse(is.na(lower95_s), lower95_r, lower95_s),
    upper95_s = ifelse(is.na(upper95_s), upper95_l, upper95_s),
    upper95_s = ifelse(is.na(upper95_s), upper95_r, upper95_s)
  ) %>% 
  filter(date > as.Date("2025-03-01")) %>% 
  ggplot(aes(
    x = date, 
    group = candidate, 
    color = candidate
  )) +
  
  # Plot data
  geom_line(aes(y = median * 100)) +
  geom_ribbon(aes(ymin = lower50_s * 100, 
                  ymax = upper50_s * 100, 
                  fill = candidate), 
              alpha = .1, 
              color = NA) +
  geom_ribbon(aes(ymin = lower95_s * 100, 
                  ymax = upper95_s * 100, 
                  fill = candidate), 
              alpha = .1, 
              color = NA) +
  
  # Candidate labels
  geom_text(aes(x = date + 1, 
                y = median_label * 100, 
                label = label), 
            na.rm = TRUE,
            hjust = 0, 
            vjust = 0, 
            nudge_y = -.1, 
            family = "Open Sans Condensed", 
            size = 3) +
  
  # Show latest poll's date
  annotate("segment", x = max(plot_spline_estimates$date), y = 0, xend = max(plot_spline_estimates$date), yend = 45,
           size = .4) +
  annotate(geom = "text", x = max(plot_spline_estimates$date), y = 45.5, family = "Open Sans Condensed",
           label = format(max(plot_spline_estimates$date), "%d %B %Y"), size = 3) +
  
  # Show election date
  annotate("segment",
           x = as.Date("2027-04-18"), 
           y = 0, 
           xend = as.Date("2027-04-18"), 
           yend = 43.5,
           size = .4) +
  annotate(geom = "text", 
           x = as.Date("2027-04-18"), 
           y = 44.75, 
           label = "Premier tour", 
           family = "Open Sans Condensed",
           size = 3) +
  
  # Define labs
  labs(x = "", 
       y = "Intentions de votes (% votes exprimés)",
       title = "Intentions de vote à l'élection européenne française de 2027",
       subtitle = "Depuis février 2026",
       caption = paste0("Estimations obtenues à partir des enquêtes d'opinion réalisées par BVA, Cluster17, Elabe, Harris Interactive, IFOP, IPSOS, Odoxa, OpinionWay et ViaVoice depuis septembre 2023 sur la base des rapports d'enquête publiés sur le site de la Commission des sondages, et agrégées à l'aide d'un modèle bayésien tenant compte \ndes principales caractéristiques des enquêtes. Le graphique présente les médianes et intervalles de crédibilité (95% / 50%). Pour chaque candidat, la ligne solide relie les médianes des distributions a posteriori à chaque date, et la zone colorée représente la partie la plus dense de la distribution a posteriori (95% / 50%) des \ndistributions a posteriori. Dernière mise à jour : ", format(Sys.time(), "%d %B %Y"), ".")) +
       #caption = paste0("Estimations obtenues à partir des enquêtes d'opinion réalisées par BVA, Cluster17, Elabe, Harris Interactive, IFOP, IPSOS, Kantar, Odoxa, et OpinionWay depuis septembre 2021 sur la base des rapports d'enquête publiés sur le site de la Commission des sondages, et agrégées à l'aide d'un modèle bayésien tenant compte \ndes principales caractéristiques des enquêtes. Le graphique présente les médianes et intervalles de crédibilité (95% / 50%)Pour chaque candidat, la ligne solide relie les médianes des distributions a posteriori à chaque date, et la zone colorée représente la partie la plus dense de la distribution a posteriori (95% / 50%) des \ndistributions a posteriori. Dernière mise à jour: ", format(Sys.time(), "%d %B %Y"), ".")) +
  
  # Specify plot theme
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#2b2b2b", 
                                          linetype = "dotted", 
                                          size = 0.05),
        text = element_text(family = "Open Sans Condensed", 
                            size = 7.5),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(margin = margin(r = -2)),
        axis.title = element_blank(),
        axis.line.x = element_line(color = "#2b2b2b", 
                                   size = 0.15),
        axis.ticks.x = element_line(color = "#2b2b2b", 
                                    size = 0.15),
        axis.ticks.length = unit(.2, "cm"),
        plot.title = element_text(size = 20, 
                                  family = "Open Sans Condensed", 
                                  face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 12),
        legend.position = "none",
        plot.caption = element_text(color = "gray30", 
                                    hjust = 0, 
                                    margin = margin(t = 15)),
        plot.margin = unit(rep(0.5, 4), "cm")) +
  
  # Candidate colors
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  scale_colour_manual(values = candidate_colors) +
  scale_fill_manual(values = candidate_colors) +
  
  # Date axis
  scale_x_date(
    expand = c(.005,1), 
    date_breaks = "1 month",
    date_labels = "%B",
    limits = c(as.Date("2026-03-01"), as.Date("2027-06-25"))
  ) +
  
  # Percent axis
  scale_y_continuous(
    labels = function(x) paste0(x, "%"), 
    expand = c(0, 0),
    breaks = seq(0, 40, 5),
    lim = c(0, 46.5)
  )



## Export plot
ggsave(poll_plot, 
       filename = "PollsFrance2024_evolution.pdf",
       height = 6, 
       width = 10, 
       device = cairo_pdf)
ggsave(poll_plot, 
       filename = "PollsFrance2024_evolution.png",
       height = 6, 
       width = 10, 
       device = "png", 
       bg = "white")


## Current estimate plot ----

## Prepare table

plot_inst_estimates <- plot_spline_estimates %>% 
  filter(date == max(date))  %>%
  mutate(label = paste0(unlist(lapply(median*1000, round))/10, "%"),
         label = ifelse(label == "0.5%", "   0.5%", label))
plot_inst_estimates$candidate <- factor(plot_inst_estimates$candidate,
                                        levels = as.vector(plot_inst_estimates$candidate
                                                           [rev(order(plot_inst_estimates$median))]))

## Create plot

# Define candidate colors
candidate_colors <- c("#00b050",
                      "#ff1300",
                      "#0070c0",
                      "#ff6600",
                      "#b30d00",
                      "#f7b4b4",
                      "#002060",
                      "black")[match(as.vector(plot_inst_estimates$candidate
                                                 [rev(order(plot_inst_estimates$median))]),
                                       c("Liste EELV", 
                                         "Liste LFI",  
                                         "Liste LR",
                                         "Liste LREM", 
                                         "Liste PCF", 
                                         "Liste PS-PP", 
                                         "Liste R!", 
                                         "Liste RN"))]


# Generate plot: last estimate
inst_plot <- plot_inst_estimates %>% 
  ggplot(aes(x = candidate, color = candidate)) +
  
  # Plot data
  geom_linerange(aes(ymin = lower50 * 100, 
                     ymax = upper50 * 100), 
                 alpha = .2, 
                 size = 10) +
  geom_linerange(aes(ymin = lower80 * 100, 
                     ymax = upper80 * 100), 
                 alpha = .2, 
                 size = 10) +
  geom_linerange(aes(ymin = lower90 * 100, 
                     ymax = upper90 * 100), 
                 alpha = .2, 
                 size = 10) +
  geom_linerange(aes(ymin = lower95 * 100, 
                     ymax = upper95 * 100), 
                 alpha = .2, 
                 size = 10) +
  geom_linerange(aes(ymin = median * 100 - .05, 
                     ymax = median * 100 + .05), 
                 size = 12, 
                 color = "white") +
  geom_linerange(aes(ymin = median * 100 - .02, 
                     ymax = median * 100 + .02), 
                 size = 11) +
  geom_text(aes(label = label, 
                y = median * 100), 
            family = "Open Sans Condensed SemiBold", 
            vjust = -1.65) +
  
  # Define labs
  labs(x = "", 
       y = "Intentions de votes (% votes exprimés)",
       title = "Intentions de vote à l'élection européenne française de 2024",
       subtitle = paste("Au", format(Sys.time(), "%d %B %Y")),
       caption = paste0("Estimations obtenues à partir des enquêtes d'opinion réalisées par BVA, Cluster17, Elabe, Harris Interactive, IFOP, IPSOS, Odoxa, OpinionWay et ViaVoice depuis juin 2023 sur la base des rapports d'enquête publiés sur le site de la Commission des sondages, et agrégées à \nl'aide d'un modèle bayésien tenant compte des principales caractéristiques des enquêtes. Le graphique présente les médianes et intervalles de crédibilité (95% / 90% / 80% / 50%) des distributions a posteriori. Dernière mise à jour : ", format(Sys.time(), "%d %B %Y"), ".")) +
  
  # Specify plot theme
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#2b2b2b", 
                                          linetype = "dotted", 
                                          size = 0.05),
        panel.grid.major.y = element_line(color = "gray99", 
                                          size = 10),
        text = element_text(family = "Open Sans Condensed", 
                            size = 7.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12, 
                                   family = "Open Sans Condensed", 
                                   face = "bold"),
        axis.title = element_blank(),
        axis.line.x = element_line(color = "#2b2b2b", 
                                   size = 0.15),
        axis.ticks.x = element_line(color = "#2b2b2b", 
                                    size = 0.15),
        axis.ticks.length = unit(.2, "cm"),
        plot.title = element_text(size = 20, 
                                  family = "Open Sans Condensed ExtraBold", 
                                  face = "bold"),
        plot.subtitle = element_text(size = 15),
        plot.title.position = "plot",
        legend.position = "none",
        plot.caption = element_text(color = "gray30", 
                                    hjust = 0, 
                                    margin = margin(t = 15)),
        plot.margin = unit(rep(0.5, 4), "cm")) +
  
  # Candidate colors
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  scale_colour_manual(values = candidate_colors) +
  scale_fill_manual(values = candidate_colors) +
  
  # Percent axis
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     expand = c(0, 0), 
                     breaks = seq(0, 35, 5), 
                     lim = c(0, 36))


## Export plot
ggsave(inst_plot, 
       filename = "PollsFrance2024_latest.pdf",
       height = 7.5, 
       width = 10,
       device = cairo_pdf)
ggsave(inst_plot, 
       filename = "PollsFrance2024_latest.png",
       height = 7.5, 
       width = 10, 
       device = "png", 
       bg = "white")


