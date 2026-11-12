# Intentions de votes pour la présidentielle française 2027
# Flavien Ganter

# Created on June 27, 2026
# Last modified on June 27, 2026




# PRELIMINARIES ------------------------------------------------------------------------------------

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
  scale_cont <- function(variable) (variable - mean(variable)) / (2 * sd(variable))
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

## Candidates and scenario dummies ----

  # Modeled candidates (column -> candidate ID)
  candidate_ids <- c(
    c_arthd = 1, c_melch = 2, c_roussl = 3, c_tondl = 4, c_glcks = 5,
    c_philp = 6, c_rtll = 7, c_nda = 8, c_bardella = 9, c_mlp = 9,
    c_zemmr = 10, c_attl = 11, c_lisnard = 12
  )
  n_candidates <- 12

  # Scenario dummies (relative to the reference scenario: Le Pen,
  # Philippe without Attal, Glucksmann, Roussel, Tondelier, Retailleau,
  # Zemmour, no Ruffin / Lisnard / Villepin)
  scen_dummies <- c("d_attal_only", "d_attal_plus", "d_hollande", "d_faure", "d_ruffin",
                   "d_no_tondl", "d_no_roussl", "d_lisnard", "d_villepin", "d_mlp")

  # Value of each dummy in the reference scenario, by candidate
  # (Attal: Attal instead of Philippe; Lisnard: Lisnard running)
  ref_scenario <- function(cand) {
    ref <- setNames(rep(0, length(scen_dummies)), scen_dummies)
    ref["d_mlp"] <- 1
    if (cand == 11) ref["d_attal_only"] <- 1
    if (cand == 12) ref["d_lisnard"] <- 1
    ref
  }

data_wide <- read_excel("polls_data_2027.xlsx") %>%

  # Create scenario ID
  mutate(id_scen = 1:n()) %>%

  # Remove irrelevant scenarios
  filter(n_wgt == 1) %>%

  # Remove scenarios that cannot be modeled (elements observed too rarely)
  filter(
    is.na(c_drmnn), is.na(c_lcrn), is.na(c_lemaire),                    # Other bloc central candidates
    is.na(c_royal), is.na(c_guedj), is.na(c_maurel),                     # Other PS candidates
    (!is.na(c_glcks)) + (!is.na(c_hlld)) + (!is.na(c_faure)) == 1,      # Exactly one PS candidate
    !is.na(c_philp) | !is.na(c_attl),                                    # Philippe and/or Attal
    !is.na(c_rtll),                                                      # Retailleau running
    !is.na(c_zemmr), is.na(c_knafo),                                     # Zemmour (not Knafo)
    is.na(c_lagarde), is.na(c_riner), is.na(c_asselineau)                # Rarely tested candidates
  ) %>%

  # Scenario dummies
  # (Dupont-Aignan / Arthaud absent, Bouamrane, Bonnal, Polony, Bertrand: ignored)
  mutate(
    d_attal_only = 1 * (!is.na(c_attl) & is.na(c_philp)),
    d_attal_plus = 1 * (!is.na(c_attl) & !is.na(c_philp)),
    d_hollande   = 1 * !is.na(c_hlld),
    d_faure      = 1 * !is.na(c_faure),
    d_ruffin     = 1 * !is.na(c_ruffin),
    d_no_tondl   = 1 * is.na(c_tondl),
    d_no_roussl  = 1 * is.na(c_roussl),
    d_lisnard    = 1 * !is.na(c_lisnard),
    d_villepin   = 1 * !is.na(c_villpn),
    d_mlp        = 1 * !is.na(c_mlp)
  )

data <- data_wide %>%

  # Wide to long (modeled candidates only)
  select(-any_of(setdiff(grep("^c_", names(data_wide), value = TRUE), names(candidate_ids)))) %>%
  mutate(across(all_of(names(candidate_ids)), as.character)) %>%
  gather(candidate, share, all_of(names(candidate_ids))) %>%
  
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
  
  # Create population-definition covariates
  # (scenario dummies are centered candidate by candidate in the model loop)
  mutate(
    unsure_1 = scale_factor(variable = unsure)[[1]],
    unsure_2 = scale_factor(variable = unsure)[[2]]
  ) %>%

  # Create a candidate ID
  mutate(id_candidate = unname(candidate_ids[candidate])) %>%
  
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

for (i in 1:n_candidates) {

# Keep data for one candidate
data_i <- data %>%
  filter(id_candidate == i) %>%
  arrange(id_scen) %>%
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
         r_6 = ifelse(rounding_ind == 6, rr, 0)) %>%
  ungroup()

# Restrict the spline to the candidate's observation period: day 1 is the
# first fieldwork day of the first poll testing the candidate, and the last day
# is the end of the most recent poll (all candidates). This changes nothing for
# candidates tested since February 26 (date_offset = 0)
date_offset <- min(data_i$id_date_start) - 1
data_i <- data_i %>%
  mutate(
    id_date     = id_date - date_offset,
    id_date_end = id_date_end - date_offset
  )
D_i <- max(data$id_date_end) - date_offset

# Scenario dummies for this candidate: keep those that vary in the
# candidate's data (e.g. d_attal_only is always 0 for Philippe, always 1 or 0
# with d_attal_plus for Attal), center them, and center the reference scenario
# accordingly
x_ref_raw <- ref_scenario(i)
X_raw     <- as.matrix(data_i[, scen_dummies])
keep      <- apply(X_raw, 2, function(x) length(unique(x)) > 1)
if (i == 11) keep["d_attal_only"] <- FALSE # collinear with d_attal_plus in Attal's data
X_raw     <- X_raw[, keep, drop = FALSE]
X_means   <- colMeans(X_raw)
X_scen    <- sweep(X_raw, 2, X_means)
x_ref     <- x_ref_raw[colnames(X_raw)] - X_means
cat("Candidate", i, "- scenario dummies:", paste(colnames(X_scen), collapse = ", "), "\n")

# Define splines
num_knots     <- ifelse(D_i < .75 * max(data$id_date_end), 3, 4) # Smoother spline for short observation periods
spline_degree <- 3
num_basis     <- num_knots + spline_degree - 1
B             <- t(bs(1:D_i, df = num_basis, degree = spline_degree, intercept = TRUE))

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
  U               = as.matrix(data_i[, c("unsure_1", "unsure_2")]),
  K               = ncol(X_scen),
  X               = X_scen,
  x_ref           = as.vector(x_ref),
  num_knots       = num_knots,
  knots           = unname(quantile(1:D_i, probs = seq(from = 0, to = 1, length.out = num_knots))),
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

spline_draws <- bind_cols(lapply(1:n_candidates, function(c) get_draws(candidate = c)))



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
candidate_names <- c(
  "1" = "Nathalie Arthaud", "2" = "Jean-Luc Mélenchon", "3" = "Fabien Roussel",
  "4" = "Marine Tondelier", "5" = "Raphaël Glucksmann", "6" = "Édouard Philippe",
  "7" = "Bruno Retailleau", "8" = "Nicolas Dupont-Aignan", "9" = "Marine Le Pen",
  "10" = "Éric Zemmour", "11" = "Gabriel Attal", "12" = "David Lisnard"
)
candidate_labels <- c(
  "1" = "Arthaud", "2" = "Mélenchon", "3" = "Roussel", "4" = "Tondelier",
  "5" = "Glucksmann", "6" = "Philippe", "7" = "Retailleau", "8" = "Dupont-Aignan",
  "9" = "Le Pen", "10" = "Zemmour", "11" = "Attal", "12" = "Lisnard"
)

# First date at which each candidate was tested (day 1 of the candidate's spline)
first_date <- data %>% 
  group_by(id_candidate) %>% 
  summarise(first_date = as.Date(min(id_date_start) - 1, origin = as.Date("2026-02-26")))

plot_spline_estimates <- plot_spline_estimates %>%
  mutate(
    id_candidate = as.numeric(sub(".*,([0-9]+)\\]$", "\\1", coef)),
    day = as.numeric(sub("^prob\\[([0-9]+),.*", "\\1", coef)),
    label_candidate = as.factor(candidate_labels[as.character(id_candidate)]),
    candidate = as.factor(candidate_names[as.character(id_candidate)])
  ) %>% 
  left_join(first_date, by = "id_candidate") %>% 
  mutate(date = first_date + (day - 1)) # The spline starts at the candidate's first poll



## Graph settings ----

# Fonts: ITC Franklin Gothic, declared under the same family names for both devices:
#   - systemfonts/ragg (PNG files), with tabular figures
#   - quartz (PDF files): CoreText gives proper letter spacing, unlike cairo_pdf at small sizes
#   "franklin":    plain = Book, bold = Demi
#   "franklinmed": plain = Medium, bold = Heavy
font_book <- "franklin"
font_med  <- "franklinmed"
local({
  f <- systemfonts::system_fonts()
  path <- function(style) {
    x <- f[f$family == "ITC Franklin Gothic Std" & f$style == style, ][1, ]
    list(x$path, x$index)
  }
  tab <- systemfonts::font_feature(numbers = c("lining", "tabular"))
  systemfonts::register_font(font_book, plain = path("Book"), bold = path("Demi"), features = tab)
  systemfonts::register_font(font_med,  plain = path("Medium"), bold = path("Heavy"), features = tab)
  systemfonts::register_font("Franklin Demi (measure)", plain = path("Demi"))  # to measure bold labels
})
quartzFonts(
  franklin    = c("ITCFranklinGothicStd-Book", "ITCFranklinGothicStd-Demi",
                  "ITCFranklinGothicStd-BookIt", "ITCFranklinGothicStd-DemiIt"),
  franklinmed = c("ITCFranklinGothicStd-Med", "ITCFranklinGothicStd-Hvy",
                  "ITCFranklinGothicStd-MedIt", "ITCFranklinGothicStd-HvyIt")
)

# Dates
last_date     <- max(plot_spline_estimates$date)
election_date <- as.Date("2027-04-18")
update_date   <- trimws(format(Sys.Date(), "%e %B %Y"))
last_date_lab <- trimws(format(last_date, "%e %B %Y"))

# French number format
fr_pct <- function(x, digits = 1) paste0(formatC(x, format = "f", digits = digits, decimal.mark = ","), " %")

# Party colours
party_colors <- c(
  "Nathalie Arthaud"      = "#8E1F2F",
  "Jean-Luc Mélenchon"    = "#CF3A4B",
  "Fabien Roussel"        = "#C2185B",
  "Marine Tondelier"      = "#2E9A5A",
  "Raphaël Glucksmann"    = "#E07B39",
  "Édouard Philippe"      = "#3D85C6",
  "Gabriel Attal"         = "#E0A91F",
  "David Lisnard"         = "#7FA0C4",
  "Bruno Retailleau"      = "#1F4E8C",
  "Nicolas Dupont-Aignan" = "#5C5C5C",
  "Marine Le Pen"         = "#1B2A4A",
  "Éric Zemmour"          = "#7A5230"
)

# Mix a colour with white (p = share of the original colour)
tint <- function(col, p) {
  rgb_col <- grDevices::col2rgb(col) / 255
  out <- p * rgb_col + (1 - p) * 1
  grDevices::rgb(out[1, ], out[2, ], out[3, ])
}

# Darker version of a party colour, legible as text
text_color <- function(col) {
  rgb_col <- grDevices::col2rgb(col) / 255
  l <- 0.2126 * rgb_col[1, ] + 0.7152 * rgb_col[2, ] + 0.0722 * rgb_col[3, ]
  k <- ifelse(l > .45, .62, ifelse(l > .3, .85, 1))
  grDevices::rgb(rgb_col[1, ] * k, rgb_col[2, ] * k, rgb_col[3, ] * k)
}

# Ink colours
ink_1    <- "#111111"
ink_2    <- "#555555"
ink_3    <- "#8C8C8C"
grid_col <- "#E4E4E4"
base_col <- "#3A3A3A"

# Source note
source_text <- paste0(
  "Source : enquêtes Cluster17, Elabe, Harris Interactive, Ifop, Ipsos-BVA, Odoxa, OpinionWay, Verian et YouGov ",
  "réalisées depuis février 2026 (rapports d'enquête publiés sur le site de la Commission des sondages), agrégées ",
  "à l'aide d'un modèle bayésien tenant compte des principales caractéristiques des enquêtes. Gabriel Attal : ",
  "scénario où il est candidat à la place d'Édouard Philippe ; David Lisnard : scénario où il est candidat en plus ",
  "d'Édouard Philippe et de Bruno Retailleau. Dernière mise à jour : ", update_date, "."
)

# Latest estimates, highest first
latest <- plot_spline_estimates %>%
  filter(date == last_date) %>%
  mutate(across(c(median, starts_with("lower"), starts_with("upper")), ~ .x * 100)) %>%
  arrange(desc(median)) %>%
  mutate(
    short = candidate_labels[as.character(id_candidate)],
    value = fr_pct(median)
  )
cand_order <- as.character(latest$candidate)

# Month labels, with the year on the first month and on January
month_labels <- function(x) {
  x <- as.Date(x)
  lab <- sub("\\.$", "", format(x, "%b"))
  first <- !is.na(x) & seq_along(x) == min(which(!is.na(x)))
  add_year <- !is.na(x) & (format(x, "%m") == "01" | first)
  ifelse(add_year, paste0(lab, "\n", format(x, "%Y")), lab)
}

# Spread label positions vertically so that they are at least `gap` apart
spread_labels <- function(y, gap, lo = -Inf, hi = Inf) {
  o <- order(y); z <- y[o]
  for (iter in 1:200) {
    moved <- FALSE
    for (i in seq_along(z)[-1]) {
      d <- z[i] - z[i - 1]
      if (d < gap) { shift <- (gap - d) / 2; z[i] <- z[i] + shift; z[i - 1] <- z[i - 1] - shift; moved <- TRUE }
    }
    z <- pmin(pmax(z, lo), hi)
    if (!moved) break
  }
  out <- numeric(length(y)); out[o] <- z; out
}

# Common theme
theme_polls <- function() {
  theme_minimal(base_family = font_book, base_size = 9) +
    theme(
      plot.background    = element_rect(fill = "white", color = NA),
      panel.grid         = element_blank(),
      axis.title         = element_blank(),
      axis.text          = element_text(color = ink_3, size = 7.5),
      plot.title         = element_text(face = "bold", size = 14, color = ink_1, lineheight = 1.05,
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(size = 9.3, color = ink_2, lineheight = 1.25, margin = margin(b = 12)),
      plot.caption       = element_text(size = 6.4, color = ink_3, hjust = 0, lineheight = 1.3,
                                        margin = margin(t = 12)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      legend.position    = "none",
      plot.margin        = margin(16, 16, 10, 16)
    )
}

# Export as PNG (ragg, 400 dpi) and PDF (quartz, fonts embedded)
save_graph <- function(p, name, width, height) {
  ggsave(paste0(name, ".png"), p, width = width, height = height, dpi = 400,
         device = ragg::agg_png, bg = "white")
  quartz(type = "pdf", file = paste0(name, ".pdf"), width = width, height = height, bg = "white")
  print(p)
  invisible(dev.off())
}

# Plot sizes (inches)
evo_size  <- c(7.5, 5)
inst_size <- c(7.5, 5.6)



## Evolution plot: create and export ----

x_min    <- min(plot_spline_estimates$date) - 20  # room for the inline y labels
x_max    <- last_date + 95
x_name   <- last_date + 9
y_breaks <- seq(10, 40, 10)
label_size <- 2.75
value_size <- 2.4

# Label positions: name (bold), then value right after it
days_per_in <- as.numeric(x_max - x_min) / (evo_size[1] - 32 / 72)
name_width  <- systemfonts::string_width(latest$short, family = "Franklin Demi (measure)",
                                         size = label_size * .pt, res = 72) / 72
evo_labels <- latest %>%
  mutate(
    y_lab = spread_labels(median, gap = 1.75, lo = 1.1),
    x_val = x_name + (name_width * 1.06 + .045) * days_per_in,  # small margin for rendering differences
    col   = text_color(party_colors[as.character(candidate)])
  )

poll_plot <- ggplot(plot_spline_estimates, aes(x = date)) +

  # Grid and inline y labels
  geom_hline(yintercept = y_breaks, color = grid_col, linewidth = .25) +
  annotate("text", x = x_min, y = y_breaks + .5, label = ifelse(y_breaks == 40, "40 %", y_breaks),
           family = font_book, size = 2.6, color = ink_3, hjust = 0, vjust = 0) +
  geom_hline(yintercept = 0, color = base_col, linewidth = .35) +

  # Latest poll
  annotate("segment", x = last_date, xend = last_date, y = 0, yend = 41.5, color = "#BDBDBD", linewidth = .25) +
  annotate("text", x = last_date - 3, y = 41.5, label = paste("Dernier sondage :", last_date_lab),
           family = font_book, size = 2.5, color = ink_2, hjust = 1, vjust = 1) +

  # Medians (main candidates drawn last, on top)
  lapply(rev(cand_order), function(cand) {
    geom_line(data = plot_spline_estimates %>% filter(candidate == cand), aes(y = median * 100),
              color = party_colors[cand], linewidth = .7, lineend = "round")
  }) +
  geom_point(data = latest, aes(y = median, fill = candidate), shape = 21, size = 1.6,
             color = "white", stroke = .6) +

  # Labels
  geom_segment(data = evo_labels, aes(x = last_date + 3, xend = x_name - 2, y = median, yend = y_lab),
               color = "#C8C8C8", linewidth = .2) +
  geom_text(data = evo_labels, aes(x = x_name, y = y_lab - .42, label = short, color = I(col)),
            family = font_book, fontface = "bold", size = label_size, hjust = 0, vjust = 0) +
  geom_text(data = evo_labels, aes(x = x_val, y = y_lab - .42, label = value, color = I(col)),
            family = font_book, size = value_size, hjust = 0, vjust = 0) +

  scale_fill_manual(values = party_colors) +
  scale_x_date(breaks = seq(as.Date("2026-03-01"), last_date, by = "1 month"),
               labels = month_labels, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, 42), clip = "off") +
  labs(
    title = "Intentions de vote au premier tour de la présidentielle 2027",
    subtitle = paste0("Estimation de l'agrégateur (médiane des distributions a posteriori), en % des suffrages exprimés.\n",
                      "Premier tour le ", trimws(format(election_date, "%e %B %Y")), "."),
    caption = str_wrap(source_text, 165)
  ) +
  theme_polls() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.x = element_line(color = base_col, linewidth = .25),
    axis.ticks.length.x = unit(2.5, "pt"),
    axis.text.x  = element_text(margin = margin(t = 3), lineheight = 1.1)
  )

save_graph(poll_plot, "polls_france27_evolution", evo_size[1], evo_size[2])



## Current estimate plot: create and export ----

inst <- latest %>% mutate(row = rev(seq_len(n())))
n_c  <- nrow(inst)
x_breaks <- seq(0, 40, 10)

# Key for the four credible intervals
key_x <- c(25.5, 28.6, 31.7, 34.8)
key   <- tibble(xmin = key_x, xmax = key_x + 2.4, p = c(.85, .6, .4, .22),
                lab = c("50 %", "80 %", "90 %", "95 %"))
band <- function(lo, hi, p) {
  geom_linerange(aes(xmin = .data[[lo]], xmax = .data[[hi]],
                     color = I(tint(party_colors[as.character(candidate)], p))), linewidth = 2.6)
}

inst_plot <- inst %>%
  ggplot(aes(y = row)) +

  # Grid, baseline and x labels
  annotate("segment", x = x_breaks[-1], xend = x_breaks[-1], y = .5, yend = n_c + .5,
           color = grid_col, linewidth = .25) +
  annotate("segment", x = 0, xend = 0, y = .5, yend = n_c + .5, color = base_col, linewidth = .35) +
  annotate("text", x = x_breaks, y = .3, label = ifelse(x_breaks == 40, "40 %", x_breaks),
           family = font_book, size = 2.6, color = ink_3, vjust = 1) +

  # Credible intervals, medians and values
  band("lower95", "upper95", .22) +
  band("lower90", "upper90", .40) +
  band("lower80", "upper80", .60) +
  band("lower50", "upper50", .85) +
  geom_point(aes(x = median, fill = candidate), shape = 21, size = 2.3, color = "white", stroke = .7) +
  geom_text(aes(x = upper95 + .6, label = value), family = font_book, fontface = "bold", size = 2.85,
            color = ink_1, hjust = 0) +

  # Key
  annotate("rect", xmin = key$xmin, xmax = key$xmax, ymin = n_c + .78, ymax = n_c + .98,
           fill = ink_2, alpha = key$p) +
  annotate("text", x = (key$xmin + key$xmax) / 2, y = n_c + 1.12, label = key$lab,
           family = font_book, size = 2.2, color = ink_2, vjust = 0) +
  annotate("text", x = key_x[1] - .6, y = n_c + .88, label = "Intervalles de crédibilité",
           family = font_book, size = 2.2, color = ink_2, hjust = 1) +

  scale_fill_manual(values = party_colors) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(breaks = inst$row, labels = as.character(inst$candidate),
                     expand = expansion(add = c(.85, 1.35))) +
  coord_cartesian(xlim = c(0, 42), clip = "off") +
  labs(
    title = "Intentions de vote au premier tour de la présidentielle 2027",
    subtitle = paste0("Estimation de l'agrégateur au ", last_date_lab, ", en % des suffrages exprimés."),
    caption = str_wrap(source_text, 165)
  ) +
  theme_polls() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = font_med, size = 8.6, color = ink_1, margin = margin(r = 7))
  )

save_graph(inst_plot, "polls_france27_latest", inst_size[1], inst_size[2])
