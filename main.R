##### Welfare outcomes of segmentations under noise - related to Cummings et al (2020)
# Purpose of Programm: Industrial Organizations Class HSG
# Author: Erik Senn, erik.senn@gmx.de

# What is it?
  # Idea: Compute possible welfare outcomes of segmentations for given value + type set and noise function.
  # Output: Dataframe with optimal outcome for each tested segmentation, Plots for welfare outcomes with and without segmentations
  # Current Setting: Example of Cummings et al. Simple random noise and 3 valuations / types
  # Limitations: only tested for length value set = length type set. Special Points (red) in plots are customized, might not suit for every noise level
  # Optional ToDOs:
    # Compute optimal segmentation via linear programm (Cummings et al., Theorem 2.1)
    # Define new noise functions
  # Theoretical background: Bergemann et al. (2015) / Cummings et al (2020)

# Required Files:
# main.R
# functions.R

#preamble
library(matlib)
library(rlist)
library(tidyverse)
library(stats)
library(lpSolve)
library(plyr)
library(ggnewscale)

setwd(
  "C:/Users/eriks/OneDrive - Universität St.Gallen/MiQE_F/Industrial Organizations/Presentation"
)
output <- paste0(getwd(), "/output/")

# functions
source("functions.R") # functions

# segmentation rules
supress_print_messages <-
  TRUE # supress printed checks from optimization fucntion
intermediary_chooses_segmentation <-
  F # TRUE / FALSE. No impact at the moment
pricing_rule <-
  "min" # "min" or "max". NOTE: at the moment both options are always calculated
weight_consumer <- 0.5 # weight for consumer utility

###### Input Parameters #####
certainty_level_input <- 0.6 # certainty level (z)
value_set <- c(1, 2, 3) # given
type_set <-
  c(1, 2, 3) # given, #types has to match # of values (otherwise change implementation)
pdf_value <- rep(1 / 3, 3) # given
pdf_type <-
  rep(1 / 3, 3) # given, has to match value and noise. Alternative: start from value OR type and calculate the other one

# Noise function
pdf_value_f_t_fct <- function(type, certainty_level, n_types) {
  f_t <- rep(((1 - certainty_level) / (n_types - 1)), n_types)
  f_t[type] = certainty_level
  return(f_t)
}

##### Predefine Segmentations #####
# Special Segmentations
segmentation_ppd <-
  rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)) # types as cols, segments as rows. arbitrary number of segments
segmentation_max_cs <-
  rbind(c(1, 1 / 3, 2 / 3), c(0, 1 / 6, 1 / 3), c(0, 1 / 2, 0)) # bergemann segmentation for max CS
segmentation_monopoly <-
  rbind(c(1, 1, 1), c(0, 0, 0), c(0, 0, 0)) # monopoly surplus

segmentations_special <-
  list(segmentation_ppd, segmentation_max_cs, segmentation_monopoly) # Lists for specific segmentations
names(segmentations_special) <-
  paste0(c("ppd", "max_cs", "monopoly"), "_no_noise")

# grid of segmentations
segmentation_grid_valid_and_invalid <- list()
segmentation_grid_intervals <- 9
type_steps <-
  seq(from = 0,
      to = 1,
      length.out = segmentation_grid_intervals)
iter_grid = 1
for (type1_s1 in type_steps) {
  for (type1_s2 in type_steps) {
    for (type2_s1 in type_steps) {
      for (type2_s2 in type_steps) {
        for (type3_s1 in type_steps) {
          for (type3_s2 in type_steps) {
            segment_loop <- t(rbind(
              c(type1_s1, type1_s2, 1 - type1_s1 - type1_s2),
              c(type2_s1, type2_s2, 1 - type2_s1 - type2_s2),
              c(type3_s1, type3_s2, 1 - type3_s1 - type3_s2)
            )) # transpose to have segments as rows, types as cols
            segmentation_grid_valid_and_invalid[[iter_grid]] <-
              segment_loop
            iter_grid <- iter_grid + 1
          }
        }
      }
    }
  }
}

# remove grid segments where any element is not between 0 and 1 & colSum (typesum) not equal to one
# note: some grid segments will be the symmetric, those are not removed
is_valid_segment <- function(segment) {
  if (!(all(colSums(segment) == rep(1, ncol(segment))))) {
    # sum type over segments == 1?
    valid = FALSE
  } else if (any(segment < 0) |
             any(segment > 1)) {
    # any entries <0 or >1?
    valid = FALSE
  } else {
    valid = TRUE
  }
  return(valid)
}
segmentation_grid <-
  segmentation_grid_valid_and_invalid[sapply(segmentation_grid_valid_and_invalid, is_valid_segment)] # remove invalid segments
names(segmentation_grid) <-
  paste0("grid_seg_", seq(1:length(segmentation_grid))) # name segments

# Names segmentations for clarification
name_segmentation <- function(segmentation) {
  colnames(segmentation) <-
    paste0("type", type_set)
  rownames(segmentation) <-
    paste0("segment", c(1:nrow(segmentation)))
  return(segmentation)
}
segmentations_special <-
  lapply(segmentations_special, name_segmentation)
segmentation_grid <- lapply(segmentation_grid, name_segmentation)

##### Calculate Welfare outcomes #####
### Min Pricing rule
## No Noise
# special segmentations
results_segmentations_special_no_noise_min_pricing <-
  lapply(
    segmentations_special,
    optimal_pricing_given_segmentation,
    "min",
    weight_consumer,
    1,
    # no noise
    value_set,
    type_set,
    pdf_value,
    pdf_type,
    pdf_value_f_t_fct,
    supress_print_messages
  )

# grid of segmentations
results_segmentations_grid_no_noise_min_pricing <-
  lapply(
    segmentation_grid,
    optimal_pricing_given_segmentation,
    "min",
    weight_consumer,
    1,
    # no noise
    value_set,
    type_set,
    pdf_value,
    pdf_type,
    pdf_value_f_t_fct,
    supress_print_messages
  )


## With Noise
# special segmentations
results_segmentations_special_noise_min_pricing <-
  lapply(
    segmentations_special,
    optimal_pricing_given_segmentation,
    "min",
    weight_consumer,
    certainty_level_input,
    value_set,
    type_set,
    pdf_value,
    pdf_type,
    pdf_value_f_t_fct,
    supress_print_messages
  )

# grid of segmentations
results_segmentations_grid_noise_min_pricing <-
  lapply(
    segmentation_grid,
    optimal_pricing_given_segmentation,
    "min",
    weight_consumer,
    certainty_level_input,
    value_set,
    type_set,
    pdf_value,
    pdf_type,
    pdf_value_f_t_fct,
    supress_print_messages
  )
### Max Pricing rule
## No Noise
# special segmentations
results_segmentations_special_no_noise_max_pricing <-
  lapply(
    segmentations_special,
    optimal_pricing_given_segmentation,
    "max",
    weight_consumer,
    1,
    # no noise
    value_set,
    type_set,
    pdf_value,
    pdf_type,
    pdf_value_f_t_fct,
    supress_print_messages
  )

# grid of segmentations
results_segmentations_grid_no_noise_max_pricing <-
  lapply(
    segmentation_grid,
    optimal_pricing_given_segmentation,
    "max",
    weight_consumer,
    1,
    # no noise
    value_set,
    type_set,
    pdf_value,
    pdf_type,
    pdf_value_f_t_fct,
    supress_print_messages
  )

## With Noise
# special segmentations
results_segmentations_special_noise_max_pricing <-
  lapply(
    segmentations_special,
    optimal_pricing_given_segmentation,
    "max",
    weight_consumer,
    certainty_level_input,
    value_set,
    type_set,
    pdf_value,
    pdf_type,
    pdf_value_f_t_fct,
    supress_print_messages
  )

# grid of segmentations
results_segmentations_grid_noise_max_pricing <-
  lapply(
    segmentation_grid,
    optimal_pricing_given_segmentation,
    "max",
    weight_consumer,
    certainty_level_input,
    value_set,
    type_set,
    pdf_value,
    pdf_type,
    pdf_value_f_t_fct,
    supress_print_messages
  )
### Combine all results on one dataframe
# Combine results in dataframe
welfare_results <- NULL

# special points
for (i in 1:length(results_segmentations_special_no_noise_min_pricing)) {
  ## min pricing
  # no noise
  welfare_results <-
    rbind(
      welfare_results,
      results_segmentations_special_no_noise_min_pricing[[i]]$optimal_outcome_summary
    )
  rownames(welfare_results)[nrow(welfare_results)] <-
    paste0(
      names(results_segmentations_special_no_noise_min_pricing)[i],
      "_no_noise_",
      "min_pricing"
    )
  # noise
  welfare_results <-
    rbind(
      welfare_results,
      results_segmentations_special_noise_min_pricing[[i]]$optimal_outcome_summary
    )
  rownames(welfare_results)[nrow(welfare_results)] <-
    paste0(
      names(results_segmentations_special_noise_min_pricing)[i],
      "_noise_",
      "min_pricing"
    )
  
  ## max pricing
  # no noise
  welfare_results <-
    rbind(
      welfare_results,
      results_segmentations_special_no_noise_max_pricing[[i]]$optimal_outcome_summary
    )
  rownames(welfare_results)[nrow(welfare_results)] <-
    paste0(
      names(results_segmentations_special_no_noise_max_pricing)[i],
      "_no_noise_",
      "max_pricing"
    )
  # noise
  welfare_results <-
    rbind(
      welfare_results,
      results_segmentations_special_noise_max_pricing[[i]]$optimal_outcome_summary
    )
  rownames(welfare_results)[nrow(welfare_results)] <-
    paste0(
      names(results_segmentations_special_noise_max_pricing)[i],
      "_noise_",
      "max_pricing"
    )
}

# grid of segments
for (i in 1:length(results_segmentations_grid_no_noise_min_pricing)) {
  ## min pricing
  # no noise
  welfare_results <-
    rbind(
      welfare_results,
      results_segmentations_grid_no_noise_min_pricing[[i]]$optimal_outcome_summary
    )
  rownames(welfare_results)[nrow(welfare_results)] <-
    paste0(
      names(results_segmentations_grid_no_noise_min_pricing)[i],
      "_no_noise_",
      "min_pricing"
    )
  # noise
  welfare_results <-
    rbind(
      welfare_results,
      results_segmentations_grid_noise_min_pricing[[i]]$optimal_outcome_summary
    )
  rownames(welfare_results)[nrow(welfare_results)] <-
    paste0(names(results_segmentations_grid_noise_min_pricing)[i],
           "_noise_",
           "min_pricing")
  
  ## max pricing
  # no noise
  welfare_results <-
    rbind(
      welfare_results,
      results_segmentations_grid_no_noise_max_pricing[[i]]$optimal_outcome_summary
    )
  rownames(welfare_results)[nrow(welfare_results)] <-
    paste0(
      names(results_segmentations_grid_no_noise_max_pricing)[i],
      "_no_noise_",
      "max_pricing"
    )
  # noise
  welfare_results <-
    rbind(
      welfare_results,
      results_segmentations_grid_noise_max_pricing[[i]]$optimal_outcome_summary
    )
  rownames(welfare_results)[nrow(welfare_results)] <-
    paste0(names(results_segmentations_grid_noise_max_pricing)[i],
           "_noise_",
           "max_pricing")
}

welfare_results$special_point <- FALSE
welfare_results$special_point[!grepl("grid", rownames(welfare_results))] = TRUE

##### Plot Results #####

## Reformat result data for plotting

# get special points with noise
max_cs_point_noise <-
  max(welfare_results$consumer_surplus[welfare_results$certainty_level ==
                                         certainty_level_input])
max_profit_point_noise <-
  max(welfare_results$profit[welfare_results$certainty_level == certainty_level_input])

# keep only unique points and collect summary statistics for each point (mean pricing rule and number of points)
# recode pricing rule to binary (to take means later)
# generate special_point_min_pricing_rule (interaction) to only plot special points with min price rule
# summarize
welfare_results_plot <- ungroup(welfare_results) %>%
  mutate(pricing_rule = ifelse(pricing_rule == "max", 1, ifelse(pricing_rule ==
                                                                  "min", 0, NA))) %>%
  group_by(certainty_level) %>%
  mutate(plot_special_point = case_when((consumer_surplus == max(consumer_surplus)) ~ 1,
                                        (profit == max(profit)) ~ 1,
                                        (consumer_surplus == 1 / 3 &
                                           profit == 4 / 3) ~ 1,
                                        TRUE ~ 0
  )) %>% # manually add max CS point C for noise cause dyplr doesnt work with max fct
  mutate(plot_special_point = case_when(((consumer_surplus == max_cs_point_noise) &
                                           certainty_level == certainty_level_input
  ) ~ 1,
  ((
    abs(profit - max_profit_point_noise) < 0.001
  ) &
    certainty_level == certainty_level_input) ~ 1,
  TRUE ~ plot_special_point
  )) %>% # GROUP DOES NOT FULLY WORK with max -< hardcode add max CS point C for noise cause dyplr doesnt work with max fct
  mutate(
    plot_special_point_min_pricing_rule = ifelse(plot_special_point == 1 &
                                                   pricing_rule == 0, 1, 0)
  ) %>%
  group_by(
    special_point,
    plot_special_point,
    plot_special_point_min_pricing_rule,
    profit,
    consumer_surplus,
    certainty_level
  ) %>%
  dplyr::summarize(number_points = n(),
                   mean_pricing_rule = mean(pricing_rule)) %>%
  ungroup


## Scatter Plots welfare results
# Plot without noise
# colorscale for pricing rule
plot_no_noise_blanc <-
  ggplot(
    welfare_results_plot[(welfare_results_plot$certainty_level == 1) , ],
    aes(
      x = consumer_surplus,
      y = profit,
      color = mean_pricing_rule,
      fill = plot_special_point_min_pricing_rule
    )
  ) +
  geom_segment(aes(
    x = 0,
    y = 4 / 3,
    xend = 2 / 3,
    yend = 4 / 3
  ),
  color = "black",
  size = 1) +
  geom_segment(aes(
    x = 0,
    y = 0,
    xend = 0,
    yend = 2
  ),
  color = "black",
  size = 1) +
  geom_segment(aes(
    x = 0,
    y = 2,
    xend = 2 / 3,
    yend = 4 / 3
  ),
  color = "black",
  size = 1) +
  geom_point(
    aes(
      x = consumer_surplus * plot_special_point_min_pricing_rule,
      y = profit * plot_special_point_min_pricing_rule,
      fill = plot_special_point_min_pricing_rule
    ),
    color = "red",
    size = 4
  ) +
  geom_text(
    aes(consumer_surplus, profit),
    label = "A",
    color = "red",
    size = 7,
    vjust = 1.5,
    hjust = 0,
    data = subset(welfare_results_plot, consumer_surplus == 1 / 3 &
                    profit == 4 / 3)
  ) +
  geom_text(
    aes(consumer_surplus, profit),
    label = "B",
    color = "red",
    size = 7,
    hjust = -1,
    data = subset(welfare_results_plot, consumer_surplus == 0 &
                    profit == 2)
  ) +
  geom_text(
    aes(consumer_surplus, profit),
    label = "C",
    color = "red",
    size = 7,
    vjust = 1.5,
    hjust = 0.5,
    data = subset(welfare_results_plot, consumer_surplus == 2 / 3 &
                    profit == 4 / 3)
  ) +
  coord_cartesian(xlim = c(0, 0.84)) +
  coord_cartesian(ylim = c(1.2, 2)) +
  labs(x = "Consumer Surplus") +
  labs(y = "Firm Profit") +
  theme_bw() +
  guides(fill = FALSE) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold")) +
  ggtitle("")
plot_no_noise_blanc
ggsave(
  file = paste0(output, "scatter_no_noise_blanc.png"),
  width = 7,
  height = 7,
  dpi = 600
)


plot_no_noise_blanc + geom_point(aes(color = mean_pricing_rule), size =
                                   1.3) +
  #geom_jitter(height = 0.005, width = 0.005)+
  labs(color = "Max-Pricing-Rule Share") +
  theme(
    legend.position = c(0.78, 0.72),
    legend.box.background = element_rect(colour = "white")
  )
ggsave(
  file = paste0(output, "scatter_no_noise.png"),
  width = 7,
  height = 7,
  dpi = 600
)

# colorscale for density of points
ggplot(
  welfare_results_plot[(welfare_results_plot$certainty_level == 1) , ],
  aes(x = consumer_surplus, y = profit, fill = plot_special_point_min_pricing_rule)
) +
  geom_segment(aes(
    x = 0,
    y = 4 / 3,
    xend = 2 / 3,
    yend = 4 / 3
  ),
  color = "black",
  size = 1) +
  geom_segment(aes(
    x = 0,
    y = 0,
    xend = 0,
    yend = 2
  ),
  color = "black",
  size = 1) +
  geom_segment(aes(
    x = 0,
    y = 2,
    xend = 2 / 3,
    yend = 4 / 3
  ),
  color = "black",
  size = 1) +
  geom_point(
    aes(
      x = consumer_surplus * plot_special_point_min_pricing_rule,
      y = profit * plot_special_point_min_pricing_rule,
      fill = plot_special_point_min_pricing_rule
    ),
    color = "red",
    size = 4
  ) +
  geom_text(
    aes(consumer_surplus, profit),
    label = "A",
    color = "red",
    size = 7,
    vjust = 1.5,
    hjust = 0,
    data = subset(welfare_results_plot, consumer_surplus == 1 / 3 &
                    profit == 4 / 3)
  ) +
  geom_text(
    aes(consumer_surplus, profit),
    label = "B",
    color = "red",
    size = 7,
    hjust = -1,
    data = subset(welfare_results_plot, consumer_surplus == 0 &
                    profit == 2)
  ) +
  geom_text(
    aes(consumer_surplus, profit),
    label = "C",
    color = "red",
    size = 7,
    vjust = 1.5,
    hjust = 0.5,
    data = subset(welfare_results_plot, consumer_surplus == 2 / 3 &
                    profit == 4 / 3)
  ) +
  coord_cartesian(xlim = c(0, 0.84)) +
  coord_cartesian(ylim = c(1.2, 2)) +
  labs(x = "Consumer Surplus") +
  labs(y = "Firm Profit") +
  theme_bw() +
  guides(fill = FALSE) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold")) +
  ggtitle("") + geom_point(size = 1, alpha = 0.3) +
  theme(
    legend.position = c(0.78, 0.72),
    legend.box.background = element_rect(colour = "white")
  )
ggsave(
  file = paste0(output, "scatter_no_noise_no_scale.png"),
  width = 7,
  height = 7,
  dpi = 600
)

# Plot with noise
# with colorscale for pricing rule
plot_noise_blanc <-
  ggplot(
    welfare_results_plot[(welfare_results_plot$certainty_level == certainty_level_input) , ],
    aes(
      x = consumer_surplus,
      y = profit,
      color = mean_pricing_rule,
      fill = plot_special_point_min_pricing_rule
    )
  ) +
  geom_segment(aes(
    x = 0,
    y = 4 / 3,
    xend = 2 / 3,
    yend = 4 / 3
  ),
  color = "black",
  size = 1) +
  geom_segment(aes(
    x = 0,
    y = 0,
    xend = 0,
    yend = 2
  ),
  color = "black",
  size = 1) +
  geom_segment(aes(
    x = 0,
    y = 2,
    xend = 2 / 3,
    yend = 4 / 3
  ),
  color = "black",
  size = 1) +
  geom_point(
    aes(
      x = consumer_surplus * plot_special_point_min_pricing_rule,
      y = profit * plot_special_point_min_pricing_rule,
      fill = plot_special_point_min_pricing_rule
    ),
    color = "red",
    size = 4
  ) +
  geom_text(
    aes(consumer_surplus, profit),
    label = "A",
    color = "red",
    size = 7,
    vjust = 1.5,
    hjust = 0,
    data = subset(welfare_results_plot, consumer_surplus == 1 / 3 &
                    profit == 4 / 3)
  ) +
  geom_text(
    aes(consumer_surplus, profit),
    label = "B'",
    color = "red",
    size = 7,
    hjust = 0,
    data = subset(welfare_results_plot, profit == max_profit_point_noise)
  ) +
  geom_text(
    aes(consumer_surplus, profit),
    label = "C'",
    color = "red",
    size = 7,
    vjust = 1.5,
    hjust = 0.5,
    data = subset(welfare_results_plot, consumer_surplus == max_cs_point_noise)
  ) +
  coord_cartesian(xlim = c(0, 0.84)) +
  coord_cartesian(ylim = c(1.2, 2)) +
  labs(x = "Consumer Surplus") +
  labs(y = "Firm Profit") +
  theme_bw() +
  guides(fill = FALSE) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold")) +
  ggtitle("")
plot_noise_blanc
ggsave(
  file = paste0(
    output,
    "scatter_noise_blanc_z_",
    certainty_level_input,
    ".png"
  ),
  width = 7,
  height = 7,
  dpi = 600
)

plot_noise_blanc + geom_point(aes(color = mean_pricing_rule), size = 1.3) +
  #geom_jitter(height = 0.005, width = 0.005)+
  labs(color = "Max-Pricing-Rule Share") +
  theme(
    legend.position = c(0.78, 0.72),
    legend.box.background = element_rect(colour = "white")
  )
ggsave(
  file = paste0(output, "scatter_noise_z_", certainty_level_input, ".png"),
  width = 7,
  height = 7,
  dpi = 600
)

# with colorscale for density
ggplot(
  welfare_results_plot[(welfare_results_plot$certainty_level == certainty_level_input) , ],
  aes(x = consumer_surplus, y = profit, fill = plot_special_point_min_pricing_rule)
) +
  geom_segment(aes(
    x = 0,
    y = 4 / 3,
    xend = 2 / 3,
    yend = 4 / 3
  ),
  color = "black",
  size = 1) +
  geom_segment(aes(
    x = 0,
    y = 0,
    xend = 0,
    yend = 2
  ),
  color = "black",
  size = 1) +
  geom_segment(aes(
    x = 0,
    y = 2,
    xend = 2 / 3,
    yend = 4 / 3
  ),
  color = "black",
  size = 1) +
  geom_point(
    aes(
      x = consumer_surplus * plot_special_point_min_pricing_rule,
      y = profit * plot_special_point_min_pricing_rule,
      fill = plot_special_point_min_pricing_rule
    ),
    color = "red",
    size = 4
  ) +
  geom_text(
    aes(consumer_surplus, profit),
    label = "A",
    color = "red",
    size = 7,
    vjust = 1.5,
    hjust = 0,
    data = subset(welfare_results_plot, consumer_surplus == 1 / 3 &
                    profit == 4 / 3)
  ) +
  geom_text(
    aes(consumer_surplus, profit),
    label = "B'",
    color = "red",
    size = 7,
    hjust = 0,
    data = subset(welfare_results_plot, (
      abs(profit - max_profit_point_noise) < 0.002
    ))
  ) +
  geom_text(
    aes(consumer_surplus, profit),
    label = "C'",
    color = "red",
    size = 7,
    vjust = 1.5,
    hjust = 0.5,
    data = subset(welfare_results_plot, consumer_surplus == max_cs_point_noise)
  ) +
  coord_cartesian(xlim = c(0, 0.84)) +
  coord_cartesian(ylim = c(1.2, 2)) +
  labs(x = "Consumer Surplus") +
  labs(y = "Firm Profit") +
  theme_bw() +
  guides(fill = FALSE) +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold")) +
  ggtitle("") +
  geom_point(size = 1, alpha = 0.3) +
  theme(
    legend.position = c(0.78, 0.72),
    legend.box.background = element_rect(colour = "white")
  )
ggsave(
  file = paste0(
    output,
    "scatter_noise_no_scale_z_",
    certainty_level_input,
    ".png"
  ),
  width = 7,
  height = 7,
  dpi = 600
)


save.image(file = "results_segmentation.Rdata")


# # special case: always monopoly pricing plot - take only first point (as all points are the same)
# welfare_results_plot_special <- filter(welfare_results_plot,certainty_level !=1 )[1,]
#
# ggplot(welfare_results_plot_special[(welfare_results_plot_special$certainty_level==certainty_level_input) ,], aes(x=consumer_surplus, y=profit,fill=plot_special_point_min_pricing_rule))+
#   geom_segment(aes(x=0,y=4/3,xend = 2/3,yend=4/3),color = "black",size=1)+
#   geom_segment(aes(x=0,y=0,xend = 0,yend=2),color = "black",size=1)+
#   geom_segment(aes(x=0,y=2,xend = 2/3,yend=4/3),color = "black",size=1)+
#   geom_point(aes(x=consumer_surplus, y = profit, fill=plot_special_point_min_pricing_rule), color = "red", size=4)+
#   geom_text(aes(consumer_surplus,profit), label = "A", color="red",size=7,vjust=1.5, hjust=0, data=subset(welfare_results_plot,consumer_surplus ==1/3 & profit ==4/3 ))+
#   geom_text(aes(consumer_surplus,profit), label = "B'", color="red",size=7,vjust=-0.5, hjust=1.5, data=subset(welfare_results_plot,consumer_surplus ==1/3 & profit ==4/3 ))+
#   geom_text(aes(consumer_surplus,profit), label = "C'", color="red",size=7,vjust=-0.5, hjust=0, data=subset(welfare_results_plot,consumer_surplus ==1/3 & profit ==4/3 ))+
#   #geom_text(aes(consumer_surplus,profit), label = "B'", color="red",size=7, hjust=0, data=subset(welfare_results_plot, profit==max_profit_point_noise))+
#   #geom_text(aes(consumer_surplus,profit), label = "C'", color="red", size=7,vjust=1.5, hjust=0.5, data=subset(welfare_results_plot,consumer_surplus ==max_cs_point_noise))+
#   coord_cartesian(xlim = c(0,0.84))+
#   coord_cartesian(ylim = c(1.2,2))+
#   labs(x = "Consumer Surplus")+
#   labs(y = "Firm Profit")+
#   theme_bw()+
#   guides(fill = FALSE)+
#   theme(plot.title = element_text(size=10, face="bold"))+
#   theme(axis.text=element_text(size=10),
#         axis.title=element_text(size=10,face="bold"))+
#   ggtitle("")+
#   geom_point(size=1,alpha=0.3)+
#   theme(legend.position = c(0.78,0.72), legend.box.background = element_rect(colour = "white"))
# ggsave(file=paste0(output,"scatter_noise_no_scale_only_monopoly_z_",certainty_level_input,".png"), width=7, height=7, dpi=600)
#

#####additional non-used plots ######
# ## Scatter Plots welfare results
# ggplot(welfare_results_plot[welfare_results_plot$certainty_level==1,], aes(x=consumer_surplus, y=profit, color=mean_pricing_rule))+
#   #geom_jitter(height = 0.005, width = 0.005)+
#   geom_segment(aes(x=0,y=4/3,xend = 2/3,yend=4/3),color = "black",size=1)+
#   geom_segment(aes(x=0,y=0,xend = 0,yend=2),color = "black",size=1)+
#   geom_segment(aes(x=0,y=2,xend = 2/3,yend=4/3),color = "black",size=1)+
#   geom_point(aes(color=mean_pricing_rule))+
#   theme(axis.text.y=element_blank())+
#   coord_cartesian(xlim = c(0,0.78))+
#   coord_cartesian(ylim = c(1.2,2))+
#   labs(x = "Consumer Surplus")+
#   labs(y = "Firm Profit")+
#   labs(color = "max. PR share") +
#   theme_bw()+
#   theme(legend.position = c(0.8,0.6), legend.box.background = element_rect(colour = "white"))+
#   ggtitle("") +
#   theme(plot.title = element_text(size=10, face="bold"))+
#   theme(axis.text=element_text(size=10),
#         axis.title=element_text(size=10,face="bold"))+
#   ggsave(file=paste0(output,"scatter_no_noise.png"), width=6, height=4, dpi=300)
#
# ggplot(welfare_results_test[welfare_results_test$certainty_level==0.8,], aes(x=consumer_surplus, y=profit,color = pricing_rule))+
#   # geom_jitter(height = 0.005, width = 0.005)+
#   stat_summary(fun.data = "pricing_rule_mean",aes(x=consumer_surplus, y =profit,color = pricing_rule))+
#   labs(color = "Pricing Rule") +
#   theme(axis.text.y=element_blank())+
#   scale_x_continuous(limits = c(0,0.8)) +
#   scale_y_continuous(limits = c(1,2)) +
#   geom_hline(yintercept=4/3, linetype="dashed", color = "red") +
#   geom_vline(xintercept=0, linetype="dashed", color = "red") +
#   geom_abline(intercept= 2, slope=-1, linetype="dashed", color = "red") +
#   labs(x = "Consumer Surplus")+
#   labs(y = "Firm Profit")+
#   theme_bw()+
#   theme(legend.position = "bottom", legend.box.background = element_rect(colour = "black"))+
#   ggtitle("Noise") +
#   theme(plot.title = element_text(size=10, face="bold"))+
#   theme(axis.text=element_text(size=10),
#         axis.title=element_text(size=10,face="bold"))+
#   ggsave(file=paste0(output,"scatter_noise.png"), width=6, height=4, dpi=300)

###### Optimal segmentation for intermediary #####

###### 1) brute force: test the grid of coefficients and take max weighed welfare #######



###### 2) via Linear programm  #####
# # Set coefficients of the objective function
# f.obj <- c(5, 7)
#
# # Set matrix corresponding to coefficients of constraints by rows
# # Do not consider the non-negative constraint; it is automatically assumed
# f.con <- matrix(c(1, 0,
#                   2, 3,
#                   1, 1), nrow = 3, byrow = TRUE)
#
# # Set unequality signs
# f.dir <- c("<=",
#            "<=",
#            "<=")
#
# # Set right hand side coefficients
# f.rhs <- c(16,
#            19,
#            8)
#
# # Final value (z)
# lp("max", f.obj, f.con, f.dir, f.rhs)
