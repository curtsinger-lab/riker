#!/usr/bin/env Rscript
suppressMessages(library(ggthemes))
suppressMessages(library(tidyverse))
suppressMessages(library(lemon))
suppressMessages(library(scales))

theme_flashrelate <-
  theme_few() +
  theme(
    text = element_text(family='Times'),
    plot.title = element_text(size = 9, face='bold', family='Times'),
    strip.background = element_rect(color='dark gray', linetype=0.5),
    panel.border = element_rect(colour='gray'),
    legend.position = 'bottom',
    legend.key = element_rect(color=NA),
    legend.text = element_text(size = 8, family='Times'),
    legend.title = element_text(size = 8, family='Times'),
    axis.title.y = element_text(size = 9, family='Times'),
    axis.title.x = element_text(size = 9, family='Times')
  )
# get the current working directory
OUTPUT_DIR = getwd();

# read in command-line arguments
args = commandArgs(trailingOnly=TRUE)
if (length(args) < 1) {
   cat("Usage: ./plots.R <input CSV>")
   cat("  This program will read in <input CSV> and generate a set of PDF plots in the current directory.")
   quit(status=1)
}

# read data in
data <- read.csv(args[1])

# convert selected columns to factors
data$benchmark_name <- as.factor(data$benchmark_name)
data$tool <- as.factor(data$tool)
data$build_task <- as.factor(data$build_task)
data$return_code <- as.factor(data$return_code)

# phase is ordinal data
data$phase <- factor(data$phase, order = TRUE, levels = c("0", "1", "2", "3"))

# ensure there are no duplicate rows
data <- data %>% distinct(docker_mode, benchmark_name, tool, build_task, phase, .keep_all = TRUE)

# prepare phase data for no-docker plot
data_combined_no_docker <- data %>%
  unite(col = "benchmark", tool:build_task, sep="-", remove=TRUE) %>%
  mutate(elapsed_ms = elapsed_ns / 1000000) %>%
  filter(docker_mode == FALSE) %>%
  select(-c(docker_mode))

# prepare phase data for docker plot
data_combined_docker <- data %>%
  unite(col = "benchmark", tool:build_task, sep="-", remove=TRUE) %>%
  mutate(elapsed_ms = elapsed_ns / 1000000) %>%
  filter(docker_mode == TRUE) %>%
  select(-c(docker_mode))

# plot stacked bar for phase time (no-docker)
pt1 <- ggplot(data_combined_no_docker, aes(x = benchmark, y = elapsed_ms)) +
  theme_flashrelate + # simple paper-like theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # turn x labels sideways
  scale_fill_manual(values = c("0" = "#0072b2", "1" = "#e69f00", "2" = "#d55e00", "3" = "#56b4e9")) + # pretty colors
  geom_col(aes(fill = phase), width = 0.7, position = position_stack(reverse = TRUE)) + # use bar chart
  facet_rep_wrap(vars(benchmark_name), repeat.tick.labels=TRUE) + # facet
  theme(axis.title.x=element_blank()) + # remove x axis label
  labs(title="Phase Cost Breakdown (Bare Metal)", y = "Elapsed Time (ms)") + ## some labels
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  scale_y_continuous(labels = comma) # don't use scientific notation
pt1
ggsave(pt1, filename=paste(OUTPUT_DIR, 'phase_time_no-docker.pdf', sep="/"), width=11, height=8.5, device=pdf)

# plot stacked bar for phase time (docker)
pt2 <- ggplot(data_combined_docker, aes(x = benchmark, y = elapsed_ms)) +
  theme_flashrelate + # simple paper-like theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # turn x labels sideways
  scale_fill_manual(values = c("0" = "#0072b2", "1" = "#e69f00", "2" = "#d55e00", "3" = "#56b4e9")) + # pretty colors
  geom_col(aes(fill = phase), width = 0.7, position = position_stack(reverse = TRUE)) + # use bar chart
  facet_rep_wrap(vars(benchmark_name), repeat.tick.labels=TRUE) + # facet
  theme(axis.title.x=element_blank()) + # remove x axis label
  labs(title="Phase Cost Breakdown (Docker)", y = "Elapsed Time (ms)") + ## some labels
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  scale_y_continuous(labels = comma) # don't use scientific notation
pt2
ggsave(pt2, filename=paste(OUTPUT_DIR, 'phase_time_docker.pdf', sep="/"), width=11, height=8.5, device=pdf)

