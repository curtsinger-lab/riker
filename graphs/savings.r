#!/usr/bin/env Rscript
suppressMessages(library(ggplot2))
suppressMessages(library(ggthemes))
#suppressMessages(library(tidyverse))
#suppressMessages(library(lemon))
#suppressMessages(library(scales))

data <- read.csv('results/case-study.csv')

theme_flashrelate <-
  theme_bw() +
  theme(
    text = element_text(family='Times'),
    plot.title = element_text(size = 9, face='bold', family='Times', margin=margin(0, 0, 0, 0, 'in')),
    strip.background = element_rect(color='dark gray', linetype=0.5),
    panel.border = element_rect(colour='gray'),
    legend.position = 'right',
    legend.key = element_rect(color=NA),
    legend.key.size = unit(0.15, 'in'),
    legend.text = element_text(size = 8, family='Times'),
    legend.title = element_text(size = 8, family='Times'),
    axis.title.y = element_text(size = 8, family='Times'),
    axis.title.x = element_text(size = 8, family='Times'),
    strip.text.x = element_text(size = 8, family='Times'),
    strip.text.y = element_text(size = 8, family='Times'),
    plot.margin = margin(0.05, 0.1, 0, 0, 'in'),
    panel.spacing = unit(0.05, 'in'),
    legend.margin = margin(-0.1, 0, 0, 0, 'in')
  ) +
  theme(plot.title = element_text(hjust = 0.5)) # Center title
  
# Create a line graph of commands run
plot <- ggplot(data=data, aes(x=program, y=savings, fill=build_tool)) +
  geom_col(position='dodge') +
  coord_flip() +
  theme_flashrelate +
  scale_y_continuous(limits=c(NA, 1.0), expand=c(0.01, 0.01), labels = scales::percent_format(accuracy=1)) +
  scale_fill_manual(values = c("Riker" = "#0072b2", "Default" = "#d55e00")) +
  theme(axis.text.x = element_text(hjust = 1)) +
  labs(title='Time Saved by Building Incrementally', y='Reduction in Build Time Relative to Full Builds (higher is better)', fill='Build Tool') +
  xlab(NULL) +
  theme(strip.background = element_blank(), strip.placement = "outside")
  
ggsave(plot, filename='graphs/savings.pdf', width=3.4, height=1.35)
