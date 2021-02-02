library(dplyr)
library(ggplot2)
library(ggthemes)

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

# read it in
data <- read.csv("-riker-nightly-logs-2021-02-01_19-00-output.csv")

# convert selected columns to factors
data$benchmark_name <- as.factor(data$benchmark_name)
data$tool <- as.factor(data$tool)
data$build_task <- as.factor(data$build_task)
data$return_code <- as.factor(data$return_code)

#tplot <- ggplot(time_df, aes(x = pct_completed, y = seconds, linetype = configuration)) + 
#  labs(x = "% Benchmarks Synthesized <= Y Total Seconds", y = "# of Seconds", title = "Total Benchmark Synthesis Times") +
#  scale_linetype_discrete(name = "Analysis", breaks = c("NAll","CC10"), limits = c("NAll","CC10")) +
#  geom_line() +
#  scale_y_continuous(limits=c(0, 0.3)) +
#  scale_x_continuous(limits=c(0, 1)) +
#  theme_flashrelate
#ggsave(tplot, filename=paste(OUTPUT_DIR, 'synthesis_time.pdf', sep=""), width=6.5, height=5)
