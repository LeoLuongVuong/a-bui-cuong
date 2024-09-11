
# load libraries ----
library(tidyverse)
library(here)

# from the bar plot ----
## data manipulation ----
datDir <- here("plot-nov-09")

# Read the dataset
data <- read.csv(file.path(datDir, 'dpmas_dataset.csv'), stringsAsFactors = FALSE)

# Separate the mean and SD values from the "Before" and "After" columns
extract_mean_sd <- function(x) {
  mean_sd <- strsplit(x, " ± ")[[1]]
  return(data.frame(Mean = as.numeric(mean_sd[1]), SD = as.numeric(mean_sd[2])))
}

# Apply the extraction function to both columns
before_data <- t(apply(as.matrix(data$Before.DPMAS..Mean...SD.), 1, extract_mean_sd))
after_data <- t(apply(as.matrix(data$After.DPMAS..Mean...SD.), 1, extract_mean_sd))

# Convert to data frame
before_df <- do.call(rbind, before_data)
after_df <- do.call(rbind, after_data)

# Combine with the measurement types
long_data <- data.frame(
  Measurement = rep(data$Measurement, each = 2),
  Time = rep(c("Before DPMAS", "After DPMAS"), times = nrow(data)),
  Mean = c(before_df[,1], after_df[,1]),
  SD = c(before_df[,2], after_df[,2])
)

# extract the values of Measurement column in odd rows in long_data
long_data$Measurement <- long_data$Measurement[seq(1, nrow(long_data), by = 2)]

# extract the values of Time column in the first two rows, each repeat 5 times
long_data$Time <- rep(c("Before DPMAS", "After DPMAS"), each = 5)

## plotting ----

# re-level Time in long_data, "Before" then "After"
long_data$Time <- as.factor(long_data$Time)
long_data$Time <- fct_relevel(long_data$Time, "Before DPMAS", "After DPMAS")

# Generate the plot

ggplot(long_data, aes(x = Measurement, y = Mean, fill = Time)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = Mean, ymax = Mean + SD), 
                width = 0.2, position = position_dodge(0.9),
                size = 0.8) +
  labs(title = "Comparison of Measurements Before and After DPMAS",
       y = "Mean + SD", x = "Measurements") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = seq(0, 1600, by = 100))
ggsave(file.path(datDir, "dpmas_plot.png"), width = 9, height = 9)

# from the table ----

# Create the dataset
data <- data.frame(
  Variable = rep(c("PT (%)", "INR", "APTT (s)", "Fibrinogen (g/l)"), each = 2),
  Time = rep(c("Before DPMAS", "After DPMAS"), times = 4),
  Mean = c(48.4, 28.6, 1.90, 3.2, 52.2, 88.8, 2.4, 1.7),
  SD = c(19.07, 15.42, 1.36, 2.04, 33.46, 40.88, 1.56, 1.06),
  P_value = rep("<0.001", 8)
)

# Add n to the dataset
n_value <- 51

# re-level Time in data, "Before" then "After"
data$Time <- as.factor(data$Time)
data$Time <- fct_relevel(data$Time, "Before DPMAS", "After DPMAS")

# Generate the ggplot
ggplot(data, aes(x = Variable, y = Mean, fill = Time)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = Mean, ymax = Mean + SD), 
                width = 0.2, position = position_dodge(width = 0.9),
                size = 0.8) +
  labs(title = "Comparison of Measurements Before and After DPMAS", 
       subtitle = paste("n =", n_value, "| P values < 0.001"),
       y = "Mean + SD", x = "Measurements") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = seq(0, 160, by = 4))
ggsave(file.path(datDir, "dpmas_table_plot.png"), width = 9, height = 9)


