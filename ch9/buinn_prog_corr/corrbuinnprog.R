library(dplyr)

# Merge the datasets by textid
merged_data2 <- buinn_all %>%
  left_join(merged_data, by = "textid")

# Drop any rows with missing values in relative_frequency or rescaled_predictions columns
merged_data2 <- merged_data2 %>%
  drop_na(relative_frequency.x, relative_frequency.y)

# Group the data by textid and calculate the correlation between relative_frequency and rescaled_predictions
cor_data <- merged_data2 %>%
  group_by(textid) %>%
  summarise(correlation = cor.test(relative_frequency.x, relative_frequency.y)$estimate)

# Plot the correlation values by textid
ggplot(cor_data, aes(x = textid, y = correlation)) +
  geom_point() +
  labs(title = "Correlation of Relative Frequency between Two Datasets",
       x = "textid",
       y = "Correlation") +
  theme_minimal()

cor_data <- merged_data2 %>%
  group_by(textid) %>%
  filter(!is.na(relative_frequency.x) & !is.na(relative_frequency.y)) %>%
  summarise(correlation = cor(relative_frequency.x, relative_frequency.y, use = "pairwise.complete.obs"))

cor_data


cor_data <- merged_data2 %>%
  group_by(textid) %>%
  filter(!is.na(relative_frequency.x) & !is.na(relative_frequency.y)) %>%
  summarise(correlation = cor(relative_frequency.x, relative_frequency.y, method = "spearman", use = "pairwise.complete.obs"))

cor_data


merged_datasets <- merge(subset_buinn_all, subset_merged_data, by = "textid", suffixes = c("_buinn", "_merged"))
merged_datasets$relative_frequency_buinn[is.na(merged_datasets$relative_frequency_buinn)] <- 0
merged_datasets$relative_frequency_merged[is.na(merged_datasets$relative_frequency_merged)] <- 0
correlation_bp <- cor(merged_datasets$relative_frequency_buinn, merged_datasets$relative_frequency_merged, method = "pearson")
correlation_testbp <- cor.test(merged_datasets$relative_frequency_buinn, merged_datasets$relative_frequency_merged, method = "pearson")
print(paste("Correlation coefficient:", correlation_bp))
print(correlation_testbp)

ggplot(data = merged_datasets, aes(x = relative_frequency_buinn, y = relative_frequency_merged)) +
  geom_point(aes(color = simplified_genre), size = 3) +
  scale_color_viridis() +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid") +
  labs(title = "Scatter plot of relative frequencies",
       x = "Relative Frequency (subset_buinn_all)",
       y = "Relative Frequency (subset_merged_data)",
       color = "Correlation") +
  theme_minimal()

ggplot(data = merged_datasets, aes(x = relative_frequency_buinn, y = relative_frequency_merged)) +
  geom_point(aes(color = simplified_genre), size = 3) +
  scale_color_viridis_d() +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Scatter plot of relative frequencies",
       x = "Relative Frequency (subset_buinn_all)",
       y = "Relative Frequency (subset_merged_data)",
       color = "Simplified Genre") +
  theme_minimal()


filtered_data2 <- merged_datasets[merged_datasets$simplified_genre != "nar", ]
filtered_correlation <- cor(filtered_data2$relative_frequency_buinn, filtered_data2$relative_frequency_merged, method = "pearson")
filtered_correlation_test <- cor.test(filtered_data2$relative_frequency_buinn, filtered_data2$relative_frequency_merged, method = "pearson")

print(paste("Filtered Correlation coefficient:", filtered_correlation))
print(filtered_correlation_test)

ggplot(data = merged_datasets, aes(x = relative_frequency_buinn, y = relative_frequency_merged)) +
  geom_point(aes(color = simplified_genre), size = 3) +
  geom_text_repel(aes(label = label, color = simplified_genre), size = 3.5, box.padding = 0.35, point.padding = 0.5, segment.size = 0.2,
                  max.overlaps = 5, force = 2, max.iter = 1000, nudge_x = 0.2, nudge_y = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid") +
  scale_color_viridis_d(name = "Simplified Genre") +
  labs(title = "Correlation plot: BÚINN and PROG",
       x = "Relative Frequency BÚINN",
       y = "Relative Frequency PROG",
       color = "Genre") +
  theme_minimal() +
  theme(legend.position = "bottom")


