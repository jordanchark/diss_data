process_summary <- function(file) {
  text_lines <- suppressWarnings(readLines(file))
  text_lines <- text_lines[-c(1, length(text_lines))]
  
  process_line <- function(line) {
    matches <- regmatches(line, regexec("(\\S+)\\s+(\\d+)\\s*/\\s*(\\d+)\\s*/\\s*(\\d+)", line))[[1]]
    return(matches[-1])
  }
  
  data_list <- lapply(text_lines, process_line)
  data <- do.call(rbind, data_list)
  data <- data.frame(data, stringsAsFactors = FALSE)
  # Apply unique() function to the 'data' data frame instead of 'data_list'
  data <- unique(data, by = "source")
  
  colnames(data) <- c("source", "hits", "tokens", "total")
  data$hits <- as.numeric(data$hits)
  data$tokens <- as.numeric(data$tokens)
  data$total <- as.numeric(data$total)
  
  # Extracting year, textid, and genre information
  extract_info <- function(source) {
    if (startsWith(source, "ntmatthew")) {
      textid_genre <- strsplit(source, ".", fixed = TRUE)[[1]]
      textid <- textid_genre[1]
      genre <- textid_genre[2]
      return(c("1540", textid, genre))
    }
    
    year <- substr(source, 1, 4)
    textid_genre <- strsplit(substr(source, 6, nchar(source)), ".", fixed = TRUE)[[1]]
    textid <- textid_genre[1]
    genre <- textid_genre[2]
    
    return(c(year, textid, genre))
  }
  
  extra_data <- do.call(rbind, lapply(data$source, extract_info))
  data <- cbind(data, as.data.frame(extra_data, stringsAsFactors = FALSE))
  colnames(data)[5:7] <- c("year", "textid", "genre")
  data$year <- as.numeric(data$year)
  
  return(data)
}


library(tidyverse)
andi_all <- process_summary("pplandi.txt")

andi_all <- distinct(andi_all, source, .keep_all = TRUE)
andi_all$simplified_genre <- gsub("-(.*)", "", andi_all$genre)
andi_all <- andi_all %>%
  group_by(textid) %>%
  mutate(relative_frequency = hits / sum(total) * 100)

library(viridis)
library(ggrepel)
ggplot(andi_all, aes(x = year, y = relative_frequency, color = simplified_genre)) +
  geom_point(size = 3) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  scale_color_viridis(discrete = TRUE, name = "Simplified Genre", option = "C") +
  labs(title = "-andi (IcePAHC)",
       x = "Year",
       y = "Relative Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 16),
        legend.position = "bottom")

warnings()

ggplot(andi_all[andi_all2$year >= 1350, ], aes(x = year, y = relative_frequency)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm", se = FALSE, linetype = "dashed") +
  scale_color_viridis_d(name = "Simplified Genre", option = "C") +
  labs(title = "Pres. part. over time in IcePAHC",
       x = "Year",
       y = "Relative Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 16),
        legend.position = "bottom")

andi_all2 <- andi_all  %>% 
  filter(simplified_genre !="psd")

ggplot(andi_all[andi_all$year >= 1300, ], aes(x = year, y = relative_frequency, color = simplified_genre, size = hits)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  scale_color_viridis_d(name = "Simplified Genre", option = "C") +
  scale_size(range = c(1, 8)) +
  labs(title = "Pres. part. over time in IcePAHC",
       x = "Year",
       y = "Relative Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 16),
        legend.position = "right") +
  guides(color = guide_legend(override.aes = list(alpha = 1)),
         size = guide_legend(override.aes = list(alpha = 1)))


ggplot(andi_all, aes(x = year, y = relative_frequency, color = simplified_genre, size = hits)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  scale_color_manual(name = "Simplified Genre",
                     values = c("nar" = "#440154FF", "other" = "#21908CFF"),
                     guide = guide_legend(override.aes = list(size = 5))) +
  labs(title = "Búinn over time in IcePAHC",
       x = "Year",
       y = "Relative Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 16),
        legend.position = "bottom")


ggplot(andi_all, aes(x = year, y = relative_frequency, color = simplified_genre)) +
  geom_point(size = 3) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  geom_text_repel(aes(label = textid),
                  size = 3,
                  max.overlaps = 10,
                  box.padding = 0.5) +
  scale_color_viridis(discrete = TRUE, name = "Genre", option = "C") +
  labs(title = "-andi over time",
       x = "Year",
       y = "Relative Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 16),
        legend.position = "bottom")

andi_all <- andi_all %>%
  mutate(genre = ifelse(genre == "andi_all", "rel-bib", genre),
         simplified_genre = ifelse(simplified_genre == "andi_all", "rel", simplified_genre))

ggplot(andi_all, aes(x = year, y = relative_frequency, color = simplified_genre)) +
  geom_point(aes(size = hits)) +
  scale_size_continuous(range = c(3, 10)) +  # Adjust the size range as needed
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  geom_text_repel(aes(label = textid),
                  size = 3,                  box.padding = 0.5) +
  scale_color_viridis(discrete = TRUE, name = "Genre", option = "C") +
  labs(title = "Participle -andi (IcePAHC)",
       x = "Year",
       y = "Relative Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 16),
        legend.position = "right")



# Load required packages
install.packages("MASS")
library(MASS)

andi_all$integer <- andi_all$hits*round(andi_all$relative_frequency*10)

# Fit the Poisson regression model
poisson_model <- glm(integer ~ year + simplified_genre, data = andi_all, family = poisson())

# Fit the negative binomial regression model
negbin_model <- glm.nb(integer ~ year + simplified_genre, data = andi_all)

warnings()

predictions_df <- expand.grid(year = unique(andi_all$year),
                              simplified_genre = unique(andi_all$simplified_genre))


predictions_df$poisson_pred <- predict(poisson_model, newdata = predictions_df, type = "response")

# Add the Poisson regression lines
andi_all_plot <- ggplot(andi_all, aes(x = year, y = integer, color = simplified_genre, size = hits)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_d() + 
  scale_size(range = c(1, 10)) +
  geom_text_repel(aes(label = textid),
                  size = 3,
                  max.overlaps = 10,
                  box.padding = 0.5) +
  labs(title = "Participle -andi in IcePAHC",
       x = "Year",
       y = "Rel. freq",
       subtitle = "") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_line(data = predictions_df, aes(x = year, y = poisson_pred, group = simplified_genre), linetype = "dashed")

# Add the negative binomial regression lines (optional, comment out if not needed)
andi_all_plot <- andi_all_plot + 
  geom_line(data = predictions_df, aes(x = year, y = negbin_pred, group = simplified_genre), linetype = "solid")

# Print the plot
print(andi_all_plot)



ggplot(andi_all2, aes(x = year, y = relative_frequency, color = simplified_genre, size = hits)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_d() + 
  scale_size(range = c(1, 10)) +
  geom_text_repel(aes(label = textid),
                  size = 3,
                  max.overlaps = 10,
                  box.padding = 0.5) +
  labs(title = "Participle -andi in IcePAHC",
       x = "Year",
       y = "Rel. freq",
       subtitle = "") +
  theme_minimal() +
  theme(legend.position = "right")



ggplot(andi_all2, aes(x = year, y = relative_frequency, color = genre, size = hits)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_d() + 
  scale_size(range = c(1, 10)) +
  geom_text_repel(aes(label = textid),
                  size = 3,
                  max.overlaps = 10,
                  box.padding = 0.5) +
  labs(title = "Participle -andi in IcePAHC",
       x = "Year",
       y = "Rel. freq",
       subtitle = "") +
  theme_minimal() +
  theme(legend.position = "right")


# Add the Poisson regression lines
andi_all_plot <- ggplot(andi_all, aes(x = year, y = relative_frequency, color = simplified_genre, size = hits)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_d() + 
  scale_size(range = c(1, 10)) +
  geom_text_repel(aes(label = textid),
                  size = 3,
                  max.overlaps = 10,
                  box.padding = 0.5) +
  labs(title = "Participle -andi in IcePAHC",
       x = "Year",
       y = "Rel. freq",
       subtitle = "") +
  theme_minimal() +
  theme(legend.position = "right")
# Print the plot
print(andi_all_plot)

