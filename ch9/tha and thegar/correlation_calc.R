#Looking at þegar vs. þá
#Processing and GLMs

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

data1 <- process_summary("summarytha.txt")
data2 <- process_summary("summarythegar.txt")

# Add a column to each data frame representing the linguistic form (0 or 1)
data1$form <- 0
data2$form <- 1


data <- rbind(data1, data2)

combined_hits <- merge(data1[, c("source", "hits")], data2[, c("source", "hits")], by="source", suffixes=c("_data1", "_data2"))


combined_hits$total_hits <- combined_hits$hits_data1 + combined_hits$hits_data2

combined_hits$proportion_data1 <- combined_hits$hits_data1 / combined_hits$total_hits
combined_hits$proportion_data2 <- combined_hits$hits_data2 / combined_hits$total_hits

data1_complete <- merge(data1, combined_hits, by="source")
data2_complete <- merge(data2, combined_hits, by="source")


combined_data <- unique(rbind(data1_complete, data2_complete), by = c("source", "hits", "tokens", "total", "year", "textid", "genre", "form", "total_hits", "proportion_data1", "proportion_data2"))
combined_data <- combined_data[complete.cases(combined_data),]


long_data <- data.frame(source = character(),
                        year = integer(),
                        genre = character(),
                        form = character(),
                        hits = integer(),
                        stringsAsFactors = FALSE)

for (i in seq_len(nrow(combined_data))) {
  hits1 <- ifelse(is.na(combined_data$hits_data1[i]), 0, combined_data$hits_data1[i])
  hits2 <- ifelse(is.na(combined_data$hits_data2[i]), 0, combined_data$hits_data2[i])
  
  if (nrow(long_data) == 0) {
    long_data <- data.frame(source = combined_data$source[i],
                            year = combined_data$year[i],
                            genre = combined_data$genre[i],
                            form = c(rep("hits1", times = hits1), rep("hits2", times = hits2)),
                            hits = c(rep(0, times = hits1), rep(1, times = hits2)),
                            stringsAsFactors = FALSE)
  } else {
    if (hits1 > 0) {
      long_data <- rbind(long_data, data.frame(source = combined_data$source[i],
                                               year = combined_data$year[i],
                                               genre = combined_data$genre[i],
                                               form = rep("hits1", times = hits1),
                                               hits = rep(0, times = hits1),
                                               stringsAsFactors = FALSE))
    }
    
    if (hits2 > 0) {
      long_data <- rbind(long_data, data.frame(source = combined_data$source[i],
                                               year = combined_data$year[i],
                                               genre = combined_data$genre[i],
                                               form = rep("hits2", times = hits2),
                                               hits = rep(1, times = hits2),
                                               stringsAsFactors = FALSE))
    }
  }
}




library(lme4)
model_1 <- glmer(hits ~ year + (1 | source), data = long_data, family = binomial)
model_2 <- glmer(hits ~ year + simplified_genre + (1 | source), data = long_data, family = binomial)
model_3 <- glmer(hits ~ year*simplified_genre + (1 | source), data = long_data, family = binomial)
model_4 <- glm(hits ~ year + simplified_genre, data = long_data, family = binomial)

summary(model_1)
summary(model_2)
summary(model_3)


summary(model_4)

model_comparison_aic <- model.sel(model_3, model_2, rank.args = list(AIC = list()))

intercept <- -15.78
year_coef <- 0.008267
law_coef <- -9.288
nar_coef <- 1.806
rel_coef <- 1.486
sci_coef <- 3.357

arbitrary_year <- 1750

ref_genre <- 0



Xb_initial <- intercept + (year_coef * arbitrary_year) + nar_coef
Xb_one_year_later <- intercept + (year_coef * (arbitrary_year + 1)) + nar_coef

prob_initial <- 1 / (1 + exp(-Xb_initial))
prob_one_year_later <- 1 / (1 + exp(-Xb_one_year_later))

prob_initial

peryear <- 0.001804304
peryear*100

law_prob <- 1 / (1 + exp(-(ref_genre + law_coef)))
nar_prob <- 1 / (1 + exp(-(ref_genre + nar_coef)))
rel_prob <- 1 / (1 + exp(-(ref_genre + rel_coef)))
sci_prob <- 1 / (1 + exp(-(ref_genre + sci_coef)))

# Print probabilities
cat("Law:", law_prob, "\n")
cat("Narrative:", nar_prob, "\n")
cat("Religion:", rel_prob, "\n")
cat("Science:", sci_prob, "\n")
print(model_comparison_aic)
anova(model_2, model_3)

library(ggplot2)


library(rsq)

library(MuMIn)

model_3 <- glm(hits ~ year * simplified_genre, family = binomial, data = long_data)
model_2 <- glm(hits ~ year + simplified_genre, family = binomial, data = long_data)
model_4 <- glmer(hits ~ year + (1 | simplified_genre) + (1 | source), family = binomial, data = long_data)

r.squaredGLMM(model_3)
r.squaredGLMM(model_2)
r.squaredGLMM(model_4)

rsq(model_3)
rsq(model_2)

long_data$simplified_genre <- gsub("-(.*)", "", long_data$genre)
combined_data$simplified_genre <- gsub("-(.*)", "", combined_data$genre)

# Fit the new model using 'simplified_genre' instead of 'genre'
glm_model_simplified <- glm(hits ~ year * simplified_genre, family = binomial, data = long_data)
summary(glm_model_simplified)


# Create predicted values using the model
long_data$pred <- predict(glm_model_simplified, type = "response")

# Create the plot
ggplot(long_data, aes(x = year, y = hits)) +
  geom_point() +
  geom_line(aes(y = pred), color = "blue") +
  labs(x = "Year", y = "Probability of Hit")

ggplot(long_data, aes(x = year, y = hits)) +
  geom_point() +
  geom_line(aes(y = pred), color = "blue") +
  labs(x = "Year", y = "Probability of Hit")

ggplot(long_data, aes(x = year, y = hits)) +
  geom_point(aes(color = simplified_genre), alpha = 0.5) +
  geom_line(aes(y = pred, color = simplified_genre, group = simplified_genre)) +
  facet_wrap(~simplified_genre, ncol = 4) +
  labs(title = "Hits by Genre and Year",
       x = "Year",
       y = "Hits",
       color = "Genre",
       size = "Hits") +
  theme_minimal() +
  theme(legend.position = "bottom")

library(ggplot2)

# Create the plot
ggplot(long_data, aes(x = year, y = hits, color = simplified_genre)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
  labs(title = "Role of Interaction Term in GLM Model",
       x = "Year",
       y = "Hits") +
  theme_minimal() +
  scale_color_discrete(name = "Simplified Genre")



library(dplyr)
filtered_data <- combined_data %>%
  filter(simplified_genre %in% c("nar", "rel"))

glm_model <- glm(proportion_data1 ~ year + genre, data = filtered_data, family = quasibinomial)


ggplot(long_data, aes(x = year, y = hits)) +
  geom_point() +
  geom_line(aes(y = pred), color = "blue") +
  labs(x = "Year", y = "Probability of Hit")

ggplot(long_data, aes(x = year, y = hits)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +
  labs(x = "Year", y = "Probability of Hit")

ggplot(long_data, aes(x = year, y = hits)) +
  geom_point(aes(color = simplified_genre), alpha = 0.5, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue", linetype = "solid", size = 1.2) +
  labs(title = "Proportion of þegar vs þá as temporal conjunction",
       x = "Year",
       y = "Probability of Hit",
       color = "Linguistic Form") +
  scale_color_manual(values = c("darkorange", "steelblue")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom")
