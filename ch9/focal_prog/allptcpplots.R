allptcp <- read.csv(file = "icepahc_allptcp.csv", sep = ",", header = TRUE)


df_joined <- allptcp %>%
  left_join(buinn_all[, c("textid", "total")], by = "textid")

df_long <- df_joined %>%
  gather(key = "variable", value = "value", prog, fara, taka, na.rm = TRUE) %>%
  group_by(textid, year, variable) %>%
  summarize(count = sum(value)) %>%
  ungroup()

df_long <- df_long %>%
  left_join(buinn_all[, c("textid", "total")], by = "textid") %>%
  mutate(relative_frequency = count / total * 100)

ggplot(df_long, aes(x = year, y = relative_frequency, group = variable, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1", name = "Variable") +
  labs(title = "Relative frequency over time",
       x = "Year",
       y = "Relative Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 16),
        legend.position = "bottom")

ggplot(df_long, aes(x = year, y = relative_frequency, group = variable, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1", name = "Variable") +
  labs(title = "Relative frequency over time",
       x = "Year",
       y = "Relative Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 16),
        legend.position = "none") + # Hide the legend because of facet
  facet_wrap(~ variable, scales = "free_y") # Apply facet wrap

df_prog <- allptcp %>%
  filter(prog == 1, century < 20, !is.na(FOCAL)) 

df_prog_count <- df_prog %>%
  group_by(textid, year, FOCAL) %>%
  summarise(count = n(), .groups = 'drop')

df_prog_joined <- df_prog_count %>%
  left_join(buinn_all[, c("textid", "total")], by = "textid")

df_prog_final <- df_prog_joined %>%
  group_by(year, FOCAL) %>%
  mutate(relative_frequency = (count / total) * 1000)

ggplot(df_prog_final, aes(x = year, y = relative_frequency, group = FOCAL, color = as.factor(FOCAL))) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1", name = "FOCAL") +
  labs(title = "Focalised vs durative",
       x = "Year",
       y = "Relative Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 16),
        legend.position = "bottom") +
  facet_wrap(~ FOCAL, scales = "free_y", ncol = 1)


df_prog2 <- allptcp %>%
  filter(prog == 1) %>%
  left_join(buinn_all[, c("textid", "total")], by = "textid") %>%
  group_by(century, FOCAL) %>%
  summarise(count = n(), .groups = 'drop') %>%
  spread(FOCAL, count, fill = 0) %>%  
  mutate(total = `0` + `1`,           
         prop_0 = `0` / total,        
         prop_1 = `1` / total)       

# Now create a simple bar plot for proportions by century
ggplot(df_prog2, aes(x = as.factor(century))) +
  geom_bar(aes(y = prop_0, fill = "0"), stat = "identity") +
  geom_bar(aes(y = prop_1, fill = "1"), stat = "identity") +
  scale_fill_brewer(palette = "Set1", name = "FOCAL") +
  labs(title = "Proportions of FOCAL 0 and 1 by Century",
       x = "Century",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))


df_prog3 <- allptcp %>%
  filter(prog == 1) %>%
  count(century, FOCAL) %>%
  group_by(century) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(prop = n / total) %>%
  filter(century < 20) 

# Create the plot
ggplot(df_prog3, aes(fill = as.factor(FOCAL), y = prop, x = as.factor(century))) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Century", y = "Proportion", fill = "FOCAL") +
  theme_minimal() +
  scale_x_discrete(drop = FALSE) +
  theme_minimal() 
