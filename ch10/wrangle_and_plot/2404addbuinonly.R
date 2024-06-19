buinadd <- read_csv("buin_toimport_addition.csv",  col_types = cols(.default = col_character()))

fulldescr_imp <- read_csv("fullfordescr.csv",  col_types = cols(.default = col_character()))

buin_added_df <- bind_rows(fulldescr_imp , buinadd)

buin_added_df_filter <- buin_added_df %>%
  filter(BuinnForm == "buin")

revised_full <- buin_added_df

# Adjust breakpoints for 25-year periods, including the end of the last period
extended_breakpoints <- c(1785, 1810, 1835, 1860, 1885, 1910, 1935)

revised_full$PERIOD <- as.numeric(revised_full$PERIOD)
revised_full$year<- as.numeric(revised_full$year)

# Extend labels for each period
extended_period_labels <- c("1785-1809", "1810-1834", "1835-1859", "1860-1884", "1885-1909", "1910-1934")

# Recategorize the PERIOD variable
revised_full$PERIOD <- cut(revised_full$year, breaks=extended_breakpoints, labels=extended_period_labels, include.lowest=TRUE, right=FALSE)

contrasts(revised_full$PERIOD) <- contr.sdif(6) 
revised_full$PERIOD <- as.factor(revised_full$PERIOD)

indicative_rev <- subset(revised_full, Moodc == "Indicative")

indicative_revised <- indicative_rev %>%
  mutate(Excl = replace_na(Excl, 0)) %>%
  filter(Excl != 1) %>%
  filter(GENDER != "?") 

indicative_rev <- indicative_revised %>%
  mutate(Tensec = ifelse(Tensec == "Plural", "Present", Tensec))

indicative_rev_filter <- indicative_rev %>%
  filter(NAME == "Geir Vídalín biskup") %>%
  filter(is.na(PERIOD))

indicative_rev$DECADE <- as.numeric(indicative_rev$DECADE)
indicative_rev$year <- as.numeric(indicative_rev$year)

indicative_rev <- indicative_rev %>%
  mutate(DECADE = floor(year / 10) * 10)

summary_dataverbs <- indicative_rev %>%
  group_by(DECADE, Tensec, source, Verb) %>%
  filter(!is.na(Verb)) %>%
  summarise(Count = n(), .groups = 'drop') %>%  # Ensure that the grouping is dropped after summarising
  group_by(DECADE, Tensec, source) %>%          # Regroup by DECADE, Tensec, and source to sum counts correctly
  mutate(Proportion = Count / sum(Count)) %>%
  ungroup() %>%
  filter(!is.na(DECADE), DECADE < 1930)


# For random effects, particularly the slopes for DECADE across verbs
plot_model(model, type = "re", terms = "Verb")
plot_model(model, type = "re", group.terms = "Verb", title = "Random Effects for Top 20 Verbs")
#Now that we are loaded in - LINE PLOTS

summary_data1 <- indicative_rev %>%
  group_by(DECADE, Tensec, source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  filter(!is.na(DECADE), DECADE < 1930)

#THIS IS A STACKED BAR PLOT, by decade and tense

indicative_pres <- subset(indicative_rev, Tensec == "Present")
indicative_past <- subset(indicative_rev, Tensec == "Past")

ggplot(data = summary_data1, aes(x = DECADE, y = Proportion, fill = source)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Proportion of búinn by Decade and Tense (Indicative)",
       x = "Decade",
       y = "Proportion") +
  scale_fill_manual(values = custom_colors) +
  facet_grid(~ Tensec) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1))

#as a prettier line plot with colorbrewer

summary_data1b <- indicative_rev %>%
  group_by(DECADE, GENDER, Tensec, source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

summary_data1c <- indicative_pres %>%
  # Group by the combination of interest
  group_by(DECADE, GENDER, RANKNR, source) %>%
  # Calculate count for each group
  summarise(Count = n(), .groups = "drop") %>%
  # Calculate the total for each DECADE, GENDER, RANKNR combination
  group_by(DECADE, GENDER, RANKNR) %>%
  mutate(Total = sum(Count)) %>%
  # Calculate proportion
  mutate(Proportion = Count / Total) %>%
  # Filter based on Total
  filter(Total > 10)

summary_data_filtered <- summary_data1 %>% 
  filter(source == "buinn")

summary_data_filtered_genderrank <- summary_data1c %>% 
  filter(source == "buinn")

summary_data_filtered_gender <- summary_data1b %>% 
  filter(source == "buinn")

filter_pres <- indicative_pres %>% 
  filter(is.na(DECADE))

# line plot
ggplot(data = summary_data_filtered, aes(x = DECADE, y = Proportion, color = source)) +
  geom_line() +
  geom_point() +
  labs(title = "Proportion of búinn (by Tense and Decade, Indicative)",
       x = "Decade",
       y = "Proportion") +
  scale_color_brewer(palette = 'Set1')  +
  facet_grid(~ Tensec) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1))

ggplot(data = summary_data_filtered_gender, aes(x = DECADE, y = Proportion, color = source)) +
  geom_line() +
  geom_point() +
  labs(title = "Proportion of búinn (by Tense, Decade and Gender, Indicative)",
       x = "Decade",
       y = "Proportion") +
  scale_color_brewer(palette = 'Set1')  +
  facet_grid(GENDER ~ Tensec) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1))

ggplot(data = summary_data1, aes(x = DECADE, y = Proportion, color = source, group = source)) +
  geom_line() + # Use geom_line for line plots
  geom_point() + # Adding points on the line plot can help visualize individual data points
  labs(title = "Proportion of búinn (by Tense and Decade, Indicative)",
       x = "Decade",
       y = "Proportion") +
  scale_color_brewer(palette = 'Set1') + # Use scale_color_manual for line colors
  facet_grid(~ Tensec) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1))

summary_data2 <- indicative_pres %>%
  group_by(DECADE, GENDER, RANKNR, source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  filter(!is.na(DECADE), DECADE < 1940)

# Create a stacked bar plot by period25, faceted by GENDER and RANK
ggplot(data = summary_data2, aes(x = DECADE, y = Proportion, fill = source)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Proportion of búinn by Decade, Gender and Rank (Indicative Present)",
       x = "Period",
       y = "Proportion") +
  scale_color_brewer(palette = 'Set1') +
  facet_grid(GENDER ~ RANKNR) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1))

summary2table <- xtable(summary_data2)
print(summary2table, include.rownames=FALSE)

tensegendertable <- xtable(summary_data1b)
print(tensegendertable, include.rownames=FALSE)


summary_datapres_agg <- indicative_pres %>%
  group_by(year,source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

summary_datapast_agg <- indicative_past %>%
  group_by(year,source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

buinn_datapres_agg <- summary_datapres_agg %>%
  filter(source == "buinn")

buinn_datapast_agg <- summary_datapast_agg %>%
  filter(source == "buinn")

p = ggplot(aes(x = year, y = Proportion), data = buinn_datapres_agg) +
  geom_point(aes(size = Count), alpha = 0.5, position = "identity") +
  geom_smooth(method = "loess", se = FALSE, size = 4) +
  scale_x_continuous(name = "Year", limits = c(1780, 1930)) +
  scale_y_continuous(name = "Proportion of forms", breaks = seq(0, 1, .25)) +
  theme(legend.position = "none") +
  scale_size_area(name = "Count", max_size = 20) +
  coord_cartesian(xlim = c(1780, 1930), ylim = c(0, 1))

p

ppast = ggplot(aes(x = year, y = Proportion), data = buinn_datapast_agg) +
  geom_point(aes(size = Count), alpha = 0.5, position = "identity") +
  geom_smooth(method = "loess", se = FALSE, size = 4) +
  scale_x_continuous(name = "Year", limits = c(1780, 1930)) +
  scale_y_continuous(name = "Proportion of forms", breaks = seq(0, 1, .25)) +
  theme(legend.position = "none") +
  scale_size_area(name = "Count", max_size = 20) +
  coord_cartesian(xlim = c(1780, 1930), ylim = c(0, 1))

ppast

summary_individualsbyperiod <- indicative_pres %>%
  group_by(PERIOD, NAME, RANKNR, source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  mutate(Total = sum(Count)) %>%
  filter(Total > 20)

ind_table_period <- xtable(summary_individualsbyperiod)
print(ind_table_period, include.rownames=FALSE)

#Look at individuals
byindsum <- indicative_pres %>%
  group_by(DECADE, NAME, RANKNR, GENDER, source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  mutate(Total = sum(Count)) 

byindsum_table <- xtable(byindsum)
print(byindsum_table , include.rownames=FALSE)



individuals_with_enough_data <- indicative_pres %>%
  group_by(NAME) %>%
  filter(length(unique(AGE)) > 10) %>%
  ungroup() %>%
  distinct(NAME)

indicative_pres_newgen <- indicative_pres %>%
  mutate(GENERATIONEX = case_when(
    BIRTHYEAR < 1750 ~ "<1750",
    BIRTHYEAR >= 1750 & BIRTHYEAR < 1775 ~ "1750-1774",
    BIRTHYEAR >= 1775 & BIRTHYEAR < 1800 ~ "1775-1799",
    BIRTHYEAR >= 1800 & BIRTHYEAR < 1825 ~ "1800-1824",
    BIRTHYEAR >= 1825 & BIRTHYEAR < 1850 ~ "1825-1849",
    BIRTHYEAR >= 1850 & BIRTHYEAR < 1875 ~ "1850-1874",
    BIRTHYEAR >= 1875 & BIRTHYEAR < 1950 ~ ">1875",
    TRUE ~ NA_character_)
  )

result3 <- indicative_pres_newgen %>%
  group_by(GENERATIONEX, DECADE, source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))


result3_bu <- result3 %>%
  filter(source=="buinn") %>%
  filter(Count > 10)

ggplot(result3_bu, aes(x = DECADE, y = Proportion, group = GENERATIONEX, color = GENERATIONEX)) +
  geom_line() +
  geom_point(size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Proportion by Generation and Decade",
       x = "Period",
       y = "Proportion",
       color = "Generation") +
  scale_color_viridis_d(begin = 0.3, end = 0.9) # Use viridis for a pleasing color scale


ggplot(result3_bu, aes(x = DECADE, y = Proportion, group = GENERATIONEX, color = GENERATIONEX)) +
  geom_line() +
  geom_point(size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Proportion by Generation and Decade (Indicative Present)",
       x = "Period",
       y = "Proportion",
       color = "Generation") +
  scale_color_viridis_d(begin = 0.3, end = 0.9) +
  scale_y_continuous(limits = c(min(result3_bu$Proportion, na.rm = TRUE), max(result3_bu$Proportion, na.rm = TRUE)))


ggplot(result3_bu, aes(x = DECADE, y = GENERATION, fill = Proportion)) +
  geom_tile(color = "white") + # Use tiles, with white borders for better visibility
  scale_fill_viridis_c() + # Use a continuous color scale from viridis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Apparent-time Analysis",
       x = "Time of Writing",
       y = "Generation",
       fill = "Proportion")

result5 <- indicative_pres_newgen %>%
  group_by(GENERATIONEX, NAME, source) %>%
  summarise(Count = n()) %>%
  mutate(Total= sum(Count)) %>%  
  ungroup() %>%
  mutate(Proportion = Count / Total) %>%
  filter(Total != 0) 

result5_bu <- result5 %>%
  filter(Total > 10) %>%
  filter(source=="buinn") 

result52 <- result5 %>%
  filter(Total > 49)

result5_bu2 <- result5_bu %>%
  arrange(GENERATIONEX, NAME) %>%
  group_by(GENERATIONEX) %>%
  mutate(Index = 1:n())

breaks_x <- seq(min(result5_bu2$Index), max(result5_bu2$Index), by = 1)  # Adjust 'by' as needed

# Create the plot
plot <- ggplot(result5_bu2, aes(x = Index, y = Proportion)) +
  geom_point() +  # Add points
  geom_line(aes(group = GENERATIONEX)) +  # Connect points with lines
  facet_wrap(~GENERATIONEX, ncol = 1) +  # Facets for each GENERATION
  scale_x_continuous(breaks = breaks_x) +  # Custom x-axis labels
  theme_minimal() +
  labs(x = "Individual within Generation (Indicative Present)", y = "Proportion") +  # Labels
  theme(
    strip.background = element_blank(),  # Remove facet label background
    strip.text.x = element_text(size = 10),  # Adjust facet label text size
    axis.text.x = element_text(),  # Horizontal x-axis labels
    axis.ticks.x = element_line()  # Show x-axis ticks
  )
plot


plot <- ggplot(result5_bu2, aes(x = Index, y = Proportion)) +
  geom_point() +  # Add points
  geom_line(aes(group = GENERATIONEX)) +  # Connect points with lines
  facet_wrap(~GENERATIONEX, ncol = 1) +  # Facets for each GENERATION
  scale_x_continuous(breaks = breaks_x) +  # Custom x-axis labels
  theme_minimal() +
  labs(x = "Individual within Generation", y = "Proportion") +  # Labels
  theme(
    strip.background = element_blank(),  # Remove facet label background
    strip.text.x = element_text(size = 10),  # Adjust facet label text size
    axis.text.x = element_text(),  # Horizontal x-axis labels
    axis.ticks.x = element_line()  # Show x-axis ticks
  )

plot

sum_gen_ind <- xtable(result5_bu2)
print(sum_gen_ind, include.rownames=FALSE)

ggplot(data = result52, aes(x = NAME, y = Proportion, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ GENERATIONEX, scales = "free_x") +
  labs(title = "Proportion by INDIVIDUAL and GENERATION (>49 total)",
       x = "NAME",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  viridis::scale_color_viridis(discrete = TRUE) 


individualbygen <- indicative_pres_newgen %>%
  group_by(GENERATIONEX, NAME, source) %>%
  summarise(Count = n()) %>%
  mutate(Total= sum(Count)) %>%  
  ungroup() %>%
  mutate(Proportion = Count / Total) %>% 
  filter(Total > 100)


individualbygen

sum_gen_ind2 <- xtable(individualbygen)
print(sum_gen_ind2, include.rownames=FALSE)

ggplot(data = individualbygen, aes(x = NAME, y = Proportion, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ GENERATIONEX, scales = "free_x") +
  labs(title = "Proportion by INDIVIDUAL and GENERATION (>100 total)",
       x = "NAME",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  viridis::scale_color_viridis(discrete = TRUE) 

individualbyperiod <- indicative_pres_newgen %>%
  group_by(PERIOD, NAME, source) %>%
  summarise(Count = n()) %>%
  mutate(Total= sum(Count)) %>%  
  ungroup() %>%
  mutate(Proportion = Count / Total) %>% 
  filter(Total > 30)

ggplot(data = individualbyperiod_total, aes(x = NAME, y = Proportion, fill = source)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ PERIOD, scales = "free_x") +
  labs(title = "Proportion by INDIVIDUAL and PERIOD (IND/PRS, >30 total)",
       x = "NAME",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  viridis::scale_color_viridis(discrete = TRUE) 

individualbyperiod_total <- indicative_pres_newgen %>%
  group_by(PERIOD, GENDER, RANKNR, NAME, source) %>%
  summarise(Count = n()) %>%
  mutate(Total= sum(Count)) %>%  
  ungroup() %>%
  mutate(Proportion = Count / Total) %>% 
  filter(Total > 10)

individualbyperiod_total_table <- xtable(individualbyperiod_total)
print(individualbyperiod_total_table, include.rownames = FALSE)

df_modelprep  <- indicative_rev %>%
  mutate(LETTER = coalesce(Bréf, doc_file_name, FILE))

df_modelprep$PERIOD <- as.numeric(df_modelprep$PERIOD)
df_modelprep$year<- as.numeric(df_modelprep$year)
df_modelprep$AGE<- as.numeric(df_modelprep$AGE)

df_modelprep_na  <- df_modelprep %>%
  filter(is.na(LETTER))

df_modelprep <- df_modelprep %>%
  mutate(
    AGE_scaled = scale(AGE, center = TRUE, scale = TRUE),
    year_scaled = scale(year, center = TRUE, scale = TRUE)
  )

df_modelprep <- df_modelprep %>%
  mutate(response = if_else(source == "buinn", 1, 0))

model_vars_df <- df_modelprep %>%
  dplyr::select(response, Tensec, year_scaled, year, GENDER, RANKNR, ORIGINREGION, AGE_scaled, NAME, LETTER)


model_vars_df_filter <- model_vars_df %>%
  filter(ORIGINREGION == "HIGH")

#Manual fix, drop levels and check again

model_vars_df$ORIGINREGION[model_vars_df$NAME == "Sigríður Pálsdóttir"] <- "E"
model_vars_df$ORIGINREGION <- droplevels(model_vars_df$ORIGINREGION)
model_vars_df$ORIGINREGION <- as.factor(model_vars_df$ORIGINREGION)
levels(model_vars_df$ORIGINREGION)

model_vars_df$Tensec <- as.factor(model_vars_df$Tensec)
levels(model_vars_df$Tensec)

#Change reference levels, keep dummy coding...

model_vars_df$GENDER <- relevel(model_vars_df$GENDER, ref = "MALE")
model_vars_df$ORIGINREGION <- relevel(model_vars_df$ORIGINREGION, ref = "SW")

levels(model_vars_df$ORIGINREGION)
levels(model_vars_df$GENDER)

#Drop rows with any NA values across these specific columns

cleaned_df <- drop_na(model_vars_df)

write_csv(cleaned_df, "cleaned_df_2504fix.csv")


