summary_female_byyear <- indicative_pres_newgen %>%
  filter(GENDER=="FEMALE") %>%
  group_by(year,source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

buinn_femaleagg <- summary_female_byyear  %>%
  filter(source == "buinn")

p = ggplot(aes(x = year, y = Proportion), data = buinn_femaleagg) +
  geom_point(aes(size = Count), alpha = 0.5, position = "identity") +
  geom_smooth(method = "loess", se = FALSE, size = 4) +
  scale_x_continuous(name = "Decade", limits = c(1780, 1930)) +
  scale_y_continuous(name = "Proportion of forms", breaks = seq(0, 1, .25)) +
  theme(legend.position = "none") +
  scale_size_area(name = "Count", max_size = 20) +
  coord_cartesian(xlim = c(1780, 1930), ylim = c(0, 1))
p


summary_male_byyear <- indicative_pres_newgen %>%
  filter(GENDER=="MALE") %>%
  group_by(year,source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

buinn_maleagg <- summary_male_byyear   %>%
  filter(source == "buinn")

p = ggplot(aes(x = year, y = Proportion), data = buinn_maleagg) +
  geom_point(aes(size = Count), alpha = 0.5, position = "identity") +
  geom_smooth(method = "loess", se = FALSE, size = 4) +
  scale_x_continuous(name = "Decade", limits = c(1780, 1930)) +
  scale_y_continuous(name = "Proportion of forms", breaks = seq(0, 1, .25)) +
  theme(legend.position = "none") +
  scale_size_area(name = "Count", max_size = 20) +
  coord_cartesian(xlim = c(1780, 1930), ylim = c(0, 1))
p


summary_maleofficials_byyear <- indicative_pres_newgen %>%
  filter(GENDER=="MALE") %>%
  filter(RANKNR=="OFFICIALS/LETTERED")  %>%
  group_by(year,source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  filter(Count > 5)


buinn_maleofficialsagg <- summary_maleofficials_byyear   %>%
  filter(source == "buinn")

p = ggplot(aes(x = year, y = Proportion), data = buinn_maleofficialsagg) +
  geom_point(aes(size = Count), alpha = 0.5, position = "identity") +
  geom_smooth(method = "loess", se = FALSE, size = 4) +
  scale_x_continuous(name = "Decade", limits = c(1780, 1930)) +
  scale_y_continuous(name = "Proportion of forms", breaks = seq(0, 1, .25)) +
  theme(legend.position = "none") +
  scale_size_area(name = "Count", max_size = 20) +
  coord_cartesian(xlim = c(1800, 1860), ylim = c(0, 1))
p

summary_femaleofficials_byyear <- indicative_pres_newgen %>%
  filter(GENDER=="FEMALE") %>%
  filter(RANKNR=="OFFICIALS/LETTERED")  %>%
  group_by(year,source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

buinn_femaleofficialsagg <- summary_femaleofficials_byyear   %>%
  filter(source == "buinn")

p = ggplot(aes(x = year, y = Proportion), data = buinn_femaleofficialsagg) +
  geom_point(aes(size = Count), alpha = 0.5, position = "identity") +
  geom_smooth(method = "loess", se = FALSE, size = 4) +
  scale_x_continuous(name = "Decade", limits = c(1780, 1930)) +
  scale_y_continuous(name = "Proportion of forms", breaks = seq(0, 1, .25)) +
  theme(legend.position = "none") +
  scale_size_area(name = "Count", max_size = 20) +
  coord_cartesian(xlim = c(1800, 1920), ylim = c(0, 1))
p


summary_femalepeasants_byyear <- indicative_pres_newgen %>%
  filter(GENDER=="FEMALE") %>%
  filter(RANKNR=="PEASANTS/LABOURERS")  %>%
  group_by(year,source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

buinn_femalepeasantssagg <- summary_femalepeasants_byyear   %>%
  filter(source == "buinn")

p = ggplot(aes(x = year, y = Proportion), data = buinn_femalepeasantssagg) +
  geom_point(aes(size = Count), alpha = 0.5, position = "identity") +
  geom_smooth(method = "loess", se = FALSE, size = 4) +
  scale_x_continuous(name = "Decade", limits = c(1780, 1930)) +
  scale_y_continuous(name = "Proportion of forms", breaks = seq(0, 1, .25)) +
  theme(legend.position = "none") +
  scale_size_area(name = "Count", max_size = 20) +
  coord_cartesian(xlim = c(1800, 1920), ylim = c(0, 1))
p

summary_femaleoth_byyear <- indicative_pres_newgen %>%
  filter(GENDER=="FEMALE") %>%
  filter(RANKNR=="OTHER PROFESSIONS")  %>%
  group_by(year,source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

buinn_femaleothagg <- summary_femaleoth_byyear   %>%
  filter(source == "buinn")

p = ggplot(aes(x = year, y = Proportion), data = buinn_femaleothagg) +
  geom_point(aes(size = Count), alpha = 0.5, position = "identity") +
  geom_smooth(method = "loess", se = FALSE, size = 4) +
  scale_x_continuous(name = "Decade", limits = c(1780, 1930)) +
  scale_y_continuous(name = "Proportion of forms", breaks = seq(0, 1, .25)) +
  theme(legend.position = "none") +
  scale_size_area(name = "Count", max_size = 20) +
  coord_cartesian(xlim = c(1800, 1920), ylim = c(0, 1))
p





summary_malepeasants_byyear <- indicative_pres_newgen %>%
  filter(GENDER=="MALE") %>%
  filter(RANKNR=="PEASANTS/LABOURERS")  %>%
  group_by(year,source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

buinn_malepeasantssagg <- summary_malepeasants_byyear   %>%
  filter(source == "buinn")

p = ggplot(aes(x = year, y = Proportion), data = buinn_malepeasantssagg) +
  geom_point(aes(size = Count), alpha = 0.5, position = "identity") +
  geom_smooth(method = "loess", se = FALSE, size = 4) +
  scale_x_continuous(name = "Decade", limits = c(1780, 1930)) +
  scale_y_continuous(name = "Proportion of forms", breaks = seq(0, 1, .25)) +
  theme(legend.position = "none") +
  scale_size_area(name = "Count", max_size = 20) +
  coord_cartesian(xlim = c(1800, 1930), ylim = c(0, 1))
p




summary_maleoth_byyear <- indicative_pres_newgen %>%
  filter(GENDER=="MALE") %>%
  filter(RANKNR=="OTHER PROFESSIONS")  %>%
  group_by(year,source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

buinn_maleothagg <- summary_maleoth_byyear   %>%
  filter(source == "buinn")

p = ggplot(aes(x = year, y = Proportion), data = buinn_maleothagg) +
  geom_point(aes(size = Count), alpha = 0.5, position = "identity") +
  geom_smooth(method = "loess", se = FALSE, size = 4) +
  scale_x_continuous(name = "Decade", limits = c(1780, 1930)) +
  scale_y_continuous(name = "Proportion of forms", breaks = seq(0, 1, .25)) +
  theme(legend.position = "none") +
  scale_size_area(name = "Count", max_size = 20) +
  coord_cartesian(xlim = c(1800, 1920), ylim = c(0, 1))
p





summary_female_byyearpast <- indicative %>%
  filter(Tensec=="Past") %>%
  filter(GENDER=="FEMALE") %>%
  group_by(year,source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

buinn_femaleaggp <- summary_female_byyearpast  %>%
  filter(source == "buinn")

p = ggplot(aes(x = year, y = Proportion), data = buinn_femaleaggp) +
  geom_point(aes(size = Count), alpha = 0.5, position = "identity") +
  geom_smooth(method = "loess", se = FALSE, size = 4) +
  scale_x_continuous(name = "Decade", limits = c(1780, 1930)) +
  scale_y_continuous(name = "Proportion of forms", breaks = seq(0, 1, .25)) +
  theme(legend.position = "none") +
  scale_size_area(name = "Count", max_size = 20) +
  coord_cartesian(xlim = c(1780, 1930), ylim = c(0, 1))
p


summary_male_byyearp <- indicative %>%
  filter(Tensec=="Past") %>%
  filter(GENDER=="MALE") %>%
  group_by(year,source) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

buinn_maleaggp <- summary_male_byyearp   %>%
  filter(source == "buinn")

p = ggplot(aes(x = year, y = Proportion), data = buinn_maleaggp) +
  geom_point(aes(size = Count), alpha = 0.5, position = "identity") +
  geom_smooth(method = "loess", se = FALSE, size = 4) +
  scale_x_continuous(name = "Decade", limits = c(1780, 1930)) +
  scale_y_continuous(name = "Proportion of forms", breaks = seq(0, 1, .25)) +
  theme(legend.position = "none") +
  scale_size_area(name = "Count", max_size = 20) +
  coord_cartesian(xlim = c(1780, 1930), ylim = c(0, 1))
p


