#cleaned_df_pres is INDICATIVE only, present; we pull data from there. 1780-1910

cleaned_df <- read_csv("cleaned_df_2504fix.csv")

present_indicative <- cleaned_df %>%
  filter(Tensec == "Present")

#all good there....

cycle.data = data.frame(year=rep(0, 131), has.tokens=rep(0,131),
                        ones=rep(0, 131), zeros=rep(0, 131))
for (i in c(0:131)) {
  cycle.data$year[i] = i + 1780 - 1
  cycle.data$has.tokens[i] = nrow(present_indicative %>%
                                    filter(year == i + 1780 - 1)) > 0
  cycle.data$ones[i] = nrow(present_indicative %>%
                              filter(year == i + 1780 - 1, response == 1))
  cycle.data$zeros[i] = nrow(present_indicative %>%
                               filter(year == i + 1780 - 1, response == 0))
}

write.csv(cycle.data, "pres-cycle-full.csv", row.names=F)

male_presind <- present_indicative %>%
  filter(GENDER == "MALE")

cycle.data.m = data.frame(year=rep(0, 131), has.tokens=rep(0,131),
                        ones=rep(0, 131), zeros=rep(0, 131))
for (i in c(0:131)) {
  cycle.data.m$year[i] = i + 1780 - 1
  cycle.data.m$has.tokens[i] = nrow(male_presind %>%
                                    filter(year == i + 1780 - 1)) > 0
  cycle.data.m$ones[i] = nrow(male_presind %>%
                              filter(year == i + 1780 - 1, response == 1))
  cycle.data.m$zeros[i] = nrow(male_presind %>%
                               filter(year == i + 1780 - 1, response == 0))
}

write.csv(cycle.data.m, "pres-cycle-male.csv", row.names=F)

female_presind <- present_indicative %>%
  filter(GENDER == "FEMALE")

cycle.data.f = data.frame(year=rep(0, 131), has.tokens=rep(0,131),
                          ones=rep(0, 131), zeros=rep(0, 131))
for (i in c(0:131)) {
  cycle.data.f$year[i] = i + 1780 - 1
  cycle.data.f$has.tokens[i] = nrow(female_presind %>%
                                      filter(year == i + 1780 - 1)) > 0
  cycle.data.f$ones[i] = nrow(female_presind %>%
                                filter(year == i + 1780 - 1, response == 1))
  cycle.data.f$zeros[i] = nrow(female_presind %>%
                                 filter(year == i + 1780 - 1, response == 0))
}

write.csv(cycle.data.f, "pres-cycle-female.csv", row.names=F)
