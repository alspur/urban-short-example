# example analysis project

# 2020-02-18

# load -----------

library(tidyverse)
library(educationdata)
library(scales)

# get data -----------

# get dc data
dc_enroll_raw <- get_education_data(level = 'schools',
                                    source = 'ccd',
                                    topic = 'directory',
                                    filters = list(year = 2015, 
                                                   fips = 11))


dc_race_raw <- get_education_data(level = 'schools',
                                  source = 'ccd',
                                  topic = 'enrollment',
                                  by = list('race'),
                                  filters = list(year = 2015,
                                                 fips = 11),
                                  add_labels = TRUE)


# clean ----------

dc_race_summary <- dc_race_raw %>%
  filter(enrollment > 0) %>%
  filter(grade == "Total")%>%
  select(ncessch, race, enrollment) %>%
  spread(race, enrollment) 

dc_charter_dist_comp <- dc_enroll_raw %>%
  left_join(dc_race_summary) %>%
  filter(enrollment > 0) %>%
  select(ncessch, school_name, charter, free_or_reduced_price_lunch,
         enrollment, White, Black, Hispanic, Total) %>%
  group_by(charter) %>%
  summarise(enrollment = sum(enrollment),
            frpl_cnt = sum(free_or_reduced_price_lunch),
            black_cnt = sum(Black, na.rm = T),
            white_cnt = sum(White, na.rm = T),
            hisp_cnt = sum(Hispanic, na.rm = T), 
            n_schools = n()) %>%
  mutate(frpl_pct = frpl_cnt / enrollment,
         black_pct = black_cnt / enrollment,
         white_pct = white_cnt / enrollment,
         hisp_pct = hisp_cnt / enrollment,
         other_pct = 1 - (black_pct + white_pct + hisp_pct))

charter_dist_frpl <- dc_charter_dist_comp %>%
  select(charter, frpl_pct) %>%
  mutate(charter = factor(charter, labels = c("District Schools",
                                              "Charter Schools")))

charter_dist_race <- dc_charter_dist_comp %>%
  select(charter, black_pct, white_pct, hisp_pct, other_pct) %>%
  rename(Black = black_pct, White = white_pct, Hispanic = hisp_pct,
         Other = other_pct) %>%
  pivot_longer(-charter, names_to = "race", values_to =  "pct") %>%
  mutate(race = factor(race, levels = c("Black", "Hispanic",
                                        "White", "Other"))) %>%
  mutate(charter = factor(charter, labels = c("District Schools", 
                                              "Charter Schools"))) %>%
  arrange(charter, race) %>%
  group_by(charter) %>%
  mutate(lab_pos = cumsum(pct) - (pct/2))
# plot ----------------------

ggplot(charter_dist_frpl, aes(x = charter, y = frpl_pct, fill = charter)) +
  geom_col()+ 
  geom_text(aes(label = percent(frpl_pct), y = frpl_pct + .02)) +
  scale_y_continuous(limits = c(0,1), labels = percent,expand = c(0,0))+
  scale_fill_manual(values = c("#4b7e9a", "#953545"))+
  labs(x = "", y= "Percent Free/Reduced Price Lunch") +
  theme_bw() +
  theme(legend.position = "none", axis.ticks = element_blank())

ggsave("frpl_comp.png", height = 4, width = 4, units = "in")

ggplot(charter_dist_race, aes(x = charter, y = pct, fill = race)) +
  geom_col(position = position_stack(reverse = TRUE))+ 
  geom_text(aes(label = percent(pct), y = lab_pos)) +
  scale_y_continuous(limits = c(0,1), labels = percent,expand = c(0,0))+
  scale_fill_manual(values = c("grey90", "grey50", "#4b7e9a", "#953545"))+
  labs(x = "", y= "Percent of Enrollment, 2015", fill = "Race") +
  theme_bw() +
  theme(axis.ticks = element_blank())

ggsave("race_comp.png", height = 4, width = 4, units = "in")

