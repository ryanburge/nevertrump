library(haven)

vp <- read_dta("D://voter_project/data.dta")


graph <- vp %>% 
  mutate(obama_app = frcode(obamaapp_baseline == 1 ~ "Strongly Approve",
                            obamaapp_baseline == 2 ~ "Somewhat Approve",
                            obamaapp_baseline == 3 ~ "Somewhat Disapprove",
                            obamaapp_baseline == 4 ~ "Strongly Disapprove")) %>% 
  mutate(obama_app2 = frcode(obamaapp_baseline == 1 | obamaapp_baseline == 2 ~ "Approve of Obama",
                            obamaapp_baseline == 3 | obamaapp_baseline == 4 ~ "Disapprove of Obama"))  %>% 
  mutate(pid3_base = frcode(pid7_baseline == 1 | pid7_baseline == 2 | pid7_baseline == 3 ~ "Democrat",
                       pid7_baseline == 4 ~ "Independent",
                       pid7_baseline == 5 | pid7_baseline == 6 | pid7_baseline == 7 ~ "Republican")) %>% 
  mutate(pid3_2017 = frcode(pid7_2017 == 1 | pid7_2017 == 2 | pid7_2017 == 3 ~ "Democrat",
                            pid7_2017 == 4 ~ "Independent",
                            pid7_2017 == 5 | pid7_2017 == 6 | pid7_2017 == 7 ~ "Republican")) %>% 
  filter(pid3_base == "Democrat") %>% 
  group_by(obama_app2) %>% 
  ct(pid3_2017, show_na = FALSE) %>% 
  filter(obama_app2 != "NA")

graph %>% 
  ggplot(., aes(x = 1, y = pct, fill = pid3_2017)) +
  geom_col(color = "black") +
  coord_flip() + 
  facet_wrap(~obama_app2, ncol = 1)+
  scale_fill_manual(values = c('Republican' = "firebrick3", 'Democrat' = "dodgerblue3", 'Independent' = "azure3")) +
  theme_gg("Lato") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()) +
  geom_text(aes(label = paste0(pct*100, '%')), position = position_stack(vjust = 0.5), size = 2, family = "font") +
  labs(x = "", y = "", title = "Political Partisanship in 2017", subtitle = "Among Self Identified Democrats in 2011", caption = "Data: Voter Study Group 2011-2017") +
  ggsave("D://nevertrump/voterproject_dems.png", height = 4)

graph <- vp %>% 
  mutate(obama_app = frcode(obamaapp_baseline == 1 ~ "Strongly Approve",
                            obamaapp_baseline == 2 ~ "Somewhat Approve",
                            obamaapp_baseline == 3 ~ "Somewhat Disapprove",
                            obamaapp_baseline == 4 ~ "Strongly Disapprove")) %>% 
  mutate(obama_app2 = frcode(obamaapp_baseline == 1 | obamaapp_baseline == 2 ~ "Approve of Obama",
                             obamaapp_baseline == 3 | obamaapp_baseline == 4 ~ "Disapprove of Obama"))  %>% 
  mutate(pid3_base = frcode(pid7_baseline == 1 | pid7_baseline == 2 | pid7_baseline == 3 ~ "Democrat",
                            pid7_baseline == 4 ~ "Independent",
                            pid7_baseline == 5 | pid7_baseline == 6 | pid7_baseline == 7 ~ "Republican")) %>% 
  mutate(pid3_2017 = frcode(pid7_2017 == 1 | pid7_2017 == 2 | pid7_2017 == 3 ~ "Democrat",
                            pid7_2017 == 4 ~ "Independent",
                            pid7_2017 == 5 | pid7_2017 == 6 | pid7_2017 == 7 ~ "Republican")) %>% 
  filter(pid3_base == "Republican") %>% 
  group_by(obama_app2) %>% 
  ct(pid3_2017, show_na = FALSE) %>% 
  filter(obama_app2 != "NA")

graph %>% 
  ggplot(., aes(x = 1, y = pct, fill = pid3_2017)) +
  geom_col(color = "black") +
  coord_flip() + 
  facet_wrap(~obama_app2, ncol = 1)+
  scale_fill_manual(values = c('Republican' = "firebrick3", 'Democrat' = "dodgerblue3", 'Independent' = "azure3")) +
  theme_gg("Lato") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()) +
  geom_text(aes(label = paste0(pct*100, '%')), position = position_stack(vjust = 0.5), size = 2, family = "font") +
  labs(x = "", y = "", title = "Political Partisanship in 2017", subtitle = "Among Self Identified Republicans in 2011", caption = "Data: Voter Study Group 2011-2017") +
  ggsave("D://nevertrump/voterproject_rep.png", height = 4)