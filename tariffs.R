
tariff1 <- cces18 %>% 
  mutate(pid3 = frcode(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ "Democrat",
                       pid7 == 4 ~ "Independent",
                       pid7 == 5 | pid7 == 6 | pid7 == 7 ~ "Republican")) %>% 
  mutate(dis_trump = frcode(CC18_308a == 3 | CC18_308a == 4 ~ "Disapprove\nof Trump",
                            CC18_308a == 1 | CC18_308a == 2 ~ "Approve\nof Trump")) %>% 
  mutate(tariff = recode(CC18_331a, "1=1; 2=0; else = NA")) %>% 
  filter(dis_trump != "NA") %>% 
  filter(pid3 != "NA") %>% 
  group_by(pid3, dis_trump) %>% 
  mean_ci(tariff) %>% 
  mutate(issue = "Tariffs on China")  

tariff2 <- cces18 %>% 
  mutate(pid3 = frcode(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ "Democrat",
                       pid7 == 4 ~ "Independent",
                       pid7 == 5 | pid7 == 6 | pid7 == 7 ~ "Republican")) %>% 
  mutate(dis_trump = frcode(CC18_308a == 3 | CC18_308a == 4 ~ "Disapprove\nof Trump",
                            CC18_308a == 1 | CC18_308a == 2 ~ "Approve\nof Trump")) %>% 
  mutate(tariff = recode(CC18_331b, "1=1; 2=0; else = NA")) %>% 
  filter(dis_trump != "NA") %>% 
  filter(pid3 != "NA") %>% 
  group_by(pid3, dis_trump) %>% 
  mean_ci(tariff) %>% 
  mutate(issue = "Steel Tariffs Excluding\nCanada, Europe, and Mexico")  


tariff3 <- cces18 %>% 
  mutate(pid3 = frcode(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ "Democrat",
                       pid7 == 4 ~ "Independent",
                       pid7 == 5 | pid7 == 6 | pid7 == 7 ~ "Republican")) %>% 
  mutate(dis_trump = frcode(CC18_308a == 3 | CC18_308a == 4 ~ "Disapprove\nof Trump",
                            CC18_308a == 1 | CC18_308a == 2 ~ "Approve\nof Trump")) %>% 
  mutate(tariff = recode(CC18_331c, "1=1; 2=0; else = NA")) %>% 
  filter(dis_trump != "NA") %>% 
  filter(pid3 != "NA") %>% 
  group_by(pid3, dis_trump) %>% 
  mean_ci(tariff) %>% 
  mutate(issue = "Steel Tariffs Including\nCanada, Europe, and Mexico")  

tariff4 <- cces18 %>% 
  mutate(pid3 = frcode(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ "Democrat",
                                pid7 == 4 ~ "Independent",
                                pid7 == 5 | pid7 == 6 | pid7 == 7 ~ "Republican")) %>% 
  mutate(dis_trump = frcode(CC18_308a == 3 | CC18_308a == 4 ~ "Disapprove\nof Trump",
                            CC18_308a == 1 | CC18_308a == 2 ~ "Approve\nof Trump")) %>%  
  mutate(tar1 = car::recode(CC18_331a, "1=1; 2=0; else =NA")) %>% 
  mutate(tar2 = car::recode(CC18_331b, "1=1; 2=0; else =NA")) %>% 
  mutate(tar3 = car::recode(CC18_331c, "1=1; 2=0; else =NA")) %>% 
  mutate(tariff = tar1 + tar2 + tar3) %>% 
  mutate(tariff = tariff/3) %>% 
  filter(dis_trump != "NA") %>% 
  filter(pid3 != "NA") %>% 
  group_by(pid3, dis_trump) %>% 
  mean_ci(tariff) %>% 
  mutate(issue = "Scale of Overall\nTariff Support")  
  

graph <- bind_df("tariff") %>% 
  filter(pid3 != "Independent")


graph %>% 
  ggplot(., aes(x= dis_trump, y = mean, fill = pid3)) +
  geom_col(color = "black", position = "dodge") +
  facet_wrap(~ issue, ncol = 2) +
  theme_gg("Lato") +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3")) + 
  scale_y_continuous(labels = percent) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9))  +
  geom_text(aes(y = .08, label = paste0(mean*100, '%')), size = 3, family = "font", position = position_dodge(width = .9)) +
  theme(legend.position = "bottom") +
  labs(x = "Trump Approval Status", y = "", title = "Support for Tariffs by Party and Approval Status", subtitle = "", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/tariff_4square.png", type = "cairo-png")



tariff1 %>% 
  ggplot(., aes(x=dis_trump, y = mean, fill = dis_trump)) +
  geom_col(color = "black") +
  facet_wrap(~pid3) +
  theme_gg("Lato") +
  scale_fill_simpsons() + 
  scale_y_continuous(labels = percent) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9))  +
  geom_text(aes(y = .25, label = paste0(mean*100, '%')), position = position_stack(vjust = 0.5), size = 5, family = "font") +
  labs(x = "Trump Approval Status", y = "", title = "Support for Tariffs on China", subtitle = "", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/tariff_china.png", type = "cairo-png", width = 9)


tariff2 %>% 
  ggplot(., aes(x=dis_trump, y = mean, fill = dis_trump)) +
  geom_col(color = "black") +
  facet_wrap(~pid3) +
  theme_gg("Lato") +
  scale_fill_simpsons() + 
  scale_y_continuous(labels = percent) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9))  +
  geom_text(aes(y = .25, label = paste0(mean*100, '%')), position = position_stack(vjust = 0.5), size = 5, family = "font") +
  labs(x = "Trump Approval Status", y = "", title = "Support for Steel Tariffs Excluding Canada, Europe, and Mexico", subtitle = "", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/tariff_exclude.png", type = "cairo-png", width = 9)



tariff3 %>% 
  ggplot(., aes(x=dis_trump, y = mean, fill = dis_trump)) +
  geom_col(color = "black") +
  facet_wrap(~pid3) +
  theme_gg("Lato") +
  scale_fill_simpsons() + 
  scale_y_continuous(labels = percent) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9))  +
  geom_text(aes(y = .25, label = paste0(mean*100, '%')), position = position_stack(vjust = 0.5), size = 5, family = "font") +
  labs(x = "Trump Approval Status", y = "", title = "Support for Steel Tariffs Including Canada, Europe, and Mexico", subtitle = "", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/tariff_include.png", type = "cairo-png", width = 9)
