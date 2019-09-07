

abort1 <- cces18 %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4='Disapprove'; 1:2='Approve'; else =NA")) %>% 
  filter(dis_trump != "NA") %>% 
  mutate(illegal = recode(CC18_321f, "1=1; 2=0; else = NA")) %>% 
  group_by(dis_trump) %>% 
  mean_ci(illegal) %>% 
  mutate(issue = "Completely Illegal")
  
abort2 <- cces18 %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4='Disapprove'; 1:2='Approve'; else =NA")) %>% 
  filter(dis_trump != "NA") %>% 
  mutate(illegal = recode(CC18_321a, "1=1; 2=0; else = NA")) %>% 
  group_by(dis_trump) %>% 
  mean_ci(illegal) %>% 
  mutate(issue = "Allow Abortion\nAs a Choice")

abort3 <- cces18 %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(dis_trump = car::recode(CC18_308c, "3:4='Disapprove'; 1:2='Approve'; else =NA")) %>% 
  filter(dis_trump != "NA") %>% 
  mutate(illegal = recode(CC18_321c, "1=1; 2=0; else = NA")) %>% 
  group_by(dis_trump) %>% 
  mean_ci(illegal) %>% 
  mutate(issue = "Ban After\n20 Weeks")

abort4 <- cces18 %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4='Disapprove'; 1:2='Approve'; else =NA")) %>% 
  filter(dis_trump != "NA") %>% 
  mutate(illegal = recode(CC18_321d, "1=1; 2=0; else = NA")) %>% 
  group_by(dis_trump) %>% 
  mean_ci(illegal) %>% 
  mutate(issue = "Employers Deny\nAbortion Ins. Coverage")

abort5 <- cces18 %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4='Disapprove'; 1:2='Approve'; else =NA")) %>% 
  filter(dis_trump != "NA") %>% 
  mutate(illegal = recode(CC18_321e, "1=1; 2=0; else = NA")) %>% 
  group_by(dis_trump) %>% 
  mean_ci(illegal) %>% 
  mutate(issue = "Prohibit Federal\nFunds")

abort6 <- cces18 %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4='Disapprove'; 1:2='Approve'; else =NA")) %>% 
  filter(dis_trump != "NA") %>% 
  mutate(illegal = recode(CC18_321b, "1=1; 2=0; else = NA")) %>% 
  group_by(dis_trump) %>% 
  mean_ci(illegal) %>% 
  mutate(issue = "Only for Rape,\nIncest, Life of Mother")

graph <- bind_df("abort")


graph %>% 
  ggplot(., aes(x=dis_trump, y = mean, fill = dis_trump)) +
  geom_col(color = "black") +
  facet_wrap(~issue, ncol = 3) +
  theme_gg("Lato") +
  scale_fill_simpsons() + 
  scale_y_continuous(labels = percent) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9))  +
  geom_text(aes(y = .25, label = paste0(mean*100, '%')), position = position_stack(vjust = 0.5), size = 5, family = "font") +
  labs(x = "Trump Approval Status", y = "", title = "Support for Abortion Policies", subtitle = "Among Republicans", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/abortion_facets.png", type = "cairo-png")

