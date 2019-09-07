gun1 <- cces18 %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4='Disapprove'; 1:2='Approve'; else =NA")) %>% 
  filter(dis_trump != "NA") %>% 
  mutate(background = car::recode(CC18_320a, "1=1; 2=0; else =NA")) %>% 
  group_by(dis_trump) %>% 
  mean_ci(background) %>% 
  mutate(issue = "Background Checks\nfor All Sales")
  
gun2 <- cces18 %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4='Disapprove'; 1:2='Approve'; else =NA")) %>% 
  filter(dis_trump != "NA") %>% 
  mutate(ar15 = car::recode(CC18_320c, "1=1; 2=0; else = NA")) %>% 
  group_by(dis_trump) %>% 
  mean_ci(ar15) %>% 
  mutate(issue = "Ban Assault Rifles")
  
gun3 <- cces18 %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4='Disapprove'; 1:2='Approve'; else =NA")) %>% 
  filter(dis_trump != "NA") %>% 
  mutate(conceal = car::recode(CC18_320d, "2=1; 1 =0; else = NA")) %>% 
  group_by(dis_trump) %>% 
  mean_ci(conceal) %>% 
  mutate(issue = "Make it Harder to Get\nConceal Carry Permit")  
  
graph <- bind_df("gun")


graph %>% 
  ggplot(., aes(x=dis_trump, y = mean, fill = dis_trump)) +
  geom_col(color = "black") +
  facet_wrap(~issue) +
  theme_gg("Lato") +
  scale_fill_simpsons() + 
  scale_y_continuous(labels = percent) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9))  +
  geom_text(aes(y = .25, label = paste0(mean*100, '%')), position = position_stack(vjust = 0.5), size = 5, family = "font") +
  labs(x = "Trump Approval Status", y = "", title = "Support for Gun Control Policies", subtitle = "Among Republicans", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/guncontrol_facets.png", type = "cairo-png")


  
  