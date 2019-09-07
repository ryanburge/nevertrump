
graph <- cces18 %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4='Disapprove of Trump'; 1:2='Approve of Trump'; else =NA")) %>% 
  mutate(house = car::recode(CC18_412, "1 = 'Democrat'; 2 = 'Republican'; 3:90 = 'All Others'; else = NA")) %>%  
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  group_by(dis_trump) %>% 
  ct(house, show_na = FALSE) %>% 
  filter(dis_trump != "NA")

graph %>% 
  ggplot(., aes(x = house, y = pct, fill = house)) +
  geom_col(color = "black") +
  facet_wrap(~ dis_trump) +
  theme_gg("Lato") +
  scale_fill_manual(values = c("azure3", "dodgerblue3", "firebrick3")) +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 14)) +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 5, family = "font") +
  labs(x = "House Vote", y = "", title = "House Vote Among Republicans by Trump Approval", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/house_vote.png", type = "cairo-png")

cces10 <- read.fst("C://cces10.fst")

graph <- cces10 %>% 
  mutate(dis_obama = car::recode(CC308a, "3:4='Disapprove of Obama'; 1:2='Approve of Obama'; else =NA")) %>% 
  mutate(house = car::recode(CC412, "1 = 'Democrat'; 2 = 'Republican'; 3:90 = 'All Others'; else = NA")) %>%  
  mutate(dem = car::recode(V212d, "1:3=1; else=0")) %>% 
  filter(dem == 1) %>% 
  group_by(dis_obama) %>% 
  ct(house, show_na = FALSE) %>% 
  filter(dis_obama != "NA")


graph %>% 
  ggplot(., aes(x = house, y = pct, fill = house)) +
  geom_col(color = "black") +
  facet_wrap(~ dis_obama) +
  theme_gg("Lato") +
  scale_fill_manual(values = c("azure3", "dodgerblue3", "firebrick3")) +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 14)) +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 5, family = "font") +
  labs(x = "House Vote", y = "", title = "House Vote Among Democrats by Obama Approval", caption = "Data: CCES 2010") +
  ggsave("D://nevertrump/house_vote10.png", type = "cairo-png")





