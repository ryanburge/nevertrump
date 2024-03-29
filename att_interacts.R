reg <- cces18 %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4=1; 1:2=0; else =NA")) %>%
  mutate(pid2 = frcode(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ "Democrat",
                       pid7 == 5 | pid7 == 6 | pid7 == 7 ~ "Republican")) %>% 
  mutate(trad = case_when(evangelical == 1 ~ "Evangelical",
                          mainline == 1 ~ "Mainline",
                          catholic == 1 ~ "Catholic",
                          religpew == 3 ~ "Mormon")) %>% 
  mutate(trad = as.factor(trad)) %>% 
  mutate(male = car::recode(gender, "1=1; else=0")) %>% 
  mutate(male = as.factor(male)) %>% 
  mutate(age = 2018 - birthyr) %>% 
  mutate(white = car::recode(race, "1=1; else =0")) %>% 
  mutate(white = as.factor(white)) %>% 
  mutate(att = car::recode(pew_churatd, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else =NA"))

reg1 <- glm(dis_trump ~ att*trad*pid2 + educ + age + white, data = reg, family = "binomial")

gg2 <- interact_plot(reg1, pred= att, modx = pid2, mod2 = trad, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican") , mod2.labels = c("Evangelical", "Mainline", "Catholic", "Mormon")) 


gg2 + 
  labs(x = "Church Attendance", y = "Percent Disapproving of Trump", title = "Interaction of Attendance and Party ID on Trump Approval", caption = "Data: CCES 2018", subtitle = "") +
  theme_gg("Lato") +
  scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "", "Yearly", "", "Weekly", "")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = c(.15,.85)) +
  theme(plot.title = element_text(size = 14)) +
  ggsave("D://nevertrump/att_gender_interact.png", width = 6)
 
