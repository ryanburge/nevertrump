cces18 <- cces18 %>% 
  mutate(bsec = car::recode(CC18_322a, "1=1; 2=0; else = NA")) %>% 
  mutate(daca = car::recode(CC18_322b, "2=1; 1=0; else = NA")) %>% 
  mutate(lottery = car::recode(CC18_322c_new, "1=1; 2=0; else = NA")) %>% 
  mutate(sanc = car::recode(CC18_332c, "1=1; 2=0; else =NA")) %>% 
  mutate(prison = car::recode(CC18_332e, "1=1; 2=0; else =NA")) %>% 
  mutate(imm = bsec + daca + lottery + sanc + prison) %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4=1; 1:2=0; else =NA")) %>%
  mutate(pid2 = frcode(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ "Democrat",
                       pid7 == 5 | pid7 == 6 | pid7 == 7 ~ "Republican")) %>% 
  mutate(male = car::recode(gender, "1=1; else=0")) %>% 
  mutate(male = as.factor(male)) %>% 
  mutate(age = 2018 - birthyr) %>% 
  mutate(white = car::recode(race, "1=1; else =0")) %>% 
  mutate(white = as.factor(white)) %>% 
  mutate(att = car::recode(pew_churatd, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else =NA"))


### Another Way ####


bar_fun <- function(df, var, name){
  
  var <- enquo(var)
  
  df %>% 
    filter(rep == 1) %>%
    filter(!! var != "NA") %>% 
    group_by(dis_trump) %>% 
    mean_ci(!! var) %>% 
    mutate(group = name) %>% 
    ungroup(white) %>% 
    mutate(dis_trump = frcode(dis_trump == 1 ~ "Never\nTrump",
                          dis_trump == 0 ~ "Pro\nTrump"))  %>% 
    filter(dis_trump != "NA")
  
  
  
}

ttt1 <- cces18 %>% bar_fun(bsec, "Increase Border\nSecurity Funding") 
ttt2 <- cces18 %>% bar_fun(daca, "Oppose DACA") 
ttt3 <- cces18 %>% bar_fun(lottery, "End Visa Lottery") 
ttt4 <- cces18 %>% bar_fun(sanc, "Withhold Fed. Funds\nto Sanctuary PDs") 
ttt5 <- cces18 %>% bar_fun(prison, "Imprison Frequent\nBorder Crossers") 

graph <- bind_df("ttt")

graph %>% 
  ggplot(., aes(x=dis_trump, y = mean, fill = dis_trump)) +
  geom_col(color = "black") +
  facet_wrap(~group) +
  theme_gg("Lato") +
  scale_fill_simpsons() + 
  scale_y_continuous(labels = percent) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9))  +
  geom_text(aes(y = .25, label = paste0(mean*100, '%')), position = position_stack(vjust = 0.5), size = 5, family = "font") +
  labs(x = "", y = "", title = "Support for Immigration Policies", subtitle = "Among Republicans", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/immigration_facets.png", type = "cairo-png")


  

## Interaction ####

reg1 <- glm(dis_trump ~ imm*white*pid2 + educ + age + male + att, data = cces18, family = "binomial")

gg2 <- interact_plot(reg1, pred= imm, modx = white, mod2 = pid2, int.width = .76, interval = TRUE, modx.labels = c("Non-White", "White"), mod2.labels = c("Democrat", "Republican"))


gg2 + 
  labs(x = "Higher Values = More Immigration Restriction", y = "Percent Disapproving of Trump", title = "Interaction of Immigration Views and Race on Trump Approval", caption = "Data: CCES 2018", subtitle = "") +
  theme_gg("Lato") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "", "Yearly", "", "Weekly", "")) +
  scale_color_simpsons() +
  scale_fill_simpsons() +
  scale_y_continuous(labels = percent) +
  theme(legend.position = c(.75,.85)) +
  theme(plot.title = element_text(size = 14)) +
  ggsave("D://nevertrump/imm_race_interact_pid2.png", width = 7)
  