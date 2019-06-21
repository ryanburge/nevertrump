cces18 <- cces18 %>% 
  mutate(bsec = car::recode(CC18_322a, "1=1; 2=0; else = NA")) %>% 
  mutate(daca = car::recode(CC18_322b, "2=1; 1=0; else = NA")) %>% 
  mutate(lottery = car::recode(CC18_322c_new, "1=1; 2=0; else = NA")) %>% 
  mutate(sanc = car::recode(CC18_332c, "1=1; 2=0; else =NA")) %>% 
  mutate(prison = car::recode(CC18_332e, "1=1; 2=0; else =NA")) %>% 
  mutate(imm = bsec + daca + lottery + sanc + prison) %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4=1; 1:2=0; else =NA")) %>%
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  mutate(male = car::recode(gender, "1=1; else=0")) %>% 
  mutate(male = as.factor(male)) %>% 
  mutate(age = 2018 - birthyr) %>% 
  mutate(white = car::recode(race, "1=1; else =0")) %>% 
  mutate(white = as.factor(white)) %>% 
  mutate(att = car::recode(pew_churatd, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else =NA"))


## Bars ####

bar_fun <- function(df, var, name){
  
  var <- enquo(var)

  df %>% 
    filter(rep == 1) %>%
    filter(!! var == 1) %>% 
    filter(dis_trump != "NA") %>% 
    # group_by(white) %>% 
    ct(dis_trump) %>% 
    mutate(group = name) %>% 
    ungroup(white) %>% 
    # mutate(white = frcode(white == 1 ~ "White",
    #                       white == 0 ~ "Non-White")) %>% 
    mutate(dis_trump = frcode(dis_trump == 0 ~ "Approve",
                              dis_trump == 1 ~ "Disapprove"))
    
    
  
}

ttt1 <- cces18 %>% bar_fun(bsec, "Increase Border Security Funding") 
ttt2 <- cces18 %>% bar_fun(daca, "Don't Pass DACA") 
ttt3 <- cces18 %>% bar_fun(lottery, "End Visa Lottery") 
ttt4 <- cces18 %>% bar_fun(sanc, "Withhold Fed. Funds to Sanctuary PDs") 
ttt5 <- cces18 %>% bar_fun(prison, "Imprison Frequent Border Crossers") 

graph <- bind_df("ttt")


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

reg <- cces18 %>% 
  filter(rep ==1)

reg1 <- glm(dis_trump ~ imm*white + educ + age + male + att, data = reg, family = "binomial")

gg2 <- interact_plot(reg1, pred= imm, modx = white, int.width = .76, interval = TRUE, modx.labels = c("Non-White", "White"))


gg2 + 
  labs(x = "<- Opposed to Immigration Restrictions: In Favor of Restrictions -->", y = "Percent Disapproving of Trump", title = "Interaction of Immigration Views and Gender on Trump Approval", caption = "Data: CCES 2018", subtitle = "Among Republicans") +
  theme_gg("Lato") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "", "Yearly", "", "Weekly", "")) +
  scale_color_simpsons() +
  scale_fill_simpsons() +
  scale_y_continuous(labels = percent) +
  theme(legend.position = c(.55,.85)) +
  theme(plot.title = element_text(size = 14)) +
  ggsave("D://nevertrump/imm_race_interact.png", width = 7)
  