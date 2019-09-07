library(socsci)
library(car)
library(fst)
library(interactions)
library(ggsci)
source("D://theme.R")

cces18 <- read.fst("C://cces18.fst")
source("D://reltrad/CCES/reltrad18.R")

### First Heat Map ####

graph <- cces18 %>% 
  mutate(app_trump = frcode(CC18_308a == 4 ~ "Strongly disapprove",
                            CC18_308a == 3 ~ "Somewhat disapprove",
                            CC18_308a == 2 ~ "Somewhat approve",
                            CC18_308a == 1 ~ "Strongly approve")) %>% 
  mutate(pid7 = frcode(pid7 == 1 ~ "Strong\nDemocrat",
                       pid7 == 2 ~ "Not\nStrong Dem.",
                       pid7 == 3 ~ "Lean\nDemocrat", 
                       pid7 == 4 ~ "Independent",
                       pid7 == 5 ~ "Lean\nRepublican",
                       pid7 == 6 ~ "Not\nStrong Rep.",
                       pid7 == 7 ~ "Strong\nRepublican")) %>% 
  group_by(pid7) %>% 
  ct(app_trump) %>% 
  na.omit()
  

graph %>% 
  ggplot(., aes(x= pid7, y = app_trump)) +
  geom_tile(aes(fill = pct), color = "black") +
  theme_gg("Lato") +
  scale_fill_gradient(low = "azure3", high = "#E94057") +
  geom_text(aes(x= pid7, y = app_trump, label = paste0(pct*100, '%')), size = 4, family = "font") +
  labs(x= "", y = "", title = "Trump Approval Rating", subtitle = "", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/pid7_app.png", type = "cairo-png", width = 9)

### Gender Heat ####

graph <- cces18 %>% 
  mutate(app_trump = frcode(CC18_308a == 4 ~ "Strongly disapprove",
                            CC18_308a == 3 ~ "Somewhat disapprove",
                            CC18_308a == 2 ~ "Somewhat approve",
                            CC18_308a == 1 ~ "Strongly approve")) %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(gender = frcode(gender == 1 ~ "Men",
                         gender == 2 ~ "Women")) %>% 
  group_by(gender) %>% 
  ct(app_trump) %>% 
  na.omit()

graph %>% 
  ggplot(., aes(x = app_trump, y = pct, fill = gender)) +
  geom_col(color = "black", position = "dodge") +
  scale_y_continuous(labels = percent) +
  scale_fill_d3() +
  theme_gg("Lato") +
  theme(legend.position = "bottom") +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 5, family = "font") +
  labs(x = "", y = "", title = "Trump Approval by Gender", subtitle = "Among Republicans", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/gender_bars.png", type = "cairo-png", width = 7)
  
cces18 %>% 
  mutate(rep = car::recode(pid7, "1:3=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(gender = frcode(gender == 1 ~ "Men",
                         gender == 2 ~ "Women")) %>% 
  ct(gender, wt = commonweight) %>% 
  ct(app_trump) %>% 
  na.omit()



### By Age and Gender ####

graph <- cces18 %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4=1; 1:2=0; else =NA")) %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  mutate(age = 2018 - birthyr) %>% 
  filter(rep == 1) %>% 
  mutate(gender = frcode(gender == 1 ~ "Men", 
                         gender == 2 ~ "Women")) %>% 
  group_by(age, gender) %>%  
  mean_ci(dis_trump, wt = commonweight)

rep <- graph %>% 
  filter(age <= 80) %>% 
  ggplot(., aes(x = age, y = mean, color = gender, group = gender)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax= upper, color = gender, fill = gender), alpha = .4) +
  theme_gg("Lato") +
  theme(legend.position = c(.75,.75)) +
  scale_fill_d3() +
  scale_color_d3() +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 12)) +
  labs(x = "Age", y = "Percent Who Disapprove", title = "Disapproval of Trump by Age and Gender", subtitle = "Among Republicans", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/age_disapprove.png", type = "cairo-png")

graph <- cces18 %>% 
  mutate(app_trump = car::recode(CC18_308a, "3:4=0; 1:2=1; else =NA")) %>% 
  mutate(dem = car::recode(pid7, "1:3=1; else=0")) %>% 
  mutate(age = 2018 - birthyr) %>% 
  filter(dem == 1) %>% 
  mutate(gender = frcode(gender == 1 ~ "Men", 
                         gender == 2 ~ "Women")) %>% 
  group_by(age, gender) %>%  
  mean_ci(app_trump, wt = commonweight)

dem <- graph %>% 
  filter(age <= 80) %>% 
  ggplot(., aes(x = age, y = mean, color = gender, group = gender)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax= upper, color = gender, fill = gender), alpha = .4) +
  theme_gg("Lato") +
  # theme(legend.position = c(.75,.85)) +
  scale_fill_d3() +
  scale_color_d3() +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 12)) +
  labs(x = "Age", y = "Percent Who Approve", title = "Approval of Trump by Age and Gender", subtitle = "Among Democrats", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/age_approve.png", type = "cairo-png")

both <- rep + dem + plot_layout(ncol = 1)

ggsave("D://nevertrump/age_gender_both.png", both)




graph <- cces18 %>% 
  mutate(dis_trump = car::recode(CC18_308a, "3:4=1; 1:2=0; else =NA")) %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  mutate(age = 2018 - birthyr) %>% 
  mutate(age2 = car::recode(age, "40:80 =1; else =0")) %>% 
  filter(rep == 1) %>% 
  filter(age2 == 1) %>% 
  mutate(gender = frcode(gender == 1 ~ "Men", 
                         gender == 2 ~ "Women")) %>% 
  group_by(gender) %>%  
  mean_ci(dis_trump, wt = commonweight)

## By Race Heatmap ####

graph <- cces18 %>% 
  mutate(app_trump = frcode(CC18_308a == 4 ~ "Strongly disapprove",
                            CC18_308a == 3 ~ "Somewhat disapprove",
                            CC18_308a == 2 ~ "Somewhat approve",
                            CC18_308a == 1 ~ "Strongly approve")) %>% 
  mutate(race = frcode(race == 1 ~ "White",
                       race == 2 ~ "Black",
                       race == 3 ~ "Hispanic",
                       race == 4 ~ "Asian", 
                       race >= 5 ~ "All Others")) %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  group_by(race) %>% 
  ct(app_trump) %>% 
  na.omit() 


graph %>% 
  ggplot(., aes(x= race, y = app_trump)) +
  geom_tile(aes(fill = pct), color = "black") +
  theme_gg("Lato") +
  scale_fill_gradient(low = "azure3", high = "#E94057") +
  geom_text(aes(x= race, y = app_trump, label = paste0(pct*100, '%')), size = 4, family = "font") +
  labs(x= "", y = "", title = "Trump Approval Rating Among Republicans", subtitle = "", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/race_app.png", type = "cairo-png", width = 7)


### By Race Bars ####


graph <- cces18 %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(app2 = car::recode(CC18_308a, "3:4=1; 1:2=0; else = NA")) %>% 
  mutate(race = frcode(race == 1 ~ "White",
                       race == 2 ~ "Black",
                       race == 3 ~ "Hispanic",
                       race == 4 ~ "Asian", 
                       race >= 5 ~ "All\nOthers")) %>% 
  group_by(race) %>% 
  mean_ci(app2, wt = commonweight) 
  

rep <- graph %>% 
  ggplot(., aes(x = reorder(race, - mean), y = mean, fill = race)) +
  geom_col(color = "black") +
  ggthemes::scale_fill_tableau("Tableau 10") +
  theme_gg("Lato") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2, position = position_dodge(.9)) +
  geom_text(aes(y = .05, label = paste0(mean*100, '%')), position = position_dodge(width = .9), size = 4, family = "font") +
  labs(x = "", y = "", title = "Percent Who Disapprove of Donald Trump", subtitle = "Among Republicans", caption = "Data: CCES 2018") +
  guides(fill = guide_legend(reverse=T)) +
  ggsave("D://nevertrump/race_stacks.png")


cces18 %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(race = frcode(race == 1 ~ "White",
                       race == 2 ~ "Black",
                       race == 3 ~ "Hispanic",
                       race == 4 ~ "Asian", 
                       race >= 5 ~ "All Others")) %>% 
  ct(race) 


graph <- cces18 %>% 
  mutate(rep = car::recode(pid7, "1:3=1; else=0")) %>% 
  filter(rep == 1) %>% 
  mutate(app2 = car::recode(CC18_308a, "3:4=0; 1:2=1; else = NA")) %>% 
  mutate(race = frcode(race == 1 ~ "White",
                       race == 2 ~ "Black",
                       race == 3 ~ "Hispanic",
                       race == 4 ~ "Asian", 
                       race >= 5 ~ "All\nOthers")) %>% 
  group_by(race) %>% 
  mean_ci(app2, wt = commonweight)


dem <- graph %>% 
  ggplot(., aes(x = reorder(race, - mean), y = mean, fill = race)) +
  geom_col(color = "black") +
  ggthemes::scale_fill_tableau("Tableau 10") +
  theme_gg("Lato") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2, position = position_dodge(.9)) +
  geom_text(aes(y = .03, label = paste0(mean*100, '%')), position = position_dodge(width = .9), size = 4, family = "font") +
  labs(x = "", y = "", title = "Percent Who Approve of Donald Trump", subtitle = "Among Democrats", caption = "Data: CCES 2018") +
  guides(fill = guide_legend(reverse=T)) +
  ggsave("D://nevertrump/race_stacks_dems.png")

both <- rep + dem 

ggsave("D://nevertrump/race_bars_patched.png", both, width = 10)

### Ideology #####

graph <- cces18 %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>%
  mutate(app_trump = frcode(CC18_308a == 4 ~ "Strongly\ndisapprove",
                            CC18_308a == 3 ~ "Somewhat\ndisapprove",
                            CC18_308a == 2 ~ "Somewhat\napprove",
                            CC18_308a == 1 ~ "Strongly\napprove")) %>% 
  mutate(ideo5 = frcode(ideo5 == 1 ~ "Very Liberal",
                        ideo5 == 2 ~ "Liberal",
                        ideo5 == 3 ~ "Moderate",
                        ideo5 == 4 ~ "Conservative",
                        ideo5 == 5 ~ "Very Conservative")) %>% 
  filter(app_trump != "NA") %>% 
  group_by(ideo5) %>% 
  ct(app_trump)

graph %>% 
  filter(ideo5 != "NA") %>% 
  ggplot(., aes(x= app_trump, y = pct, fill = app_trump)) +
  geom_col(color = "black") +
  facet_wrap(~ ideo5) +
  theme_gg("Lato") +
  scale_y_continuous(labels = percent) +
  theme(strip.text = element_text(size = 18)) +
  scale_fill_manual(values = c("#000080", "#6593F5", "#A45A52", "#800000" )) +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 5, family = "font") +
  labs(x = "Approval", y = "", title = "Trump Approval by Ideology", subtitle = "Among Republicans", caption = "Data: CCES 2018") + 
  ggsave("D://nevertrump/ideo5.png", type = "cairo-png", width = 11, height = 10)


cces10 <- read.fst("C://cces10.fst")

graph <- cces10 %>% 
  mutate(app_obama = frcode(CC308a == 4 ~ "Strongly\ndisapprove",
                            CC308a == 3 ~ "Somewhat\ndisapprove",
                            CC308a == 2 ~ "Somewhat\napprove",
                            CC308a == 1 ~ "Strongly\napprove")) %>% 
  mutate(ideo7 = frcode(CC334A == 1 ~ "Very Liberal",
                        CC334A == 2 | CC334A == 3 ~ "Liberal",
                        CC334A == 4 ~ "Moderate",
                        CC334A == 5 | CC334A == 6 ~ "Conservative",
                        CC334A == 7 ~ "Very Conservative")) %>% 
  mutate(dem = car::recode(V212d, "1:3=1; else=0")) %>% 
  filter(dem == 1) %>% 
  group_by(ideo7) %>% 
  ct(app_obama, show_na = FALSE) 

graph %>% 
  filter(ideo7 != "NA") %>% 
  ggplot(., aes(x= app_obama, y = pct, fill = app_obama)) +
  geom_col(color = "black") +
  facet_wrap(~ ideo7) +
  scale_y_continuous(labels = percent) +
  theme_gg("Lato") +
  theme(strip.text = element_text(size = 18)) +
  scale_fill_manual(values = c("#800000", "#A45A52", "#6593F5","#000080")) +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 5, family = "font") +
  labs(x = "Approval", y = "", title = "Obama Approval by Ideology", subtitle = "Among Democrats", caption = "Data: CCES 2010") + 
  ggsave("D://nevertrump/ideo5_2010.png", type = "cairo-png", width = 11, height = 10)

## Very Liberal 2010 + Very Conservative 2018
##   93.4                   97.5     == 4.1
##  Liberal 2010     + Conservative 2018
##   91.8                   91.2     == -.6
##  Moderate 2010    + Moderate 2018
##  81.3                    74.7    == -6.6
## Conservative 2010 + Liberal 2018
##  70.5                  66.8      == -3.7
## Very Conservative 2010 + Very Liberal 2018
##  75.7                  75.8      == . 1


graph1 <- cces10 %>% 
  mutate(app_obama = frcode(CC308a == 4 ~ "Strongly\ndisapprove",
                            CC308a == 3 ~ "Somewhat\ndisapprove",
                            CC308a == 2 ~ "Somewhat\napprove",
                            CC308a == 1 ~ "Strongly\napprove")) %>% 
  mutate(ideo7 = frcode(CC334A == 1 ~ "Very Liberal",
                        CC334A == 2 | CC334A == 3 ~ "Liberal",
                        CC334A == 4 ~ "Moderate",
                        CC334A == 5 | CC334A == 6 ~ "Conservative",
                        CC334A == 7 ~ "Very Conservative")) %>% 
  mutate(dem = car::recode(V212d, "1:3=1; else=0")) %>% 
  filter(dem == 1) %>% 
  group_by(ideo7) %>% 
  ct(app_obama, show_na = FALSE) %>% 
  filter(ideo7 == "Very Liberal") %>% 
  rename(app = app_obama) %>% 
  mutate(pid = "Among Democrats")


graph2 <- cces18 %>% 
  mutate(rep = car::recode(pid7, "5:7=1; else=0")) %>% 
  filter(rep == 1) %>%
  mutate(app_trump = frcode(CC18_308a == 4 ~ "Strongly\ndisapprove",
                            CC18_308a == 3 ~ "Somewhat\ndisapprove",
                            CC18_308a == 2 ~ "Somewhat\napprove",
                            CC18_308a == 1 ~ "Strongly\napprove")) %>% 
  mutate(ideo5 = frcode(ideo5 == 1 ~ "Very Liberal",
                        ideo5 == 2 ~ "Liberal",
                        ideo5 == 3 ~ "Moderate",
                        ideo5 == 4 ~ "Conservative",
                        ideo5 == 5 ~ "Very Conservative")) %>% 
  filter(app_trump != "NA") %>% 
  group_by(ideo5) %>% 
  ct(app_trump) %>% 
  filter(ideo5 == "Very Conservative") %>% 
  rename(ideo7 = ideo5, app = app_trump) %>% 
  mutate(pid = "Among Republicans") 

graph <- bind_rows(graph1, graph2) %>% 
  select(ideo7, app, pct, pid)

graph %>% 
  filter(pid == "Among Democrats") %>% 
  ggplot(., aes(x= app, y = pct, fill = app)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = percent) +
  theme_gg("Abel") +
  scale_fill_manual(values = c("#800000", "#A45A52", "#6593F5","#000080")) +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 5, family = "font") +
  labs(x = "", y = "", title = "Obama Approval Among Very Liberal Democrats", subtitle = "", caption = "Data: CCES 2010") + 
  ggsave("E://ideo5_2010.png", type = "cairo-png")


graph %>% 
  filter(pid == "Among Republicans") %>% 
  ggplot(., aes(x= app, y = pct, fill = app)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = percent) +
  theme_gg("Abel") +
  scale_fill_manual(values = c("#000080", "#6593F5", "#A45A52", "#800000" )) +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 5, family = "font") +
  labs(x = "", y = "", title = "Trump Approval Among Very Conservative Republicans", subtitle = "", caption = "Data: CCES 2018") + 
  ggsave("E://ideo5_2018.png", type = "cairo-png")

