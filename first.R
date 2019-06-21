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

graph %>% 
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
  labs(x = "Age", y = "Percent Who Disapprove", title = "Republicans Who Disapprove of Trump by Age and Gender", caption = "Data: CCES 2018") +
  ggsave("D://nevertrump/age_disapprove.png", type = "cairo-png")



## By Race ####

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
  theme(strip.text = element_text(size = 18)) +
  scale_fill_manual(values = c("#000080", "#6593F5", "#A45A52", "#800000" )) +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 5, family = "font") +
  labs(x = "Approval", y = "", title = "Trump Approval by Ideology", subtitle = "Among Republicans", caption = "Data: CCES 2018") + 
  ggsave("D://nevertrump/ideo5.png", type = "cairo-png", width = 11, height = 10)

