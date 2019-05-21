library("tidyverse", quietly = TRUE)
library("broom")

set.seed(42)
x <- 1:20
y <- rnorm(20)

mod <- lm(y ~ x)

#anova 
anova(mod)

# p-value is 0.1252

#summary
summary(mod)

# Effect size is 0.1256

# with broom
glance(mod)
tidy(mod)

# LOTS OF RANDOM DATA

mod1000 <- rerun(.n = 1000, tibble(x = 1:20, y = rnorm(20))) %>% 
  map(~ lm(y ~ x, data = .)) %>% 
  map_df(tidy) %>%
  filter(term == "x")

# endret .n = 2 til .n = 1000 for å finne p-value av mange (1000) modeller  

nbins <- 20 

ggplot(mod1000, aes(x = p.value)) + 
  geom_histogram(binwidth = 1/nbins, center = 1/nbins/2) + 
  geom_hline(yintercept = 1000/nbins, colour = "blue")

mod1000 %>%
  mutate(sig = p.value < 0.05) %>%
  ggplot(aes(x = 0, fill = sig)) + geom_histogram() + geom_vline(xintercept = 0, colour = "grey50", linetype = "dashed")




mod1000 <- rerun(.n = 1000, data_frame(x = 1:20, y = rnorm(20))) %>% 
  map(~ lm(y ~ x, data = .)) %>% 
  map_df(tidy) %>% 
  filter(term == "x")

nbins <-  20
ggplot(mod1000, aes(x = p.value)) +
  geom_histogram(binwidth = 1/nbins, center = 1/nbins/2) +
  geom_hline(yintercept = 1000/nbins, colour = "red")

mod1000 %>% mutate(sig = p.value < 0.05) %>% 
  ggplot(aes(x = estimate, fill = sig)) +
  geom_histogram() +
  geom_vline(xintercept = 0, colour = "grey50", linetype = "dashed")






# FINDING A SMALL EFFECT

mod1000 <- rerun(.n = 1000, tibble(x = 1:20, y = rnorm(20, x * 0.1))) %>% 
  map(~ lm(y ~ x, data = .)) %>% 
  map_df(glance) 

nbins <- 20 

ggplot(mod1000, aes(x = p.value)) + geom_histogram(binwidth = 1/nbins, center = 1/nbins/2) + geom_hline(yintercept = 1000/nbins, colour = "blue")

# rnorm gir bare et random nummer av 20 i dette tilfellet

# EFFECT OF SAMPLE SIZE #

mod1000 %>%
  mutate(sig = p.value < 0.05) %>%
  ggplot(aes(x = 0.1, fill = sig)) + geom_histogram() + geom_vline(xintercept = 0, colour = "grey50", linetype = "dashed")














