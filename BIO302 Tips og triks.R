# loading tidyverse
library("tidyverse")

####pipes %>% cmd-shift-m####
data(BCI, package = "vegan")
plot(sort(colSums(BCI), decreasing = TRUE))

# Rather put it like this: this is supposed to look nicer
#plot(
#    sort(
#      colSum(BCI)
#       , decreasing = TRUE
#        )
#    )

x1 <- colSum(BCI)
x2 <- sort(x1, decreasing = TRUE)
plot(x2)

x <- colSums(BCI)
x <- sort(x, decreasing = TRUE)
plot

BCI %>%
  colSums() %>%
  sort(decreasing = TRUE) %>%
  plot()  

####One table functions ####
#select, filter, mutate, group_by, summarise, slice, count, arrange, nest

as_tibble(iris)            #kjører datasettet iris
iris <- as_tibble(iris)    #navngir funksjonen over iris

iris %>% select(Sepal.Length, Species)    #selects the species

iris %>% select(-Sepal.Width)           #if we want to remove some part of the data

#kolon bruker man til å få bare opp de radene som er fra forran kolon til etter kolon, så denne her fjernet species:
iris %>% select(Sepal.Length:Petal.Length)

#her byttet vi navnet til Sepal.Length til sepal.length, kan også putte inn flere som Species til spp
iris %>% rename(sepal.length = Sepal.Length, spp = Species)


# fiter, ser her på hvilke av de i datasettet mitt som har sepal lengde over 5 OG petal lengde på under 2. 
iris %>% filter(Sepal.Length > 5, Petal.Length < 2) %>%
  select(Species)

#mutate
iris %>% mutate(petal.area = Petal.Length * Petal.Width)
#på denne måten får alle artene store bokstaver
iris %>% mutate(Species = toupper(Species))

#group_by, her har jeg gruppert artene og satt til gjennomsnittslengde på hver av artene
iris %>% group_by(Species) %>%
  summarise(mean_petal_length = mean(Petal.Length))

#her puttet jeg også inn standard avvik
iris %>% group_by(Species) %>%
  summarise(mean_petal_length = mean(Petal.Length), sd_petal_length = sd(Petal.Length))

#group_by and add a new column, not summarise
iris %>% group_by(Species) %>%
  mutate(mean_petal_length = mean(Petal.Length)) %>%
  ungroup()
# ungroup fikser på metadata, with this the data does not get grouped by species when we summarise

iris %>% arrange(Petal.Length)
iris %>% arrange(desc(Petal.Length))

#dette gjør man om man vil vite for eksempel de minste artene først, 1:3 betyr at det er fra 1 til 3, om man putter decending istede for petal length får man de største øverst.
iris %>% group_by(Species) %>% arrange(Petal.Length) %>% slice(1:3)

# dette har tatt alle artene til hver tibble. sorterte alle inn i grupper etter artene
iris %>% group_by(Species) %>% nest()

# kan bruke en mutate, mutate lager en ny column, vi lager en lineær model per art
iris %>% 
  group_by(Species) %>% 
  nest() %>% 
  mutate(mod = map(data, ~lm(Sepal.Length ~ Sepal.Width, data = .))) %>%
  mutate(coef = map(mod, broom::tidy)) %>%   
  unnest(coef) #dette gjør du for å se hva som er inni dataene dine :)

#gather, spread
iris %>%
  rownames_to_column() %>%
  gather(key = variable, value = measurement, -Species, -rowname) %>%
  group_by(Species, variable) %>%
    summarise(mean = mean(measurement))

iris %>%
  rownames_to_column() %>%
  gather(key = variable, value = measurement, -Species, -rowname) %>%
  ggplot(aes(x = variable, y = measurement, fill = Species)) + geom_violin()
