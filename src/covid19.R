library(tidyverse)
library(tidyquant)
library(car)
library(cowplot)
library(wrapr)
library(Hmisc)

dir = "~/Development/stat184/brent-covid-19/COVID-19/csse_covid_19_data/csse_covid_19_time_series"

Confirmed <- 
  read.csv(file.path(dir,
                     "time_series_covid19_confirmed_global.csv")) %>%
  pivot_longer(cols=c(starts_with("X")), names_to="date", values_to="confirmed")

Deaths <- 
  read.csv(file.path(dir,
                     "time_series_covid19_deaths_global.csv")) %>%
  pivot_longer(cols=c(starts_with("X")), names_to="date", values_to="deaths")

Recovered <- 
  read.csv(file.path(dir,
                     "time_series_covid19_recovered_global.csv")) %>%
  pivot_longer(cols=c(starts_with("X")), names_to="date", values_to="recovered")

# get all data in one data table, and make everything tidy
COVID19 <-
  Confirmed %>%
  left_join(Deaths %>% select(-c(Lat, Long)), 
            by=c('Province.State' = 'Province.State', 
                 'Country.Region' = 'Country.Region', 
                 'date'='date')) %>%
  left_join(Recovered %>% select(-c(Lat, Long)),
            by=c('Province.State' = 'Province.State', 
                 'Country.Region' = 'Country.Region', 
                 'date'='date')) %>%
  rename(c("province.state"="Province.State", 
           "country.region"="Country.Region", 
           "lat"="Lat", 
           "long"="Long")) %>%
  mutate(date = lubridate::mdy(gsub(pattern = "^X", replacement="", date)))

Brent <- 
  tq_get("BZ=F", from=mdy("1.22.2020")) %>%
  select(-symbol) %>%
  # Get rid of NA rows where trading wasn't open
  drop_na()

logistic_model = function(xvariable, yvariable, df) {
  #xvar <- enquo(xvar)
  #yvar <- enquo(yvar)
  let(
    c(xvar=xvariable, yvar=yvariable),
    {
      start <- coef(lm(logit(yvar / 100) ~ xvar, data = df))
      
      nls_model <- nls(yvar ~ phi1 / (1 + exp(-(phi2 + phi3 * xvar))),
                       start = list(phi1 = 100000, phi2 = start[[1]], phi3=start[[2]]), 
                       data = df)
    }
  )
      
  # Values for the logistic growth model
  phi1 <- coef(nls_model)[1]
  phi2 <- coef(nls_model)[2]
  phi3 <- coef(nls_model)[3]
  
  # Get x value range
  x <- c(min(df[xvariable]):(max(df[xvariable])+50))
      
  # Construct model return values and return the resulting data frame
  y <- phi1 / (1 + exp(-(phi2 + phi3 * x)))
  return(data.frame(x, y))
}

Hubei <-
  COVID19 %>%
  filter(province.state=="Hubei") %>%
  mutate(datediff = as.numeric(date - mdy("1-22-2020")))

start <- coef(lm(logit(confirmed/100) ~ datediff, data = Hubei)) 

# Important to note that the asymptote, 100000, was an assumption
hubei_nls <- nls(confirmed ~ phi1 / (1 + exp(-(phi2 + phi3 * datediff))),
                 start = list(phi1 = 100000, phi2 = start[[1]], phi3=start[[2]]), 
                 data = Hubei)

hubei_logit <- logistic_model(hubei_nls, "datediff", Hubei)

hubei_graph <-
  Hubei %>%
  ggplot(aes(datediff, confirmed))+
  geom_point(color='blue') +
  labs(x='Days Since 1-22-2020', y='Confirmed Cases')+
  geom_line(data=hubei_logit, aes(x, y)) +
  geom_vline(aes(xintercept=10), linetype = 3) +
  geom_vline(aes(xintercept=40), linetype = 3) +
  xlim(0, 60) +
  theme_minimal()

# Me trying to make confidence intervals :(
time <- 1:25
df   <- data.frame(time,
                   pop=rnorm(100*length(time), mean=10*time/(25+time)))

library(ggplot2)
ggplot(df, aes(x=time, y=pop))+ 
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, 
               fun.args=list(conf.int=0.95), fill="lightblue")+
  stat_summary(geom="line", fun=mean, linetype="dashed")+
  stat_summary(geom="point", fun=mean, color="red")