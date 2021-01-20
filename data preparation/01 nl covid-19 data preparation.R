# Getting and preparing NL data on COVID-19 and mortality and population
# Libraries and functions -------------------------------------------------
library(tidyverse)
library(cbsodataR)
library(xts)
library(utils)
library(httr)
library(zoo)
# Get mortality data from CBS -------------------------------------------------
# Get the mortality dataset
all.deaths <- cbs_get_data('70895ned') %>% # specify the dataset id
  cbs_add_label_columns() %>%
  mutate (year = as.numeric(as.character(substr(Perioden, 1, 4))), # extract the year
          week = as.numeric(as.character(substr(Perioden, 7,8))), # extract the week
          year.month = format(strptime(paste(year, week, 1), format = "%Y %W %u"),"%Y.%m"), # make a year.month index
          filter_n = as.character(substr(Perioden, 5,6)), # will be used for filtering out
          deaths = Overledenen_1, # rename column
          n_days = trimws(gsub("\\(", "", substr(Perioden_label, 14, 15))), # this gets the number of days in the week
          n_days = as.numeric(as.character(ifelse (nchar(n_days)==0, 7, n_days))), # usually 7, but not always for weeks 0, 1, 52 and 53; rescale
          week = case_when(week == 0 ~ 1, week == 53 ~ 52, TRUE ~ week) # recode weeks 0 and 53 to 1 and 52
  ) %>% 
  filter(filter_n != 'JJ')  # remove yearly totals

# Make function to filter and aggregate the data
data.prep <- function (sex, age_group){
  deaths.temp <- all.deaths %>% 
    filter (Geslacht == sex, LeeftijdOp31December == age_group) %>% 
    dplyr::select (year, week, year.month, deaths, n_days) %>% 
    mutate (index = paste0(year, '.', week)) %>% 
    group_by (index) %>% # this is needed for weeks 1 and 52
    summarize(year=mean(year), week=mean(week), deaths = sum(deaths), year.month = first (year.month), n_days = sum(n_days)-7) %>%  
    mutate (deaths.s = deaths * 7/(n_days+7)) %>% 
    dplyr::filter (year < 2021) %>% 
    #dplyr::select (-index)  %>% 
    arrange(year, week)
} 

# Apply the function to get all deaths
deaths <- data.prep(sex = 1100, age_group = 10000) # all deaths

# We can also use the function to get deaths for specific sex and age groups
deaths.m <- data.prep(sex = 3000, age_group = 10000) # men only   
deaths.w <- data.prep(sex = 4000, age_group = 10000) # women only   
deaths.80plus <- data.prep(sex = 1100, age_group = 21700) # all, 80+ deaths
deaths.65to80 <- data.prep(sex = 1100, age_group = 53950) # all, 65 to 80 deaths
deaths.0to65 <- data.prep(sex = 1100, age_group = 41700) # all, 0 to 65 deaths
deaths.m.80plus <- data.prep(sex = 3000, age_group = 21700) # men, 80+ deaths
deaths.m.65to80 <- data.prep(sex = 3000, age_group = 53950) # men, 65 to 80 deaths
deaths.m.0to65 <- data.prep(sex = 3000, age_group = 41700) # men, 0 to 65 deaths
deaths.w.80plus <- data.prep(sex = 4000, age_group = 21700) # women, 80+ deaths
deaths.w.65to80 <- data.prep(sex = 4000, age_group = 53950) # women, 65 to 80 deaths
deaths.w.0to65 <- data.prep(sex = 4000, age_group = 41700) # women, 0 to 65 deaths

# Get population data from CBS -------------------------------------------------
all.pop <- cbs_get_data('7461eng') %>%
  cbs_add_label_columns() %>%
  mutate (pop = TotalPopulation_1 / 1e6, # divide by a million
          year = as.numeric(as.character(Periods_label))) %>%
  filter (year > 1994, year < 2021) %>%
  dplyr::select (Sex_label, Age_label, year, pop)

# Make function to filter data
pop.data.prep <- function (sex, age_group){
  pop.temp <- all.pop %>% 
    filter(Sex_label == sex, Age_label == age_group) %>% 
    mutate(change.p = 1+(pop-lag (pop))/lag (pop)) # create a population change variable
  
  pop.temp <- pop.temp %>%  # interpolate data for 2020
    add_row (year = 2020, pop = last(pop.temp$pop)*mean(tail(pop.temp$change.p,2))) %>% # add data for 2020
    dplyr::select (year, pop) 
}

# Apply the function to get total population
pop<-pop.data.prep (sex='Men and women', age_group='Total population (all ages)') # all population

# Population per sex
pop.m<-pop.data.prep (sex='Men', age_group='Total population (all ages)') # men only
pop.w<-pop.data.prep (sex='Women', age_group='Total population (all ages)') # women only

# Population per age group
pop.0to20 <- pop.data.prep (sex='Men and women', age_group='0 to 20 years') 
pop.20to65 <- pop.data.prep (sex='Men and women', age_group='20 to 65 years') 
pop.65plus <- pop.data.prep (sex='Men and women', age_group='65 years or older') 
pop.65to70 <- pop.data.prep (sex='Men and women', age_group='65 to 70 years')
pop.70to75 <- pop.data.prep (sex='Men and women', age_group='70 to 75 years') 
pop.75to80 <- pop.data.prep (sex='Men and women', age_group='75 to 80 years') 

# We gotta do some work to match the age categories from the mortality dataset
pop.0to65 <- left_join(pop.0to20, pop.20to65, by = 'year') %>% 
  mutate (pop = pop.x + pop.y) %>% 
  dplyr::select (year, pop) 

pop.65to80 <- left_join(pop.65to70, pop.70to75, by = 'year') %>% 
  left_join(., pop.75to80, by = 'year') %>% 
  mutate (pop.65=pop.x+pop.y+pop) %>% 
  dplyr::select (year, pop.65)

pop.80plus <- left_join(pop.65plus, pop.65to80, by = 'year') %>% 
  mutate (pop.80 = pop - pop.65) %>% 
  dplyr::select (year, pop.80)

# Three age groups, Men only
pop.m.0to20 <- pop.data.prep (sex='Men', age_group='0 to 20 years') 
pop.m.20to65 <- pop.data.prep (sex='Men', age_group='20 to 65 years') 
pop.m.65plus <- pop.data.prep (sex='Men', age_group='65 years or older') 
pop.m.65to70 <- pop.data.prep (sex='Men', age_group='65 to 70 years')
pop.m.70to75 <- pop.data.prep (sex='Men', age_group='70 to 75 years') 
pop.m.75to80 <- pop.data.prep (sex='Men', age_group='75 to 80 years') 

pop.m.0to65 <- left_join(pop.m.0to20, pop.m.20to65, by = 'year') %>% mutate (pop.0to65 = pop.x + pop.y) %>% dplyr::select (year, pop.0to65) 
pop.m.65to80 <- left_join(pop.m.65to70, pop.m.70to75, by = 'year') %>% left_join(., pop.m.75to80, by = 'year') %>% mutate (pop.65=pop.x+pop.y+pop) %>% dplyr::select (year, pop.65)
pop.m.80plus <- left_join(pop.m.65plus, pop.m.65to80, by = 'year') %>% mutate (pop.80 = pop - pop.65) %>% dplyr::select (year, pop.80)

# Three age groups, Women only
pop.w.0to20 <- pop.data.prep (sex='Women', age_group='0 to 20 years') 
pop.w.20to65 <- pop.data.prep (sex='Women', age_group='20 to 65 years') 
pop.w.65plus <- pop.data.prep (sex='Women', age_group='65 years or older') 
pop.w.65to70 <- pop.data.prep (sex='Women', age_group='65 to 70 years')
pop.w.70to75 <- pop.data.prep (sex='Women', age_group='70 to 75 years') 
pop.w.75to80 <- pop.data.prep (sex='Women', age_group='75 to 80 years') 

pop.w.0to65 <- left_join(pop.w.0to20, pop.w.20to65, by = 'year') %>% mutate (pop = pop.x + pop.y) %>% dplyr::select (year, pop) 
pop.w.65to80 <- left_join(pop.w.65to70, pop.w.70to75, by = 'year') %>% left_join(., pop.w.75to80, by = 'year') %>% mutate (pop.65=pop.x+pop.y+pop) %>% dplyr::select (year, pop.65)
pop.w.80plus <- left_join(pop.w.65plus, pop.w.65to80, by = 'year') %>% mutate (pop.80 = pop - pop.65) %>% dplyr::select (year, pop.80)

# Get COVID-19 deaths -------------------------------------------------
# First get aggregated time series from ECDC
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
dt <- read.csv(tf)
nl <- dt %>%
  filter (countriesAndTerritories=='Netherlands') %>%
  mutate (date = as.Date(dateRep, "%d/%m/%Y"),
          year = as.numeric(as.character(substr(year_week, 1, 4))),
          week = as.numeric(as.character(substr(year_week, 6, 7))),
          covid = deaths_weekly,
          index = paste0(year, '.', week)
  ) %>%
  dplyr::select(date, year, week, index, covid) %>%
  filter (date < '2021-01-01') %>%
  arrange (year, week)

## Now get the longer non-disaggregated time series from NL
nl.corona.longer<-read.csv('https://raw.githubusercontent.com/J535D165/CoronaWatchNL/master/data/rivm_NL_covid19_national.csv')  
nl.corona.longer <- nl.corona.longer %>%
  filter (Type == 'Overleden') %>%
  mutate (date = as.Date(Datum, "%Y-%m-%d")) %>%
  dplyr::select (date, Aantal)

nl.corona.longer$covid.all <- NA
for ( i in 2:nrow(nl.corona.longer)){
  nl.corona.longer$covid.all[i] <- nl.corona.longer$Aantal[i] - nl.corona.longer$Aantal[i-1]
}

nl.corona.longer <- nl.corona.longer %>% dplyr::select(-Aantal)

## Get the file with disaggregated data  
nl.corona<- read.csv('https://raw.githubusercontent.com/J535D165/CoronaWatchNL/master/data-desc/data-deceased/RIVM_NL_deceased_age_sex.csv')

# First get by sex
nl.corona.m <- nl.corona %>%
  filter (Geslacht=='Man') %>%
  group_by(Datum) %>%
  summarise (covid.m = sum(Aantal)) %>%
  dplyr::select(Datum, covid.m)

nl.corona.w <- nl.corona %>%
  filter (Geslacht=='Vrouw') %>%
  group_by(Datum) %>%
  summarise (covid.w = sum(Aantal)) %>%
  dplyr::select(Datum, covid.w)

nl.corona.sex <- left_join(nl.corona.m, nl.corona.w, by='Datum')
nl.corona.sex$date <- as.Date(nl.corona.sex$Datum, "%Y-%m-%d") 
nl.corona.sex <- left_join (nl.corona.longer, nl.corona.sex, by = 'date') %>% dplyr::select(-Datum)

nl.corona.sex <-read.zoo (nl.corona.sex)
my.colsum<-function (x) {colSums(x, na.rm=T)}
nl.corona.sex <- apply.weekly(nl.corona.sex, my.colsum)

# add missing data
for (i in 1:7){
  nl.corona.sex$covid.m[i] = round(nl.corona.sex$covid.all[i]*0.6,0)
  nl.corona.sex$covid.w[i] = round(nl.corona.sex$covid.all[i]*0.4,0)
}

nl.corona.sex <- nl.corona.sex %>%
  transform (#date = as.character(attributes(.)$index),
    covid.all = as.numeric(as.character(covid.all)),
    covid.m = as.numeric(as.character(covid.m)),
    covid.w = as.numeric(as.character(covid.w)),
    index = paste0(as.numeric(as.character(strftime(.,format = "%Y"))), '.', trimws(as.numeric(as.character(strftime(.,format = "%V")))))
  )

# Merge with ECDC data
nl.all<-left_join(nl, nl.corona.sex, by='index', copy=TRUE) %>% replace(is.na(.), 0)


###  AGE
nl.corona.age <- nl.corona %>%
  mutate (age = case_when(LeeftijdGroep == '80-84' | LeeftijdGroep == '85-89' | LeeftijdGroep == '90-94' |LeeftijdGroep == '95+' ~ '80+',
                          LeeftijdGroep == '75-79' | LeeftijdGroep == '70-74' | LeeftijdGroep == '65-69'  ~ '65to80',
                          TRUE ~ '0to65'))
nl.corona.age<-nl.corona.age %>% 
  dplyr::select(-LeeftijdGroep, -AantalCumulatief, -Geslacht) %>%
  group_by(Datum, age) %>%
  summarize (covid = sum(Aantal)) %>%
  pivot_wider(values_from=covid, names_from=age) %>%
  mutate (date = as.Date(Datum, "%Y-%m-%d")) %>%
  ungroup() %>%
  dplyr::select (.,-Datum)

colnames(nl.corona.age)<- c('covid.0to65','covid.65to80','covid.80plus','date')

nl.corona.age <- left_join (nl.corona.longer, nl.corona.age, by = 'date') 

nl.corona.age <-read.zoo (nl.corona.age)
nl.corona.age <- apply.weekly(nl.corona.age, my.colsum)

# add missing data
for (i in 1:7){
  nl.corona.age$covid.0to65[i] = round(nl.corona.age$covid.all[i]*0.06,0)
  nl.corona.age$covid.65to80[i] = round(nl.corona.age$covid.all[i]*0.36,0)
  nl.corona.age$covid.80plus[i] = round(nl.corona.age$covid.all[i]*0.58,0)
}

nl.corona.age <- nl.corona.age %>%
  transform (#date = as.character(attributes(.)$index),
    covid.all = as.numeric(as.character(covid.all)),
    covid.0to65 = as.numeric(as.character(covid.0to65)),
    covid.65to80 = as.numeric(as.character(covid.65to80)),
    covid.80plus = as.numeric(as.character(covid.80plus)),
    index = paste0(as.numeric(as.character(strftime(.,format = "%Y"))), '.', trimws(as.numeric(as.character(strftime(.,format = "%V")))))
  )

# merge with the ECDC data which already has the data by sex group
nl.all<-left_join(nl.all, nl.corona.age, by='index', copy=TRUE) %>% 
  replace(is.na(.), 0) %>% 
  dplyr::select(-date, -year, -week, -covid.all.x, -covid.all.y) %>% 
  mutate_at(vars(starts_with('covid.')), funs(as.numeric(as.character(.))))

#nl.all[47,3:7]<-c(round(nl.all[47,2]*0.6,0), round(nl.all[47,2]*0.4,0), round(nl.all[47,2]*0.06,0), round(nl.all[47,2]*0.36,0), round(nl.all[47,2]*0.58,0)) 
# Get weather data --------------------------------------------------------
library(rnoaa)
options(noaakey = "hKkStiPTZiDOsqQfyMCLoFgloksNDMCw")
# make a function to download per year
get_weather <- function (year, datatype){
  temp <-ncdc(datasetid='GHCND', stationid='GHCND:NLM00006235', datatypeid=datatype,
              startdate = paste0(year,'-01-01'), enddate = paste0(year,'-12-31'),limit=732)}

# apply the function for all years between 1995 and 2020
for (i in 2008:2020){
  assign(paste0('nlw', i), get_weather (year = i, datatype = c('TMIN', 'TMAX')))
}

# merge all yearly datasets
nlw <- ncdc_combine(nlw2008, nlw2009, nlw2010, nlw2011, nlw2012, nlw2013, nlw2014, nlw2015, nlw2016, nlw2017, nlw2018, nlw2019, nlw2020)

# make new variables
nlw <- nlw$data %>% 
  dplyr::select (date, value, datatype) %>%
  mutate (date = substr(date, 1, 10),
          temp = value/10) %>%
  dplyr::select (-value) %>%
  spread(datatype, temp)

# aggregate at week level
nlw.w <- nlw %>%
  read.zoo()  %>%
  apply.weekly(mean, na.rm=T) %>%
  transform (year = as.numeric(as.character(strftime(.,format = "%Y"))),
             week = as.numeric(as.character(strftime(.,format = "%V"))),
             date = as.character(attributes(.)$index))

# manually clean up some week nubmer issues
nlw.w$id <- paste0(nlw.w$year, '.', trimws(nlw.w$week))
nlw.w$id <- ifelse (nlw.w$date =='2017-01-01', '2016.52',nlw.w$id)
nlw.w$id <- ifelse (nlw.w$date =='2012-01-01', '2011.52',nlw.w$id)
nlw.w$id <- ifelse (nlw.w$date =='2011-01-02', '2010.52',nlw.w$id)

nlw.w <- nlw.w[nlw.w$week!=53,]

# transform variables 
nlw.w  = data.frame(nlw.w) %>% 
  mutate (temp.max = round(as.numeric(as.character(TMAX)), 2),
          temp.min = round(as.numeric(as.character(TMIN)), 2),
          t.min = case_when (temp.min>0 ~ 0, TRUE ~ 0-temp.min),
          t.max = case_when (temp.max<20 ~ 0, TRUE ~temp.max-20),
          index = id
  ) %>% 
  dplyr::select (index, temp.max, temp.min, t.min, t.max) 

# Merge, save and subset the data  -------------------------------------------------
# General dataset
d <- left_join (deaths, pop, by = 'year') %>%  # add total population
  left_join(., pop.80plus, by='year') %>%  # add population above 80
  left_join(., pop.65to80, by='year') %>%  # add population 65 to 80
  left_join(., nlw.w, by='index') %>% 
  left_join(., nl.all, by = 'index') %>%
  mutate (deaths.pc = deaths / pop, # calculate deaths per capita
          share.80 = pop.80/pop*100, # calculate share of 80+
          share.65 = pop.65/pop*100, # calculate share of 65 to 80
          share.65plus = share.80 + share.65, # calculate share of 65+
          counter = year-2009 # a counter variable for the year
  )

write_csv(d, './data/nl_mortality_pop_covid_weather2020.csv')
save(d, file = './data/nl_mortality_pop_covid_weather2020.RData')

# Gender-specific data
d.m <- left_join (deaths.m, pop.m, by = 'year') %>%  # add total population
  left_join(., pop.m.80plus, by='year') %>%  # add population above 80
  left_join(., pop.m.65to80, by='year') %>%  # add population 65 to 80
  left_join(., nlw.w, by='index') %>% 
  left_join(., nl.all, by = 'index') %>%
  mutate (deaths.pc = deaths / pop, # calculate deaths per capita
          share.80 = pop.80/pop*100, # calculate share of 80+
          share.65 = pop.65/pop*100, # calculate share of 65 to 80
          share.65plus = share.80 + share.65, # calculate share of 65+
          counter = year-2009 # a counter variable for the year
  )

write_csv(d.m, './data/nl_men_mortality_pop_covid_weather2020.csv')
save(d.m, file = './data/nl_men_mortality_pop_covid_weather2020.RData')

d.w <- left_join (deaths.w, pop.w, by = 'year') %>%  # add total population
  left_join(., pop.w.80plus, by='year') %>%  # add population above 80
  left_join(., pop.w.65to80, by='year') %>%  # add population 65 to 80
  left_join(., nlw.w, by='index') %>% 
  left_join(., nl.all, by = 'index') %>%
  mutate (deaths.pc = deaths / pop, # calculate deaths per capita
          share.80 = pop.80/pop*100, # calculate share of 80+
          share.65 = pop.65/pop*100, # calculate share of 65 to 80
          share.65plus = share.80 + share.65, # calculate share of 65+
          counter = year-2009 # a counter variable for the year
  )

write_csv(d.w, './data/nl_women_mortality_pop_covid_weather2020.csv')
save(d.w, file = './data/nl_women_mortality_pop_covid_weather2020.RData')

# Age-specific data
d.80plus <- left_join (deaths.80plus, pop.80plus, by = 'year') %>%  # add total population
  left_join(., pop.m.80plus, by='year') %>%  # add population above 80
  left_join(., nlw.w, by='index') %>% 
  left_join(., nl.all, by = 'index') %>%
  mutate (deaths.pc = deaths / pop.80.x, # calculate deaths per capita
          share.m = pop.80.y/pop.80.x*100, # calculate share of 80+
          counter = year-2009 # a counter variable for the year
  )

write_csv(d.80plus, './data/nl_80_mortality_pop_covid_weather2020.csv')
save(d.80plus, file = './data/nl_80_mortality_pop_covid_weather2020.RData')

d.65to80 <- left_join (deaths.65to80, pop.65to80, by = 'year') %>%  # add total population
  left_join(., pop.m.65to80, by='year') %>%  # add population above 80
  left_join(., nlw.w, by='index') %>% 
  left_join(., nl.all, by = 'index') %>%
  mutate (deaths.pc = deaths / pop.65.x, # calculate deaths per capita
          share.m = pop.65.y/pop.65.x*100, # calculate share of 80+
          counter = year-2009 # a counter variable for the year
  )

write_csv(d.65to80, './data/nl_65_mortality_pop_covid_weather2020.csv')
save(d.65to80, file = './data/nl_65_mortality_pop_covid_weather2020.RData')

d.0to65 <- left_join (deaths.0to65, pop.0to65, by = 'year') %>%  # add total population
  left_join(., pop.m.0to65, by='year') %>%  # add population above 80
  left_join(., nlw.w, by='index') %>% 
  left_join(., nl.all, by = 'index') %>%
  mutate (deaths.pc = deaths / pop, # calculate deaths per capita
          share.m = pop.0to65/pop*100, # calculate share of 80+
          counter = year-2009 # a counter variable for the year
  )

write_csv(d.0to65, './data/nl_0_mortality_pop_covid_weather2020.csv')
save(d.0to65, file = './data/nl_0_mortality_pop_covid_weather2020.RData')


### THE END