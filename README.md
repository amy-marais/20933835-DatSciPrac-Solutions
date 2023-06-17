#Purpose This is the root readme and details how I approached each
question in the data science practical exam. Every question is in its
own folder that contains the data and code relevant to that question.
The data is not commited to github.

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
```

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 513813 27.5    1145915 61.2   644242 34.5
    ## Vcells 884379  6.8    8388608 64.0  1634861 12.5

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0     ✔ purrr   1.0.1
    ## ✔ tibble  3.1.8     ✔ dplyr   1.1.0
    ## ✔ tidyr   1.3.0     ✔ stringr 1.5.0
    ## ✔ readr   2.1.4     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

# Question 1

This question is about the Covid-19 Pandemic. I first want to compare
different regions’ progression through the pandemic, hopefully comparing
statistics like cases, deaths and/or vaccinations. Then, I look at how
certain population characteristics (like smoking) affected pandemic
progression. And, lastly, I consider whether increasing hospital
capacity leads or lags ICU admissions.

Let’s see what information the dataset contains.

## Data Exploration

### Dataset 1: covid_data_description

``` r
# import dataset 1/3 
library(readr)
covid_data_description <- read_csv("C:/DatSci/Solution/20933835/Question1/data/Covid/covid_data_description.csv")
```

    ## Rows: 67 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): column, source, category, description
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# let's look at the first few entries 
head(covid_data_description)
```

    ## # A tibble: 6 × 4
    ##   column      source                                        category description
    ##   <chr>       <chr>                                         <chr>    <chr>      
    ## 1 iso_code    International Organization for Standardizati… Others   ISO 3166-1…
    ## 2 continent   Our World in Data                             Others   Continent …
    ## 3 location    Our World in Data                             Others   Geographic…
    ## 4 date        Our World in Data                             Others   Date of ob…
    ## 5 total_cases COVID-19 Data Repository by the Center for S… Confirm… Total conf…
    ## 6 new_cases   COVID-19 Data Repository by the Center for S… Confirm… New confir…

``` r
# This is a dataset of the descriptors used in 'owid_covid_data' 
```

This dataset contains descriptions of the columns in ‘owid_covid_data’
and I cannot do any interesting data manipulations with it.

Let’s load the next dataset.

### Dataset 2: owid_covid_data

``` r
# import dataset 2/3 
library(readr)
owid_covid_data <- read_csv("C:/DatSci/Solution/20933835/Question1/data/Covid/owid-covid-data.csv")
```

    ## Rows: 194260 Columns: 67
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (4): iso_code, continent, location, tests_units
    ## dbl  (62): total_cases, new_cases, new_cases_smoothed, total_deaths, new_dea...
    ## date  (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# let's look at the first few entries 
head(owid_covid_data)
```

    ## # A tibble: 6 × 67
    ##   iso_code continent location    date       total_cases new_cases
    ##   <chr>    <chr>     <chr>       <date>           <dbl>     <dbl>
    ## 1 AFG      Asia      Afghanistan 2020-02-24           5         5
    ## 2 AFG      Asia      Afghanistan 2020-02-25           5         0
    ## 3 AFG      Asia      Afghanistan 2020-02-26           5         0
    ## 4 AFG      Asia      Afghanistan 2020-02-27           5         0
    ## 5 AFG      Asia      Afghanistan 2020-02-28           5         0
    ## 6 AFG      Asia      Afghanistan 2020-02-29           5         0
    ## # ℹ 61 more variables: new_cases_smoothed <dbl>, total_deaths <dbl>,
    ## #   new_deaths <dbl>, new_deaths_smoothed <dbl>, total_cases_per_million <dbl>,
    ## #   new_cases_per_million <dbl>, new_cases_smoothed_per_million <dbl>,
    ## #   total_deaths_per_million <dbl>, new_deaths_per_million <dbl>,
    ## #   new_deaths_smoothed_per_million <dbl>, reproduction_rate <dbl>,
    ## #   icu_patients <dbl>, icu_patients_per_million <dbl>, hosp_patients <dbl>,
    ## #   hosp_patients_per_million <dbl>, weekly_icu_admissions <dbl>, …

The data has many columns, which is not suitable to use in the
tidyverse. I tidy the data by making a ‘Metric’ column that contains all
the descriptors and names of metrics that were previously column names,
and a ‘Measure’ column, that contains the value of measurement
corresponding to the metric. I try to use this format and description
throughout the project.

``` r
library(lubridate)
```

    ## Loading required package: timechange

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
tidy_owid <- owid_covid_data %>% 
    gather(Metric, Measure, -date, -continent) %>% 
    na.omit() %>% #I want to delete rows with missing observations
    mutate(Year_Month = format(date, "%Y%B")) #I will probably want to work with data in months, not days, so I add another column for the 'year and month' to my tidy data. 
```

My computor cannot handle processing the tidy dataset so I need to make
a smaller dataset so that my computor doesn’t crash. I want to compare
total cases and deaths by region.

``` r
regional_covid <- owid_covid_data %>% 
    group_by(continent, date) %>% 
    select(total_cases, total_deaths) %>% 
    na.omit %>% 
    ungroup
```

    ## Adding missing grouping variables: `continent`, `date`

``` r
#check to see if we have all the continents and the spelling is correct
regional_covid %>% pull(continent) %>% unique
```

    ## [1] "Asia"          "Europe"        "Africa"        "North America"
    ## [5] "South America" "Oceania"

``` r
# we see we are missing antarctica, may have been na values that we omited?

# we can make this df more tidy 
regional_covid <- regional_covid %>% 
    gather(Metric, Measurement, -continent, -date)

#let's get total cases and deaths in each region
regional_covid <- regional_covid %>% 
    group_by(continent, Metric) %>% 
    mutate(Total_Region = sum(Measurement)) %>% 
    ungroup
```

``` r
# i want to plot the total cases   
dfplotcases <- regional_covid %>% filter( Metric == "total_cases") %>% arrange(date)

# let's plot 
g <- dfplotcases %>% 
# Initialize the canvas:
ggplot() + 
# Now add the aesthetics:
geom_line(aes(x = date, y = Measurement, color = continent), alpha = 0.8 , linewidth = 1)

#let's adjust the labels and theme
g <- g + theme_bw() + theme(legend.position = "right") + labs(x = "Date", y= "Total Cases", title = "Total Covid Cases per Continent")

print(g)
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
# save the figure 
ggsave("C:/DatSci/Solution/20933835/Question1/Figures/Figure1.png", g, width = 6, height = 6, device = "png")
```

The graphs are cumulative… it may be better to look at “new cases”.
Again, I need to make a small dataset otherwise my computer will crash.

``` r
new_covid_data <- owid_covid_data %>% 
    group_by(continent, date) %>% 
    select(contains("new")) %>% 
    na.omit %>% 
    arrange(date) %>% 
    ungroup
```

    ## Adding missing grouping variables: `continent`, `date`

``` r
small_new_covid_data <- new_covid_data %>% 
    select(date, continent, new_cases_per_million, new_deaths_per_million) %>% arrange(date) %>% 
    gather(Metric, Measurement, -date, -continent)
```

Let’s plot the new cases per million

``` r
dfplot <- small_new_covid_data %>% filter( Metric == "new_cases_per_million") %>% arrange(date)
   
g2 <- dfplot%>% 
# Initialize the canvas:
ggplot() + 
# Now add the aesthetics:
geom_line(aes(x = date, y = Measurement, color = continent), alpha = 0.8 , linewidth = 1) 

#let's split the graphs up per country 
g2 <- g2 + facet_wrap(~continent)

#let's adjust the labels and theme
g2 <- g2 + theme_bw() + theme(legend.position = "right") + labs(x = "Date", y= "New Cases Per Million", title = "New Covid Cases per Continent")

print(g2)
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
# save the figure 
ggsave("C:/DatSci/Solution/20933835/Question1/Figures/Figure2.png", g2, width = 6, height = 6, device = "png")
```

Let’s also graph the new deaths per million for each continent

``` r
#I want to plot new deaths per million    
dfplotnewdeaths <- small_new_covid_data  %>% filter( Metric == "new_deaths_per_million") %>% arrange(date)
   
g3 <- dfplotnewdeaths%>% 
# Initialize the canvas:
ggplot() + 
# Now add the aesthetics:
geom_line(aes(x = date, y = Measurement, color = continent), alpha = 0.8 , linewidth = 1) 

#let's split the graphs up per continent 
g3 <- g3 + facet_wrap(~continent)

#let's adjust the labels and theme
g3 <- g3 + theme_bw() + theme(legend.position = "bottom") + labs(x = "", y= "New Deaths per Million", title = "New Covid Deaths per Continent")

print(g3)
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
# save the figure 
ggsave("C:/DatSci/Solution/20933835/Question1/Figures/Figure3.png", g3, width = 6, height = 6, device = "png")
```

Let’s get total deaths and cases on one graph for each continent

``` r
pacman::p_load(lubridate)

dfplotnewdeaths <- small_new_covid_data  %>% filter( Metric == "new_deaths_per_million") %>% arrange(date) 

g4 <- g2 + geom_line(data = dfplotnewdeaths, aes(date, Measurement, color=continent), colour = "steelblue", alpha = 0.3) + geom_label(data = dfplotnewdeaths %>% 
    filter(date == last(date)), aes(date, Measurement, label = continent), 
    size = 3, alpha = 0.1) + 
    geom_label(data = dfplot %>% filter(date == last(date)), 
    aes(date, Measurement, label = continent), size = 3, alpha = 0.1)

print(g4)
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
# save the figure 
ggsave("C:/DatSci/Solution/20933835/Question1/Figures/Figure4.png", g4, width = 6, height = 6, device = "png")
```

I didn’t quite get that to work… Let’s move on

Other covid questions: how does poverty/ smoking/ elderly pop affect
patterns in severity

How did increasing hospitalisations lag or lead icu admissions (plot
hosp and icu admissions on one graph to show lag or lead?)

``` r
#let's collect the garbage, my computer is struggling
gc()
```

    ##            used  (Mb) gc trigger (Mb)  max used (Mb)
    ## Ncells  2172589 116.1    4043660  216   4043660  216
    ## Vcells 59136253 451.2  141160170 1077 141160170 1077

### Dataset 3: Deaths_by_cause

‘Deaths_by_cause’ provide additional insights into the causes of death
for before the Covid outbreak. Let’s investigate the dataset.

``` r
# import dataset 3/3 
library(readr)
Deaths_by_cause <- read_csv("C:/DatSci/Solution/20933835/Question1/data/Covid/Deaths_by_cause.csv")
```

    ## Rows: 7273 Columns: 36
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): Entity, Code, Number of executions (Amnesty International)
    ## dbl (33): Year, Deaths - Meningitis - Sex: Both - Age: All Ages (Number), De...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#let's look at the first few entries 
head(Deaths_by_cause)
```

    ## # A tibble: 6 × 36
    ##   Entity      Code   Year Number of executions (Amnesty…¹ Deaths - Meningitis …²
    ##   <chr>       <chr> <dbl> <chr>                                            <dbl>
    ## 1 Afghanistan AFG    2007 15                                                2933
    ## 2 Afghanistan AFG    2008 17                                                2731
    ## 3 Afghanistan AFG    2009 0                                                 2460
    ## 4 Afghanistan AFG    2011 2                                                 2327
    ## 5 Afghanistan AFG    2012 14                                                2254
    ## 6 Afghanistan AFG    2013 2                                                 2281
    ## # ℹ abbreviated names: ¹​`Number of executions (Amnesty International)`,
    ## #   ²​`Deaths - Meningitis - Sex: Both - Age: All Ages (Number)`
    ## # ℹ 31 more variables:
    ## #   `Deaths - Alzheimer's disease and other dementias - Sex: Both - Age: All Ages (Number)` <dbl>,
    ## #   `Deaths - Parkinson's disease - Sex: Both - Age: All Ages (Number)` <dbl>,
    ## #   `Deaths - Nutritional deficiencies - Sex: Both - Age: All Ages (Number)` <dbl>,
    ## #   `Deaths - Malaria - Sex: Both - Age: All Ages (Number)` <dbl>, …

I want to make this data tidy. I want one ‘Cause of Death’ column that
gathers all the different types of deaths, and a ‘Quantity’ column that
gives the number of deaths for each cause of death. I am not interested
in the deaths that have missing quantities so I delete these entries
from the tidy dataset.

``` r
tidy_Deaths_by_cause <- Deaths_by_cause %>% 
    gather(Cause_of_Death, Quantity, -Year, -Entity, -Code) %>%
    arrange(Year) %>% 
    na.omit
```

What years are the data for?

``` r
tidy_Deaths_by_cause %>% pull(Year) %>% unique
```

    ##  [1] 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004
    ## [16] 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019

I am only interested in the deaths occuring in 2019, the year before the
pandemic, so I filter out all data for dates before 2019.

``` r
deaths_2019 <- tidy_Deaths_by_cause %>% 
 filter(Year >= 2019)

#check that there are only entries for 2019 
deaths_2019 %>% pull(Year) %>% unique
```

    ## [1] 2019

Let’s check that the descriptors in ‘cause of death’ is appropriate, and
what the unique causes of death are that we’re looking at.

``` r
deaths_2019 %>% pull(Cause_of_Death) %>% unique
```

    ##  [1] "Deaths - Meningitis - Sex: Both - Age: All Ages (Number)"                                
    ##  [2] "Deaths - Alzheimer's disease and other dementias - Sex: Both - Age: All Ages (Number)"   
    ##  [3] "Deaths - Parkinson's disease - Sex: Both - Age: All Ages (Number)"                       
    ##  [4] "Deaths - Nutritional deficiencies - Sex: Both - Age: All Ages (Number)"                  
    ##  [5] "Deaths - Malaria - Sex: Both - Age: All Ages (Number)"                                   
    ##  [6] "Deaths - Drowning - Sex: Both - Age: All Ages (Number)"                                  
    ##  [7] "Deaths - Interpersonal violence - Sex: Both - Age: All Ages (Number)"                    
    ##  [8] "Deaths - Maternal disorders - Sex: Both - Age: All Ages (Number)"                        
    ##  [9] "Deaths - HIV/AIDS - Sex: Both - Age: All Ages (Number)"                                  
    ## [10] "Deaths - Drug use disorders - Sex: Both - Age: All Ages (Number)"                        
    ## [11] "Deaths - Tuberculosis - Sex: Both - Age: All Ages (Number)"                              
    ## [12] "Deaths - Cardiovascular diseases - Sex: Both - Age: All Ages (Number)"                   
    ## [13] "Deaths - Lower respiratory infections - Sex: Both - Age: All Ages (Number)"              
    ## [14] "Deaths - Neonatal disorders - Sex: Both - Age: All Ages (Number)"                        
    ## [15] "Deaths - Alcohol use disorders - Sex: Both - Age: All Ages (Number)"                     
    ## [16] "Deaths - Self-harm - Sex: Both - Age: All Ages (Number)"                                 
    ## [17] "Deaths - Exposure to forces of nature - Sex: Both - Age: All Ages (Number)"              
    ## [18] "Deaths - Diarrheal diseases - Sex: Both - Age: All Ages (Number)"                        
    ## [19] "Deaths - Environmental heat and cold exposure - Sex: Both - Age: All Ages (Number)"      
    ## [20] "Deaths - Neoplasms - Sex: Both - Age: All Ages (Number)"                                 
    ## [21] "Deaths - Conflict and terrorism - Sex: Both - Age: All Ages (Number)"                    
    ## [22] "Deaths - Diabetes mellitus - Sex: Both - Age: All Ages (Number)"                         
    ## [23] "Deaths - Chronic kidney disease - Sex: Both - Age: All Ages (Number)"                    
    ## [24] "Deaths - Poisonings - Sex: Both - Age: All Ages (Number)"                                
    ## [25] "Deaths - Protein-energy malnutrition - Sex: Both - Age: All Ages (Number)"               
    ## [26] "Deaths - Road injuries - Sex: Both - Age: All Ages (Number)"                             
    ## [27] "Deaths - Chronic respiratory diseases - Sex: Both - Age: All Ages (Number)"              
    ## [28] "Deaths - Cirrhosis and other chronic liver diseases - Sex: Both - Age: All Ages (Number)"
    ## [29] "Deaths - Digestive diseases - Sex: Both - Age: All Ages (Number)"                        
    ## [30] "Deaths - Fire, heat, and hot substances - Sex: Both - Age: All Ages (Number)"            
    ## [31] "Deaths - Acute hepatitis - Sex: Both - Age: All Ages (Number)"

There are 31 causes of death.

Let’s summarise the causes of death for each continent in a table.

``` r
# Deaths before covid:
```

# Question 2

This question is about the weather in mid parts of the UK.

I start by exploring the dataset. The data folder contains an
information document which specifies the UKMonthly_detailed column
information.

## Data Exploration

``` r
# import dataset 1/2
library(readr)
london_weather <- read_csv("C:/DatSci/Solution/20933835/Question2/data/London/london_weather.csv")
```

    ## Rows: 15341 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (10): date, cloud_cover, sunshine, global_radiation, max_temp, mean_temp...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Let's look at the first few entries 
head(london_weather)
```

    ## # A tibble: 6 × 10
    ##       date cloud_cover sunshine global_radiation max_temp mean_temp min_temp
    ##      <dbl>       <dbl>    <dbl>            <dbl>    <dbl>     <dbl>    <dbl>
    ## 1 19790101           2      7                 52      2.3      -4.1     -7.5
    ## 2 19790102           6      1.7               27      1.6      -2.6     -7.5
    ## 3 19790103           5      0                 13      1.3      -2.8     -7.2
    ## 4 19790104           8      0                 13     -0.3      -2.6     -6.5
    ## 5 19790105           6      2                 29      5.6      -0.8     -1.4
    ## 6 19790106           5      3.8               39      8.3      -0.5     -6.6
    ## # ℹ 3 more variables: precipitation <dbl>, pressure <dbl>, snow_depth <dbl>

This dataset is less detailed than the second, focusing on London, which
is not in the mid parts of the UK, but rather on the South Eastern side.
Hence, this dataset will not convince anyone that the mid-UK areas have
poor weather.

The second dataset is more detailed and focuses on the mid parts of the
UK, so we will use this dataset. It is accompanied by a pdf that defines
the column names.

``` r
# import dataset 2/2
library(readr)
UKMonthly_Detailed <- read_csv("C:/DatSci/Solution/20933835/Question2/data/London/UKMonthly_Detailed.csv")
```

    ## Rows: 1707 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (17): STATION, DATE, NAME, CDSD_ATTRIBUTES, CLDD_ATTRIBUTES, DT00_ATTRIB...
    ## dbl (17): LATITUDE, LONGITUDE, ELEVATION, CDSD, CLDD, DT00, DT32, DX32, DX70...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Let's look at the first few entries 
head(UKMonthly_Detailed)
```

    ## # A tibble: 6 × 34
    ##   STATION   DATE  LATITUDE LONGITUDE ELEVATION NAME   CDSD CDSD_ATTRIBUTES  CLDD
    ##   <chr>     <chr>    <dbl>     <dbl>     <dbl> <chr> <dbl> <chr>           <dbl>
    ## 1 UK000000… 1881…     52.4     -1.83        78 CET …     0 E                   0
    ## 2 UK000000… 1881…     52.4     -1.83        78 CET …     0 E                   0
    ## 3 UK000000… 1881…     52.4     -1.83        78 CET …     0 E                   0
    ## 4 UK000000… 1881…     52.4     -1.83        78 CET …     0 E                   0
    ## 5 UK000000… 1881…     52.4     -1.83        78 CET …     0 E                   0
    ## 6 UK000000… 1881…     52.4     -1.83        78 CET …     0 E                   0
    ## # ℹ 25 more variables: CLDD_ATTRIBUTES <chr>, DT00 <dbl>,
    ## #   DT00_ATTRIBUTES <chr>, DT32 <dbl>, DT32_ATTRIBUTES <chr>, DX32 <dbl>,
    ## #   DX32_ATTRIBUTES <chr>, DX70 <dbl>, DX70_ATTRIBUTES <chr>, DX90 <dbl>,
    ## #   DX90_ATTRIBUTES <chr>, EMNT <dbl>, EMNT_ATTRIBUTES <chr>, EMXT <dbl>,
    ## #   EMXT_ATTRIBUTES <chr>, HDSD <dbl>, HDSD_ATTRIBUTES <chr>, HTDD <dbl>,
    ## #   HTDD_ATTRIBUTES <chr>, TAVG <dbl>, TAVG_ATTRIBUTES <chr>, TMAX <dbl>,
    ## #   TMAX_ATTRIBUTES <chr>, TMIN <dbl>, TMIN_ATTRIBUTES <chr>

Based on the info doc, I don’t want any of the variables called
‘attributes’, so I drop them from the dataset.

``` r
weather <- UKMonthly_Detailed %>% 
    select(!contains("ATTRIBUTES"))
```

How many names and stations are there? I want to know which areas in the
central UK we can consider.

``` r
weather %>% pull(NAME) %>% unique
```

    ## [1] "CET CENTRAL ENGLAND, UK"

``` r
weather %>% pull(STATION) %>% unique
```

    ## [1] "UK000000000"

The weather data is all for the same station/area, so we can drop these
datapoints as they are redundant.

``` r
weather <- weather %>% select(!contains("STATION")) %>% select(!contains("NAME"))
```

Now I want to make the data tidy by collecting all columns under
‘Metric’ where their values are capture in ‘Measure.’ I want to delete
any rows with missing observations.

``` r
tidy_weather <- weather %>% 
    gather(Metric, Measure, -DATE) %>% 
    na.omit %>% 
    arrange(DATE)
```

The climate has changed over time, hence I do not want to look at the
weather going all the way back to the 1800s. Let’s only consider the
weather for the last 50 years (approximately). This means we consider
the weather from about the 1970’s onwards.

``` r
tidy_weather <- tidy_weather %>% 
    arrange(DATE) %>% 
    filter(DATE >= 1970)
```

What aspects of the weather can we consider? Let’s investigate what’s
recorded in ‘Metric’ and look up the definitions in the info doc.

``` r
tidy_weather %>% pull(Metric) %>% unique
```

    ##  [1] "LATITUDE"  "LONGITUDE" "ELEVATION" "CDSD"      "CLDD"      "DT00"     
    ##  [7] "DT32"      "DX32"      "DX70"      "DX90"      "EMNT"      "EMXT"     
    ## [13] "HDSD"      "HTDD"      "TAVG"      "TMAX"      "TMIN"

Measuring latitude, longitude, elevation is not interesting. We can look
at temperatures (extremes and averages).

It could be interesting to look at the temperetures occuring in each
season over the last 50 years. Let’s make seasons. The seasons occur in
the following months in the UK:

Spring = March - May Summer = June - August Autumn = Sep - Nov Winter =
Dec - Feb

``` r
# add a 'season' column that contains the four seasons that correspond to the month 

tidy_weather_seasons <- tidy_weather %>% 
    mutate(Season = ifelse(c(grepl("01|02|12", DATE)), "Winter", 
                           ifelse(c(grepl("03|04|05", DATE)), "Spring", 
                                  ifelse(c(grepl("06|07|08", DATE)), "Summer", 
                                         ifelse(c(grepl("09|10|11", DATE)), "Autumn", NA)))))

# Let's check that we have all the seasons and no other entries in seasons 
tidy_weather_seasons %>% pull(Season) %>% unique
```

    ## [1] "Winter" "Spring" "Summer" "Autumn"

Let’s plot the minimum temperatures for each season over the last 50
years

``` r
min_temp <- tidy_weather_seasons %>% 
    group_by(Season) %>% 
    filter (Metric == "TMIN") %>% 
    ungroup

g5 <- min_temp %>% 
    ggplot()+ 
    geom_line(aes(x=Season, y=Measure, color=Season), alpha=0.8, linewidth=1)+ 
    theme_classic()+ theme(legend.position = "right") +labs(x="Season", y="Temperature in Celsius", title = "Minimum Temperatures per Season for the last 50 Years")

print(g5)
```

![](README_files/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
# save the figure 
ggsave("C:/DatSci/Solution/20933835/Question2/Figures/Figure1.png", g5, width = 6, height = 6, device = "png")
```

This would be better as a boxplot.

``` r
min_temp <- tidy_weather_seasons %>% 
    group_by(Season) %>% 
    filter (Metric == "TMIN") %>% 
    ungroup

g6 <- min_temp %>% 
    ggplot()+ 
    geom_boxplot(aes(x=Season, y=Measure, color=Season), alpha=0.8)+ 
    theme_classic()+ theme(legend.position = "right") +labs(x="Season", y="Temperature in Celsius", title = "Summary of Minimum Temperatures per Season for the last 50 Years")

print(g6)
```

![](README_files/figure-markdown_github/unnamed-chunk-28-1.png)

``` r
# save the figure 
ggsave("C:/DatSci/Solution/20933835/Question2/Figures/Figure2.png", g6, width = 6, height = 6, device = "png")
```

Let’s make a similar boxplot of the maximum temperatures for each
season.

``` r
max_temp <- tidy_weather_seasons %>% 
    group_by(Season) %>% 
    filter (Metric == "TMAX") %>% 
    ungroup

g7 <- max_temp %>% 
    ggplot()+ 
    geom_boxplot(aes(x=Season, y=Measure, color=Season), alpha=0.8)+ 
    theme_classic()+ theme(legend.position = "right") +labs(x="Season", y="Temperature in Celsius", title = "Summary of Maximum Temperatures per Season for the last 50 Years")

print(g7)
```

![](README_files/figure-markdown_github/unnamed-chunk-29-1.png)

``` r
# save the figure 
ggsave("C:/DatSci/Solution/20933835/Question2/Figures/Figure3.png", g7, width = 6, height = 6, device = "png")
```

Let’s have a closer look at the temperatures… specifically the number of
days that the min or max temperature is above or below a specific value.

I want to use a description that is easier to understand. They are as
follows:

DT00 = no of days that min temp less -17,8 = Disgustingly Cold DT32 = no
of days that min temp less 0 = SUPER Cold DX32 = no of days that max
temps less 0 = Very Cold DX79 = no of days that max temps less 21,1 =
Cold DX90 = no of days that max temps greater 32,2 = Hot

``` r
tidy_weather_seasons <- tidy_weather_seasons %>%
    mutate(temp = ifelse(c(grepl("DX32", Metric)), "Very Cold",
                         ifelse(c(grepl("DX70", Metric)), "Cold", 
                                ifelse(c(grepl("DX90", Metric)), "Hot", 
                                       ifelse(c(grepl("DT32", Metric)), "SUPER Cold", 
                                              ifelse(c(grepl("DT00", Metric)), "Disgustingly Cold", NA))))))
```

I want to see how many cold, super cold, etc days there were last year.

``` r
#what was the temp like in the last year
temp <- tidy_weather_seasons %>% 
    group_by(temp) %>% 
    na.omit %>% 
    select(-Metric) %>%
    filter(DATE>2022-03)
    ungroup
```

    ## function (x, ...) 
    ## {
    ##     UseMethod("ungroup")
    ## }
    ## <bytecode: 0x000001e9a866d648>
    ## <environment: namespace:dplyr>

``` r
g8 <- temp %>%
    ggplot()+ 
    geom_col(aes(x=temp, y=Measure, color=temp), alpha=0.8, linewidth=1)+ 
    theme_bw() + labs(x="Weather for the Day", y="Number of Days", title = "The Weather in 2022")

print(g8)
```

![](README_files/figure-markdown_github/unnamed-chunk-31-1.png)

``` r
#as you can see, it is cold and super cold most of the time, with very few hot days  

# save the figure 
ggsave("C:/DatSci/Solution/20933835/Question2/Figures/Figure4.png", g8, width = 6, height = 6, device = "png")
```

But what is the average temperature like in each season? A boxplot of
each season can show the average min, max and median temperatures, as
well as outliers.

``` r
tavg <- tidy_weather_seasons %>% 
    group_by(Season) %>% 
    filter(Metric == "TAVG") %>% 
    ungroup 

g9 <- tavg %>%
    ggplot()+ 
    geom_boxplot(aes(x=Season, y=Measure, color=Season), alpha=0.8, linewidth=1)+
    theme_bw() + labs(x= "Season", y="Average Temperature in Celsius", title="A Summary of the Average Seasonal Temperatures")

print(g9)
```

![](README_files/figure-markdown_github/unnamed-chunk-32-1.png)

``` r
# save the figure 
ggsave("C:/DatSci/Solution/20933835/Question2/Figures/Figure5.png", g9, width = 6, height = 6, device = "png")
```

make a table with ave temps for each season

``` r
#again, let's collect the garbage before we start a new question for the sake of my poor computer
gc()
```

    ##            used  (Mb) gc trigger (Mb)  max used (Mb)
    ## Ncells  2214004 118.3    4043660  216   4043660  216
    ## Vcells 60789006 463.8  141160170 1077 141160170 1077

# Question 3

This question is about music. More specifically it is about the
longevity and musical progression of two bands; Coldplay and Metallica.

I start by exploring the datasets. Along with the data is a text file
containing the definitions used in the datasets.

## Data Exploration

### Dataset 1: Broader_Spotify_Info

``` r
# Import dataset 1/3 
library(readr)
Broader_Spotify_Info <- read_csv("C:/DatSci/Solution/20933835/Question3/data/Coldplay_vs_Metallica/Broader_Spotify_Info.csv")
```

    ## Rows: 50683 Columns: 19
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): track_id, name, artist, spotify_preview_url, spotify_id, tags, genre
    ## dbl (12): year, duration_ms, danceability, energy, key, loudness, speechines...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Let's look at the first few entries 
head(Broader_Spotify_Info)
```

    ## # A tibble: 6 × 19
    ##   track_id         name  artist spotify_preview_url spotify_id tags  genre  year
    ##   <chr>            <chr> <chr>  <chr>               <chr>      <chr> <chr> <dbl>
    ## 1 TRIOREW128F424E… Mr. … The K… https://p.scdn.co/… 09ZQ5TmUG… rock… <NA>   2004
    ## 2 TRRIVDJ128F429B… Wond… Oasis  https://p.scdn.co/… 06UfBBDIS… rock… <NA>   2006
    ## 3 TROUVHL128F426C… Come… Nirva… https://p.scdn.co/… 0keNu0t0t… rock… RnB    1991
    ## 4 TRUEIND128F9303… Take… Franz… https://p.scdn.co/… 0ancVQ9wE… rock… <NA>   2004
    ## 5 TRLNZBD128F935E… Creep Radio… https://p.scdn.co/… 01QoK9DA7… rock… RnB    2008
    ## 6 TRUMISQ128F9340… Some… The K… https://p.scdn.co/… 0FNmIQ7u4… rock… <NA>   2005
    ## # ℹ 11 more variables: duration_ms <dbl>, danceability <dbl>, energy <dbl>,
    ## #   key <dbl>, loudness <dbl>, speechiness <dbl>, acousticness <dbl>,
    ## #   instrumentalness <dbl>, liveness <dbl>, valence <dbl>, tempo <dbl>

I want to first investigate the longevity and progression of Coldplay
and Metallica, so will not manipulate the broader spotify data just yet.
However, I could filter the spotify data for each band as follows.

``` r
spotify_cold_met <- Broader_Spotify_Info %>% 
    filter("Coldplay" %in% artist)

# we want to tidy this dataset 
tidy_spotify_cold_met <- spotify_cold_met %>% 
    gather(Attribute, Measure, -year) %>% 
    na.omit
```

This is just as a reminder to myself for when I go through the README
again later.

Let’s load the other datasets.

### Dataset 2 and 3: Coldplay and Metallica

####Coldplay

Let’s import and have a look at the coldplay dataset.

``` r
# Import dataset 2/3 
library(readr)
Coldplay <- read_csv("C:/DatSci/Solution/20933835/Question3/data/Coldplay_vs_Metallica/Coldplay.csv")
```

    ## Rows: 232 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (2): name, album_name
    ## dbl  (11): duration, popularity, acousticness, danceability, energy, instrum...
    ## lgl   (1): explicit
    ## date  (1): release_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Let's look at the first few entries
head(Coldplay)
```

    ## # A tibble: 6 × 15
    ##   name         duration release_date album_name explicit popularity acousticness
    ##   <chr>           <dbl> <date>       <chr>      <lgl>         <dbl>        <dbl>
    ## 1 Planet Emoji     53.2 2021-10-15   Music Of … FALSE            62     0.887   
    ## 2 Higher Power    207.  2021-10-15   Music Of … FALSE            71     0.00643 
    ## 3 Humankind       267.  2021-10-15   Music Of … FALSE            68     0.000172
    ## 4 Star Emoji       53.0 2021-10-15   Music Of … FALSE            59     0.805   
    ## 5 Let Somebod…    242.  2021-10-15   Music Of … FALSE            76     0.193   
    ## 6 Heart           189.  2021-10-15   Music Of … FALSE            65     0.97    
    ## # ℹ 8 more variables: danceability <dbl>, energy <dbl>, instrumentalness <dbl>,
    ## #   liveness <dbl>, loudness <dbl>, speechiness <dbl>, tempo <dbl>,
    ## #   valence <dbl>

Let’s tidy the Coldplay dataset (using ‘Metric’ and ‘Measure’ in the
same way as the previous questions) and delete any rows with missing
observations.

``` r
tidy_coldplay <- Coldplay %>% 
    gather(Metric, Measure, -name, -release_date) %>% 
    arrange(release_date) %>% 
    na.omit
```

#### Metallica

Let’s import and have a look at the Metallica dataset.

``` r
# Import dataset 3/3 
library(readr)
metallica <- read_csv("C:/DatSci/Solution/20933835/Question3/data/Coldplay_vs_Metallica/metallica.csv")
```

    ## Rows: 1468 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (2): name, album
    ## dbl  (11): duration_ms, popularity, danceability, energy, loudness, speechin...
    ## date  (1): release_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Let's look at the first few entries 
head(metallica)
```

    ## # A tibble: 6 × 14
    ##   name    album duration_ms popularity release_date danceability energy loudness
    ##   <chr>   <chr>       <dbl>      <dbl> <date>              <dbl>  <dbl>    <dbl>
    ## 1 72 Sea… 72 S…      459120         71 2023-04-14          0.196  0.981    -3.86
    ## 2 Shadow… 72 S…      371880         70 2023-04-14          0.316  0.978    -3.94
    ## 3 Scream… 72 S…      330453         68 2023-04-14          0.427  0.973    -3.47
    ## 4 Sleepw… 72 S…      416067         68 2023-04-14          0.45   0.949    -4.18
    ## 5 You Mu… 72 S…      423013         67 2023-04-14          0.235  0.958    -3.07
    ## 6 Lux te… 72 S…      201760         68 2023-04-14          0.401  0.996    -3.33
    ## # ℹ 6 more variables: speechiness <dbl>, acousticness <dbl>,
    ## #   instrumentalness <dbl>, liveness <dbl>, valence <dbl>, tempo <dbl>

Let’s tidy the Metallica dataset (using ‘Metric’ and ‘Measure’ in the
same way as the previous questions) and delete any rows with missing
observations.

``` r
tidy_metal <- metallica %>% 
    gather(Metric, Measure, -name, -release_date) %>% 
    arrange(release_date) %>% 
    na.omit
```

The practical suggests making a boxplot for each band, showing the
popularity of each album using box plots. I will start my analysis by
replicating these boxplots and will handle each band separately.

Let’s start with Coldplay. The data relevant to the boxplot is
‘popularity’ and ‘album_name’

``` r
coldplay_pop <- Coldplay %>% 
    select(album_name, popularity, release_date) %>% 
    na.omit

g10 <- coldplay_pop %>% arrange(release_date) %>% 
    ggplot()+
    geom_boxplot(aes(x=album_name, y=popularity, color=album_name))+
    theme_bw() +labs(x="Album", y="Popularity", title = "Coldplay Popularity by Album") +theme(legend.position = "none")

print(g10)
```

![](README_files/figure-markdown_github/unnamed-chunk-40-1.png)

``` r
# save the figure 
ggsave("C:/DatSci/Solution/20933835/Question3/Figures/Figure1.png", g10, width = 6, height = 6, device = "png")
```

I do the same thing for metallica… but I run into an issue with knitting
here so I comment this chunk out for now… it would be nice if the issue
could be fixed

``` r
 metal_pop <- metallica %>% 
     select(album, popularity, release_date) %>% 
     na.omit
# 
# g11 <- metal_pop %>% arrange(release_date) %>% 
#     ggplot()+
#     geom_boxplot(aes(x=album, y=popularity, color=album))+
#     theme_bw() +labs(x="Album", y="Popularity", title = "Metallica Popularity by Album") +theme(legend.position = "none")
# 
# print(g11)
# 
# # save the figure 
# ggsave("C:/DatSci/Solution/20933835/Question3/Figures/Figure2.png", g11, width = 6, height = 6, device = "png")
#     
```

We want to compare the bands on a like-for-like basis. We can start by
comparing their popularity since we are already working with the
popularity metric.

I want to know how the bands popularity scores compare to each other on
average, so we I can advise music producers as to which band’s “recipe
to success” they should rather follow. We can then investigate what made
the one band more popular than the other.

### Popularity

First, let’s make functions that calculate the average popularity for
each band.

``` r
# function to return Coldplay's average popularity
mean_coldplay_popularity_calculator <- function(){
    x = coldplay_pop
    ave_pop = mean(coldplay_pop$popularity)
    
    return(print(ave_pop))
    
}

#I want to save the average popularity to use for comparison later
Coldplay_Ave_Popularity <- mean_coldplay_popularity_calculator()
```

    ## [1] 43.36207

``` r
# function to return Metallica's average popularity
mean_metallica_popularity_calculator <- function(){
    x = metal_pop
    ave_pop = mean(metal_pop$popularity)
    
    return(print(ave_pop))
    
}

#I want to save the average popularity to use for comparison later
Metallica_Ave_Popularity <- mean_metallica_popularity_calculator()
```

    ## [1] 25.34469

Now we can directly compare the bands’ average popularities. I would
like to visualise the difference in average popularity, so I make a new
data frame with the popularity scores and I tidy it so I can more easily
use ggplot2.

``` r
#I want to put both bands' average popularity scores in one data frame 
popularity = data.frame(Metallica_Pop = c(Metallica_Ave_Popularity), 
                        Coldplay_Pop = c(Coldplay_Ave_Popularity)) 

#tidy the data frame so its easier to plot 
popularity <- popularity %>% 
    gather(Band, Ave_Pop_Score)

#plot average popularity scores 
g12 <- popularity %>% 
    ggplot()+
    geom_count(aes(x=Band, y=Ave_Pop_Score, color=Band))+ 
    theme_bw() + labs(x= "Band", y= "Average Popularity", title="Average Popularity for Coldplay and Metallica") + theme(legend.position = "none")

print(g12)
```

![](README_files/figure-markdown_github/unnamed-chunk-43-1.png)

``` r
# save the figure 
ggsave("C:/DatSci/Solution/20933835/Question3/Figures/Figure3.png", g12, width = 6, height = 6, device = "png")
```

We can see that, on average, Coldplay is much more popular than
Metallica. But how much popular exactly? Let’s write a function that
will do this calculation and tell me in a sentence what the result is.

``` r
# make a function returning the percentage of how much more popular coldplay is than metallica, and round off to 2 decimal places 

percent_more_pop <- function(){
    x = Coldplay_Ave_Popularity 
    y = Metallica_Ave_Popularity
    z = round(((x-y)/x)*100, 2)
    
#I want the function to return a sentence that tells me the result 
    return(glue::glue("Coldyplay is {z} % more popular than Metallica on average"))
}

percent_more_pop()
```

    ## Coldyplay is 41.55 % more popular than Metallica on average

Why is Coldplay so much more popular?

``` r
#let's collect the garbage before we start the next question
gc()
```

    ##            used  (Mb) gc trigger (Mb)  max used (Mb)
    ## Ncells  2506965 133.9    4043660  216   4043660  216
    ## Vcells 67664771 516.3  141160170 1077 141160170 1077

# Question 4

This question is about streaming services, and what features make
streaming platforms more or less popular. The data is on Netflix only,
so I need to extrapolate what features would be good to have in a
streaming service, and what features would not be good to have in a
streaming service from one platform’s data.

Note for the column definitions: IMDb (an abbreviation of Internet Movie
Database) is an online database of information related to films,
television series, home videos, video games, and streaming content
online – including cast, production crew and personal biographies, plot
summaries, trivia, ratings, and fan and critical reviews.

I start by exploring the datasets.

## Data Exploration

### Dataset 1: credits

``` r
# Import dataset 1/2 
library(readr)
credits <- read_csv("C:/DatSci/Solution/20933835/Question4/data/netflix/credits.csv")
```

    ## Rows: 77213 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): id, name, character, role
    ## dbl (1): person_id
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Let's look at the first few entries 
head(credits)
```

    ## # A tibble: 6 × 5
    ##   person_id id      name            character               role 
    ##       <dbl> <chr>   <chr>           <chr>                   <chr>
    ## 1      3748 tm84618 Robert De Niro  Travis Bickle           ACTOR
    ## 2     14658 tm84618 Jodie Foster    Iris Steensma           ACTOR
    ## 3      7064 tm84618 Albert Brooks   Tom                     ACTOR
    ## 4      3739 tm84618 Harvey Keitel   Matthew 'Sport' Higgins ACTOR
    ## 5     48933 tm84618 Cybill Shepherd Betsy                   ACTOR
    ## 6     32267 tm84618 Peter Boyle     Wizard                  ACTOR

Let’s make it tidy, following the same method as in the preceding
questions, and deleting any rows with missing values.

``` r
tidy_credits <- credits %>% 
    gather(Metric, Measure, -name) %>% 
    arrange(name) %>% 
    na.omit
```

The credits describe which actors starred in which productions, and who
the director was, etc, which could contribute to the production’s
popularity. Keep this in mind for later. Let’s see if we can get a
measure of popularity in the other dataset to work with first.

Let’s load the other dataset.

### Dataset 2: titles

``` r
# Import dataset 2/2 
library(readr)
titles <- read_csv("C:/DatSci/Solution/20933835/Question4/data/netflix/titles.csv")
```

    ## Rows: 5806 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (8): id, title, type, description, age_certification, genres, production...
    ## dbl (7): release_year, runtime, seasons, imdb_score, imdb_votes, tmdb_popula...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Let's look at the first few entries 
head(titles)
```

    ## # A tibble: 6 × 15
    ##   id       title type  description release_year age_certification runtime genres
    ##   <chr>    <chr> <chr> <chr>              <dbl> <chr>               <dbl> <chr> 
    ## 1 ts300399 Five… SHOW  "This coll…         1945 TV-MA                  48 ['doc…
    ## 2 tm84618  Taxi… MOVIE "A mentall…         1976 R                     113 ['cri…
    ## 3 tm127384 Mont… MOVIE "King Arth…         1975 PG                     91 ['com…
    ## 4 tm70993  Life… MOVIE "Brian Coh…         1979 R                      94 ['com…
    ## 5 tm190788 The … MOVIE "12-year-o…         1973 R                     133 ['hor…
    ## 6 ts22164  Mont… SHOW  "A British…         1969 TV-14                  30 ['com…
    ## # ℹ 7 more variables: production_countries <chr>, seasons <dbl>, imdb_id <chr>,
    ## #   imdb_score <dbl>, imdb_votes <dbl>, tmdb_popularity <dbl>, tmdb_score <dbl>

We can see that this dataset contains popularity scores and imdb scores,
so we’ll work with this data first.

Let’s make it tidy, following the same method as in the preceding
questions, and deleting any rows with missing values.

``` r
tidy_titles <- titles %>% 
    gather (Metric, Measure, -release_year, -title) %>% 
    arrange(release_year) %>% 
    na.omit
```

What kind of productions are more popular? Let’s compare popularity by
age certification i.e. should streaming services upload more PG or more
R rated productions, for example. Let’s compare by tmdb popularity.

``` r
age_pop <- titles %>% 
    select(age_certification, imdb_score, tmdb_popularity) %>% 
    na.omit()

# I want the average popularity for R rated movies, so let's isolate R
R_pop <- age_pop %>% 
    group_by(age_certification) %>% 
    filter(age_certification == "R") %>% 
    ungroup

#now I can make a function that calculates the average imdb scores for R rated productions 
R_pop_ave_imdb_score <- function(){
    x= R_pop
    ave_R_imdb_score = mean(R_pop$imdb_score)
    
    return(print(ave_R_imdb_score))
    
}

#I want to save the average popularity to use for comparison later
ave_R_pop_imdb_score <- R_pop_ave_imdb_score()
```

    ## [1] 6.321168

``` r
#now I can make a function that calculates the average tmdb popularity scores for R rated productions 
R_pop_ave_tmdb_pop <- function(){
    x= R_pop
    ave_R_tmdb_pop = mean(R_pop$tmdb_popularity)
    return(print(ave_R_tmdb_pop))
    
}

#I want to save the average popularity to use for comparison later
ave_R_pop_tmdb_pop <- R_pop_ave_tmdb_pop()
```

    ## [1] 34.40999

Now I want to do the same for the other age certifications

# Question 5

This question is about… I start by exploring the dataset. \## Data
Exploration

``` r
# Import dataset 1/2 
library(readr)
googleplaystore_user_reviews <- read_csv("C:/DatSci/Solution/20933835/Question5/data/googleplay/googleplaystore_user_reviews.csv")
```

    ## Rows: 64295 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): App, Translated_Review, Sentiment
    ## dbl (2): Sentiment_Polarity, Sentiment_Subjectivity
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Let's look at the first few entries 
head(googleplaystore_user_reviews)
```

    ## # A tibble: 6 × 5
    ##   App      Translated_Review Sentiment Sentiment_Polarity Sentiment_Subjectivity
    ##   <chr>    <chr>             <chr>                  <dbl>                  <dbl>
    ## 1 10 Best… "I like eat deli… Positive                1                     0.533
    ## 2 10 Best… "This help eatin… Positive                0.25                  0.288
    ## 3 10 Best… "nan"             nan                   NaN                   NaN    
    ## 4 10 Best… "Works great esp… Positive                0.4                   0.875
    ## 5 10 Best… "Best idea us"    Positive                1                     0.3  
    ## 6 10 Best… "Best way"        Positive                1                     0.3

Let’s make it tidy

``` r
tidy_reviews <- googleplaystore_user_reviews %>% 
    gather(Attribute, Measure, -App) %>% 
    arrange(App) %>% 
    na.omit
```

Load the other dataset

``` r
# Import dataset 2/2 
library(readr)
googleplaystore <- read_csv("C:/DatSci/Solution/20933835/Question5/data/googleplay/googleplaystore.csv")
```

    ## Rows: 10054 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (11): App, Category, Size, Installs, Type, Price, Content Rating, Genres...
    ## dbl  (2): Rating, Reviews
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Let's look at the first few entries 
head(googleplaystore)
```

    ## # A tibble: 6 × 13
    ##   App        Category Rating Reviews Size  Installs Type  Price `Content Rating`
    ##   <chr>      <chr>     <dbl>   <dbl> <chr> <chr>    <chr> <chr> <chr>           
    ## 1 Photo Edi… ART_AND…    4.1     159 19M   10,000+  Free  0     Everyone        
    ## 2 U Launche… ART_AND…    4.7   87510 8.7M  5,000,0… Free  0     Everyone        
    ## 3 Sketch - … ART_AND…    4.5  215644 25M   50,000,… Free  0     Teen            
    ## 4 Pixel Dra… ART_AND…    4.3     967 2.8M  100,000+ Free  0     Everyone        
    ## 5 Paper flo… ART_AND…    4.4     167 5.6M  50,000+  Free  0     Everyone        
    ## 6 Smoke Eff… ART_AND…    3.8     178 19M   50,000+  Free  0     Everyone        
    ## # ℹ 4 more variables: Genres <chr>, `Last Updated` <chr>, `Current Ver` <chr>,
    ## #   `Android Ver` <chr>

Let’s make it tidy

``` r
tidy_playstore <- googleplaystore %>% 
    gather(Attribute, Measure, -App) %>% 
    arrange(App)
    na.omit
```

    ## function (object, ...) 
    ## UseMethod("na.omit")
    ## <bytecode: 0x000001e9a3ba8508>
    ## <environment: namespace:stats>

I ran out of time to do more data wrangling. I prioritized getting
everything to knit correctly and submitting on time.
