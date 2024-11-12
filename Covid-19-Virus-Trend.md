Investigating Covid-19-Virus-Trend
================
2024-11-11

# **This tutorial intends to investigate the Covid-19 Virus Trend in different countries**

#### **Importing necessary libraries**

    ## Warning: le package 'dplyr' a été compilé avec la version R 4.2.3

    ## 
    ## Attachement du package : 'dplyr'

    ## Les objets suivants sont masqués depuis 'package:stats':
    ## 
    ##     filter, lag

    ## Les objets suivants sont masqués depuis 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Warning: le package 'ggplot2' a été compilé avec la version R 4.2.3

    ## Warning: le package 'data.table' a été compilé avec la version R 4.2.3

    ## 
    ## Attachement du package : 'data.table'

    ## Les objets suivants sont masqués depuis 'package:dplyr':
    ## 
    ##     between, first, last

``` r
#Getting the working directory
getwd()
```

    ## [1] "D:/Dossier/Self_Training/Sofware/GIT/Covid-19_Virus_Trend"

#### **Importing data**

``` r
df_covid19 = fread("tested_worldwide.csv")
```

#### **Showing a subset of the data**

``` r
#6 First rows
head(df_covid19)
```

    ##          Date Country_Region Province_State positive active hospitalized
    ## 1: 2020-01-16        Iceland     All States        3     NA           NA
    ## 2: 2020-01-17        Iceland     All States        4     NA           NA
    ## 3: 2020-01-18        Iceland     All States        7     NA           NA
    ## 4: 2020-01-20    South Korea     All States        1     NA           NA
    ## 5: 2020-01-22  United States     All States        0     NA           NA
    ## 6: 2020-01-22  United States  Massachusetts        0     NA           NA
    ##    hospitalizedCurr recovered death total_tested daily_tested daily_positive
    ## 1:               NA        NA    NA           NA           NA             NA
    ## 2:               NA        NA    NA           NA           NA              1
    ## 3:               NA        NA    NA           NA           NA              3
    ## 4:               NA        NA    NA            4           NA             NA
    ## 5:               NA        NA     0            0           NA             NA
    ## 6:               NA        NA     0            0           NA             NA

``` r
#6 Last rows
tail(df_covid19)
```

    ##          Date Country_Region     Province_State positive active hospitalized
    ## 1: 2020-11-09      Australia Northern Territory       NA      6           NA
    ## 2: 2020-11-09      Australia         Queensland       NA      8           NA
    ## 3: 2020-11-09      Australia    South Australia       NA     19           NA
    ## 4: 2020-11-09      Australia           Tasmania       NA      0           NA
    ## 5: 2020-11-09      Australia           Victoria       NA      4           NA
    ## 6: 2020-11-09      Australia  Western Australia       NA     10           NA
    ##    hospitalizedCurr recovered death total_tested daily_tested daily_positive
    ## 1:                6        33     0        61249            0             NA
    ## 2:                8      1163     6      1265567            0             NA
    ## 3:                0       492     4       572618            0             NA
    ## 4:                0       217    13       120292            0             NA
    ## 5:                2     19522   819      3299302            0             NA
    ## 6:                0       757     9       501428            0             NA

#### **Displaying the dimension of the dataset**

``` r
dim(df_covid19)
```

    ## [1] 27641    12

``` r
#The dataset has 27641 observations and 12 columns
```

#### **Puting the columns names in a variable**

``` r
df_cols <- colnames(df_covid19)
df_cols
```

    ##  [1] "Date"             "Country_Region"   "Province_State"   "positive"        
    ##  [5] "active"           "hospitalized"     "hospitalizedCurr" "recovered"       
    ##  [9] "death"            "total_tested"     "daily_tested"     "daily_positive"

#### **Some summary statistics**

``` r
summary(df_covid19)
```

    ##       Date            Country_Region     Province_State        positive      
    ##  Min.   :2020-01-16   Length:27641       Length:27641       Min.   :      0  
    ##  1st Qu.:2020-04-27   Class :character   Class :character   1st Qu.:    635  
    ##  Median :2020-06-26   Mode  :character   Mode  :character   Median :   8044  
    ##  Mean   :2020-06-29                                         Mean   :  89042  
    ##  3rd Qu.:2020-09-02                                         3rd Qu.:  52812  
    ##  Max.   :2020-11-09                                         Max.   :9761481  
    ##                                                             NA's   :4242     
    ##      active        hospitalized   hospitalizedCurr    recovered     
    ##  Min.   :   -10   Min.   :    0   Min.   :    0.0   Min.   :     0  
    ##  1st Qu.:   118   1st Qu.:  553   1st Qu.:   37.0   1st Qu.:   476  
    ##  Median :  2332   Median : 2592   Median :  280.0   Median :  3159  
    ##  Mean   : 19030   Mean   : 7495   Mean   :  956.7   Mean   : 25775  
    ##  3rd Qu.: 15102   3rd Qu.: 8199   3rd Qu.:  808.0   3rd Qu.: 22167  
    ##  Max.   :558636   Max.   :89995   Max.   :39055.0   Max.   :811330  
    ##  NA's   :9833     NA's   :19231   NA's   :13080     NA's   :9626    
    ##      death         total_tested        daily_tested      daily_positive  
    ##  Min.   :     0   Min.   :        0   Min.   :-1243606   Min.   :-15363  
    ##  1st Qu.:     9   1st Qu.:    32191   1st Qu.:     473   1st Qu.:     7  
    ##  Median :   163   Median :   202654   Median :    3107   Median :   135  
    ##  Mean   :  3074   Mean   :  1485408   Mean   :   19085   Mean   :  1025  
    ##  3rd Qu.:  1348   3rd Qu.:   844982   3rd Qu.:   11127   3rd Qu.:   659  
    ##  Max.   :229238   Max.   :136620652   Max.   : 3760260   Max.   :128396  
    ##  NA's   :4010     NA's   :912         NA's   :1174       NA's   :4557

##### **Regarding the summary statistics, we can say that:**

- Data was collected from the 16th January 2020 to the 09th November
  2020.
- Some columns have negative values (active, daily_tested,
  daily_positive) which should require our attention.

#### **Let’s see what these records look like**

``` r
df_negative_values <-df_covid19%>% 
  filter(active <0 | daily_tested <0 | daily_positive <0) %>% 
  arrange(Date,Country_Region)
#There are 340 values in that case, 1.2%
```

#### **For simplicity, let’s remove these records to prevent baises**

``` r
df_test <-df_covid19%>% 
  filter(!(active<0 | daily_tested <0 | daily_positive <0)) %>% 
  #filter(active >=0 & daily_tested >=0 & daily_positive >=0) %>%
  arrange(Date,Country_Region)
```

#### **Exploring null values by columns**

``` r
df_covid19 %>% is.na() %>% colSums()
```

    ##             Date   Country_Region   Province_State         positive 
    ##                0                0                0             4242 
    ##           active     hospitalized hospitalizedCurr        recovered 
    ##             9833            19231            13080             9626 
    ##            death     total_tested     daily_tested   daily_positive 
    ##             4010              912             1174             4557

#### **Giving the purcentages of null values by columns**

``` r
n = dim(df_covid19)[1]
vec <- df_covid19 %>% is.na() %>% colSums()
round((vec/n)*100,2)
```

    ##             Date   Country_Region   Province_State         positive 
    ##             0.00             0.00             0.00            15.35 
    ##           active     hospitalized hospitalizedCurr        recovered 
    ##            35.57            69.57            47.32            34.83 
    ##            death     total_tested     daily_tested   daily_positive 
    ##            14.51             3.30             4.25            16.49

``` r
#Columns with no null values: Date, Country_Region,  Province_State
#Columns with more than 30% of null values: active, hospitalized, hospitalizedCur and recovered 
```

# 

# 

``` r
library(tibble)
```

    ## Warning: le package 'tibble' a été compilé avec la version R 4.2.3

``` r
glimpse(df_covid19)
```

    ## Rows: 27,641
    ## Columns: 12
    ## $ Date             <IDate> 2020-01-16, 2020-01-17, 2020-01-18, 2020-01-20, 202…
    ## $ Country_Region   <chr> "Iceland", "Iceland", "Iceland", "South Korea", "Unit…
    ## $ Province_State   <chr> "All States", "All States", "All States", "All States…
    ## $ positive         <int> 3, 4, 7, 1, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, NA, NA, NA,…
    ## $ active           <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ hospitalized     <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ hospitalizedCurr <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ recovered        <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ death            <int> NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, NA, NA…
    ## $ total_tested     <dbl> NA, NA, NA, 4, 0, 0, 0, 0, 0, 0, 27, 0, 0, 0, NA, NA,…
    ## $ daily_tested     <int> NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 5, 0, 0, 0, NA, …
    ## $ daily_positive   <int> NA, 1, 3, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, NA, NA…

#### **Let’s look at the countries in the dataset**

``` r
df_covid19$Country_Region %>% unique()
```

    ##   [1] "Iceland"                          "South Korea"                     
    ##   [3] "United States"                    "Australia"                       
    ##   [5] "United Kingdom"                   "Israel"                          
    ##   [7] "Czechia"                          "Canada"                          
    ##   [9] "Russia"                           "Armenia"                         
    ##  [11] "Poland"                           "Italy"                           
    ##  [13] "Estonia"                          "Greece"                          
    ##  [15] "Lithuania"                        "Belgium"                         
    ##  [17] "New Zealand"                      "Sweden"                          
    ##  [19] "Latvia"                           "Costa Rica"                      
    ##  [21] "Serbia"                           "Slovakia"                        
    ##  [23] "Bangladesh"                       "Turkey"                          
    ##  [25] "Kazakhstan"                       "Palestine"                       
    ##  [27] "Brazil"                           "Bolivia"                         
    ##  [29] "Grenada"                          "Spain"                           
    ##  [31] "Ukraine"                          "Germany"                         
    ##  [33] "Iran"                             "France"                          
    ##  [35] "Ireland"                          "Uruguay"                         
    ##  [37] "Egypt"                            "Singapore"                       
    ##  [39] "Netherlands"                      "Argentina"                       
    ##  [41] "Bahrain"                          "Chile"                           
    ##  [43] "Jamaica"                          "Japan"                           
    ##  [45] "Malaysia"                         "Malta"                           
    ##  [47] "Panama"                           "Peru"                            
    ##  [49] "Trinidad and Tobago"              "Finland"                         
    ##  [51] "Mexico"                           "Slovenia"                        
    ##  [53] "Austria"                          "Colombia"                        
    ##  [55] "Ecuador"                          "North Macedonia"                 
    ##  [57] "Norway"                           "Portugal"                        
    ##  [59] "South Africa"                     "Switzerland"                     
    ##  [61] "United Arab Emirates"             "Azerbaijan"                      
    ##  [63] "Belarus"                          "Bosnia and Herzegovina"          
    ##  [65] "China"                            "Croatia"                         
    ##  [67] "Hungary"                          "Indonesia"                       
    ##  [69] "Montenegro"                       "Nepal"                           
    ##  [71] "Pakistan"                         "Thailand"                        
    ##  [73] "Denmark"                          "India"                           
    ##  [75] "Kosovo"                           "Kyrgyzstan"                      
    ##  [77] "Philippines"                      "Romania"                         
    ##  [79] "Taiwan"                           "Venezuela"                       
    ##  [81] "Vietnam"                          "Barbados"                        
    ##  [83] "Scotland"                         "North Korea"                     
    ##  [85] "Albania"                          "Bulgaria"                        
    ##  [87] "Emilia-Romagna"                   "Liguria"                         
    ##  [89] "Lombardy"                         "Marche"                          
    ##  [91] "Piedmont"                         "Tuscany"                         
    ##  [93] "Veneto"                           "Nigeria"                         
    ##  [95] "Luxembourg"                       "Ghana"                           
    ##  [97] "Tunisia"                          "Cameroon"                        
    ##  [99] "Ivory Coast"                      "Kenya"                           
    ## [101] "Morocco"                          "Democratic Republic of the Congo"
    ## [103] "Uganda"                           "Burkina Faso"                    
    ## [105] "Cuba"                             "Czech Republic"                  
    ## [107] "Guinea"                           "Tanzania"                        
    ## [109] "DR Congo"                         "El Salvador"                     
    ## [111] "Qatar"                            "Malawi"                          
    ## [113] "Mozambique"                       "Myanmar"                         
    ## [115] "Cyprus"                           "Ethiopia"                        
    ## [117] "Iraq"                             "Paraguay"                        
    ## [119] "Rwanda"                           "Saudi Arabia"                    
    ## [121] "Uzbekistan"                       "Lebanon"                         
    ## [123] "Senegal"                          "Sudan"                           
    ## [125] "Northern Cyprus"                  "Mauritius"                       
    ## [127] "Oman"                             "Maldives"                        
    ## [129] "Bhutan"                           "Sri Lanka"                       
    ## [131] "Saint Lucia"                      "Afghanistan"                     
    ## [133] "Algeria"                          "Libya"                           
    ## [135] "Madagascar"                       "Faroe Islands"                   
    ## [137] "Greenland"                        "Fiji"                            
    ## [139] "Papua New Guinea"                 "Kuwait"                          
    ## [141] "Dominican Republic"               "Gabon"                           
    ## [143] "Togo"                             "Guatemala"                       
    ## [145] "Honduras"                         "Jordan"                          
    ## [147] "Namibia"

``` r
# There are 147 countries in total
```

#### \*\* The regions in the dataset\*\*

``` r
df_covid19$Province_State %>% unique()
```

    ##  [1] "All States"                   "Massachusetts"               
    ##  [3] "Washington"                   "Australian Capital Territory"
    ##  [5] "New South Wales"              "Northern Territory"          
    ##  [7] "Queensland"                   "South Australia"             
    ##  [9] "Tasmania"                     "Victoria"                    
    ## [11] "Western Australia"            "Florida"                     
    ## [13] "British Columbia"             "Ontario"                     
    ## [15] "New Jersey"                   "Nebraska"                    
    ## [17] "Indiana"                      "Virginia"                    
    ## [19] "Quebec"                       "Michigan"                    
    ## [21] "Rhode Island"                 "Vermont"                     
    ## [23] "Wisconsin"                    "Arizona"                     
    ## [25] "California"                   "Georgia"                     
    ## [27] "Hawaii"                       "Illinois"                    
    ## [29] "New Hampshire"                "New York"                    
    ## [31] "North Carolina"               "Oregon"                      
    ## [33] "South Carolina"               "Texas"                       
    ## [35] "Colorado"                     "District of Columbia"        
    ## [37] "Maryland"                     "Nevada"                      
    ## [39] "Ohio"                         "Tennessee"                   
    ## [41] "Alaska"                       "Arkansas"                    
    ## [43] "Delaware"                     "Iowa"                        
    ## [45] "Kansas"                       "Kentucky"                    
    ## [47] "Minnesota"                    "New Mexico"                  
    ## [49] "Pennsylvania"                 "West Virginia"               
    ## [51] "Alabama"                      "Connecticut"                 
    ## [53] "Idaho"                        "Louisiana"                   
    ## [55] "Maine"                        "Mississippi"                 
    ## [57] "Missouri"                     "Montana"                     
    ## [59] "North Dakota"                 "Oklahoma"                    
    ## [61] "South Dakota"                 "Utah"                        
    ## [63] "Wyoming"                      "Alberta"                     
    ## [65] "Manitoba"                     "New Brunswick"               
    ## [67] "Newfoundland and Labrador"    "Northwest Territories"       
    ## [69] "Nova Scotia"                  "Nunavut"                     
    ## [71] "Prince Edward Island"         "Repatriated travellers"      
    ## [73] "Saskatchewan"                 "Yukon"                       
    ## [75] "American Samoa"               "Guam"                        
    ## [77] "Northern Mariana Islands"     "Puerto Rico"                 
    ## [79] "Virgin Islands"               "Tokyo"                       
    ## [81] "Hong Kong"

``` r
# There are 81 provinces
```

``` r
#test<-df_covid19 %>% select(Country_Region,Province_State) %>% 
  #arrange(Country_Region) %>% distinct()
```

#### **Let’s subset the dataset into the “All States” provinces label in order to make analyses in country level**

``` r
df_covid19_cl<-df_covid19 %>% 
  filter(Province_State=="All States") %>% 
  select(-c(Province_State))
```

#### **Let’s make analyses only on daily variables**

#### **A description of all variables**

``` r
colnames(df_covid19_cl)
```

    ##  [1] "Date"             "Country_Region"   "positive"         "active"          
    ##  [5] "hospitalized"     "hospitalizedCurr" "recovered"        "death"           
    ##  [9] "total_tested"     "daily_tested"     "daily_positive"

- Date: The date
- Country_Region: The country names
- positive: The cumulative number of positive cases
- active: The number of actively cases on the day
- hospitalized: The cumulative number of hospitalized cases
- hospitalizedCurr: the daily number of hospitalized cases
- recovered: The cumulative number of recovered cases
- death: The cumulative number of dead cases
- total_tested: The cumulative number of tested cases
- daily_tested: The daily number of tests
- daily_positive: The daily number of positive cases

``` r
df_covid19_cl_d<-df_covid19_cl %>% 
  select(c(Date,Country_Region,active,hospitalizedCurr,daily_tested,daily_positive))
```

#### **Let’s aggregate the columns by countries to have the total number of cases by countries**

``` r
df_covid19_cl1 <- df_covid19_cl %>% 
  group_by(Country_Region) %>% 
  summarise(active=sum(active,na.rm = T),
            hospitalized=sum(hospitalizedCurr,na.rm = T),
           tested = sum(daily_tested,na.rm = T),
           positive = sum(daily_positive,na.rm = T))
```

#### **The top 15 countries with high number of tests**

``` r
df_covid19_cl1 %>% 
  arrange(desc(tested)) %>% 
  head(15)
```

    ## # A tibble: 15 × 5
    ##    Country_Region   active hospitalized    tested positive
    ##    <chr>             <int>        <int>     <int>    <int>
    ##  1 United States         0            0 136937092  9850413
    ##  2 India                 0            0 106267322    60959
    ##  3 Italy          17176595      2401146  17370389   934875
    ##  4 Russia          7621860            0  11319603   432269
    ##  5 Canada          1354390            0   9873530   259992
    ##  6 Australia        394222        36384   8874298        0
    ##  7 Israel                0        22726   4915043      402
    ##  8 Turkey          4025622            0   4351655   221499
    ##  9 Peru                  0            0   3578707    59497
    ## 10 Brazil                0            0   3474441    10321
    ## 11 Philippines           0            0   3195616     6630
    ## 12 Germany               0            0   2749621    29943
    ## 13 Czechia               0            0   2557224   411220
    ## 14 Bangladesh     14479558            0   2442470   420235
    ## 15 Indonesia             0            0   2038108     4785
