---
title: "Lab 7 Homework"
author: "Your Name Here"
date: "2024-02-03"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---



## Instructions
Answer the following questions and complete the exercises in RMarkdown. Please embed all of your code and push your final work to your repository. Your final lab report should be organized, clean, and run free from errors. Remember, you must remove the `#` for the included code chunks to run. Be sure to add your name to the author header above.  

Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  

## Load the libraries

```r
library(tidyverse)
library(janitor)
library(skimr)
```

For this assignment we are going to work with a large data set from the [United Nations Food and Agriculture Organization](http://www.fao.org/about/en/) on world fisheries. These data are pretty wild, so we need to do some cleaning. First, load the data.  

Load the data `FAO_1950to2012_111914.csv` as a new object titled `fisheries`.

```r
fisheries <- readr::read_csv(file = "data/FAO_1950to2012_111914.csv")
```

```
## Rows: 17692 Columns: 71
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (69): Country, Common name, ISSCAAP taxonomic group, ASFIS species#, ASF...
## dbl  (2): ISSCAAP group#, FAO major fishing area
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

1. Do an exploratory analysis of the data (your choice). What are the names of the variables, what are the dimensions, are there any NA's, what are the classes of the variables?  

```r
dim(fisheries)
```

```
## [1] 17692    71
```

```r
names(fisheries)
```

```
##  [1] "Country"                 "Common name"            
##  [3] "ISSCAAP group#"          "ISSCAAP taxonomic group"
##  [5] "ASFIS species#"          "ASFIS species name"     
##  [7] "FAO major fishing area"  "Measure"                
##  [9] "1950"                    "1951"                   
## [11] "1952"                    "1953"                   
## [13] "1954"                    "1955"                   
## [15] "1956"                    "1957"                   
## [17] "1958"                    "1959"                   
## [19] "1960"                    "1961"                   
## [21] "1962"                    "1963"                   
## [23] "1964"                    "1965"                   
## [25] "1966"                    "1967"                   
## [27] "1968"                    "1969"                   
## [29] "1970"                    "1971"                   
## [31] "1972"                    "1973"                   
## [33] "1974"                    "1975"                   
## [35] "1976"                    "1977"                   
## [37] "1978"                    "1979"                   
## [39] "1980"                    "1981"                   
## [41] "1982"                    "1983"                   
## [43] "1984"                    "1985"                   
## [45] "1986"                    "1987"                   
## [47] "1988"                    "1989"                   
## [49] "1990"                    "1991"                   
## [51] "1992"                    "1993"                   
## [53] "1994"                    "1995"                   
## [55] "1996"                    "1997"                   
## [57] "1998"                    "1999"                   
## [59] "2000"                    "2001"                   
## [61] "2002"                    "2003"                   
## [63] "2004"                    "2005"                   
## [65] "2006"                    "2007"                   
## [67] "2008"                    "2009"                   
## [69] "2010"                    "2011"                   
## [71] "2012"
```

```r
class(fisheries)
```

```
## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"
```

```r
anyNA(fisheries)
```

```
## [1] TRUE
```


2. Use `janitor` to rename the columns and make them easier to use. As part of this cleaning step, change `country`, `isscaap_group_number`, `asfis_species_number`, and `fao_major_fishing_area` to data class factor. 

```r
fisheries <- clean_names(fisheries)
fisheries <- fisheries %>% 
  mutate_if(is.character, factor)
fisheries
```

```
## # A tibble: 17,692 × 71
##    country common_name               isscaap_group_number isscaap_taxonomic_gr…¹
##    <fct>   <fct>                                    <dbl> <fct>                 
##  1 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  2 Albania Atlantic bonito                             36 Tunas, bonitos, billf…
##  3 Albania Barracudas nei                              37 Miscellaneous pelagic…
##  4 Albania Blue and red shrimp                         45 Shrimps, prawns       
##  5 Albania Blue whiting(=Poutassou)                    32 Cods, hakes, haddocks 
##  6 Albania Bluefish                                    37 Miscellaneous pelagic…
##  7 Albania Bogue                                       33 Miscellaneous coastal…
##  8 Albania Caramote prawn                              45 Shrimps, prawns       
##  9 Albania Catsharks, nursehounds n…                   38 Sharks, rays, chimaer…
## 10 Albania Common cuttlefish                           57 Squids, cuttlefishes,…
## # ℹ 17,682 more rows
## # ℹ abbreviated name: ¹​isscaap_taxonomic_group
## # ℹ 67 more variables: asfis_species_number <fct>, asfis_species_name <fct>,
## #   fao_major_fishing_area <dbl>, measure <fct>, x1950 <fct>, x1951 <fct>,
## #   x1952 <fct>, x1953 <fct>, x1954 <fct>, x1955 <fct>, x1956 <fct>,
## #   x1957 <fct>, x1958 <fct>, x1959 <fct>, x1960 <fct>, x1961 <fct>,
## #   x1962 <fct>, x1963 <fct>, x1964 <fct>, x1965 <fct>, x1966 <fct>, …
```

We need to deal with the years because they are being treated as characters and start with an X. We also have the problem that the column names that are years actually represent data. We haven't discussed tidy data yet, so here is some help. You should run this ugly chunk to transform the data for the rest of the homework. It will only work if you have used janitor to rename the variables in question 2!  

```r
fisheries_tidy <- fisheries %>% 
  pivot_longer(-c(country,common_name,isscaap_group_number,isscaap_taxonomic_group,asfis_species_number,asfis_species_name,fao_major_fishing_area,measure),
               names_to = "year",
               values_to = "catch",
               values_drop_na = TRUE) %>% 
  mutate(year= as.numeric(str_replace(year, 'x', ''))) %>% 
  mutate(catch= str_replace(catch, c(' F'), replacement = '')) %>% 
  mutate(catch= str_replace(catch, c('...'), replacement = '')) %>% 
  mutate(catch= str_replace(catch, c('-'), replacement = '')) %>% 
  mutate(catch= str_replace(catch, c('0 0'), replacement = ''))

fisheries_tidy$catch <- as.numeric(fisheries_tidy$catch)
```

3. How many countries are represented in the data? Provide a count and list their names.

```r
unique(fisheries_tidy$country)
```

```
##   [1] Albania                   Algeria                  
##   [3] American Samoa            Angola                   
##   [5] Anguilla                  Antigua and Barbuda      
##   [7] Argentina                 Aruba                    
##   [9] Australia                 Bahamas                  
##  [11] Bahrain                   Bangladesh               
##  [13] Barbados                  Belgium                  
##  [15] Belize                    Benin                    
##  [17] Bermuda                   Bonaire/S.Eustatius/Saba 
##  [19] Bosnia and Herzegovina    Brazil                   
##  [21] British Indian Ocean Ter  British Virgin Islands   
##  [23] Brunei Darussalam         Bulgaria                 
##  [25] Cabo Verde                Cambodia                 
##  [27] Cameroon                  Canada                   
##  [29] Cayman Islands            Channel Islands          
##  [31] Chile                     China                    
##  [33] China, Hong Kong SAR      China, Macao SAR         
##  [35] Colombia                  Comoros                  
##  [37] Congo, Dem. Rep. of the   Congo, Republic of       
##  [39] Cook Islands              Costa Rica               
##  [41] Croatia                   Cuba                     
##  [43] Cura\xe7ao                Cyprus                   
##  [45] C\xf4te d'Ivoire          Denmark                  
##  [47] Djibouti                  Dominica                 
##  [49] Dominican Republic        Ecuador                  
##  [51] Egypt                     El Salvador              
##  [53] Equatorial Guinea         Eritrea                  
##  [55] Estonia                   Ethiopia                 
##  [57] Falkland Is.(Malvinas)    Faroe Islands            
##  [59] Fiji, Republic of         Finland                  
##  [61] France                    French Guiana            
##  [63] French Polynesia          French Southern Terr     
##  [65] Gabon                     Gambia                   
##  [67] Georgia                   Germany                  
##  [69] Ghana                     Gibraltar                
##  [71] Greece                    Greenland                
##  [73] Grenada                   Guadeloupe               
##  [75] Guam                      Guatemala                
##  [77] Guinea                    GuineaBissau             
##  [79] Guyana                    Haiti                    
##  [81] Honduras                  Iceland                  
##  [83] India                     Indonesia                
##  [85] Iran (Islamic Rep. of)    Iraq                     
##  [87] Ireland                   Isle of Man              
##  [89] Israel                    Italy                    
##  [91] Jamaica                   Japan                    
##  [93] Jordan                    Kenya                    
##  [95] Kiribati                  Korea, Dem. People's Rep 
##  [97] Korea, Republic of        Kuwait                   
##  [99] Latvia                    Lebanon                  
## [101] Liberia                   Libya                    
## [103] Lithuania                 Madagascar               
## [105] Malaysia                  Maldives                 
## [107] Malta                     Marshall Islands         
## [109] Martinique                Mauritania               
## [111] Mauritius                 Mayotte                  
## [113] Mexico                    Micronesia, Fed.States of
## [115] Monaco                    Montenegro               
## [117] Montserrat                Morocco                  
## [119] Mozambique                Myanmar                  
## [121] Namibia                   Nauru                    
## [123] Netherlands               Netherlands Antilles     
## [125] New Caledonia             New Zealand              
## [127] Nicaragua                 Nigeria                  
## [129] Niue                      Norfolk Island           
## [131] Northern Mariana Is.      Norway                   
## [133] Oman                      Other nei                
## [135] Pakistan                  Palau                    
## [137] Palestine, Occupied Tr.   Panama                   
## [139] Papua New Guinea          Peru                     
## [141] Philippines               Pitcairn Islands         
## [143] Poland                    Portugal                 
## [145] Puerto Rico               Qatar                    
## [147] Romania                   Russian Federation       
## [149] R\xe9union                Saint Barth\xe9lemy      
## [151] Saint Helena              Saint Kitts and Nevis    
## [153] Saint Lucia               Saint Vincent/Grenadines 
## [155] SaintMartin               Samoa                    
## [157] Sao Tome and Principe     Saudi Arabia             
## [159] Senegal                   Serbia and Montenegro    
## [161] Seychelles                Sierra Leone             
## [163] Singapore                 Sint Maarten             
## [165] Slovenia                  Solomon Islands          
## [167] Somalia                   South Africa             
## [169] Spain                     Sri Lanka                
## [171] St. Pierre and Miquelon   Sudan                    
## [173] Sudan (former)            Suriname                 
## [175] Svalbard and Jan Mayen    Sweden                   
## [177] Syrian Arab Republic      Taiwan Province of China 
## [179] Tanzania, United Rep. of  Thailand                 
## [181] TimorLeste                Togo                     
## [183] Tokelau                   Tonga                    
## [185] Trinidad and Tobago       Tunisia                  
## [187] Turkey                    Turks and Caicos Is.     
## [189] Tuvalu                    US Virgin Islands        
## [191] Ukraine                   Un. Sov. Soc. Rep.       
## [193] United Arab Emirates      United Kingdom           
## [195] United States of America  Uruguay                  
## [197] Vanuatu                   Venezuela, Boliv Rep of  
## [199] Viet Nam                  Wallis and Futuna Is.    
## [201] Yemen                     Yugoslavia SFR           
## [203] Zanzibar                 
## 204 Levels: Albania Algeria American Samoa Angola ... Zanzibar
```

4. Refocus the data only to include country, isscaap_taxonomic_group, asfis_species_name, asfis_species_number, year, catch.

```r
fisheries_refocus <- fisheries_tidy %>%
  select(country,isscaap_taxonomic_group,asfis_species_name,asfis_species_number,year,catch)
fisheries_refocus
```

```
## # A tibble: 376,771 × 6
##    country isscaap_taxonomic_group asfis_species_name asfis_species_number  year
##    <fct>   <fct>                   <fct>              <fct>                <dbl>
##  1 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            1995
##  2 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            1996
##  3 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            1997
##  4 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            1998
##  5 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            1999
##  6 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            2000
##  7 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            2001
##  8 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            2002
##  9 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            2003
## 10 Albania Sharks, rays, chimaeras Squatinidae        10903XXXXX            2004
## # ℹ 376,761 more rows
## # ℹ 1 more variable: catch <dbl>
```

5. Based on the asfis_species_number, how many distinct fish species were caught as part of these data?

```r
species <- fisheries_refocus %>%
  summarize(species_distinct=n_distinct(asfis_species_number))
species
```

```
## # A tibble: 1 × 1
##   species_distinct
##              <int>
## 1             1551
```

6. Which country had the largest overall catch in the year 2000?

```r
fisheries_refocus %>%
  select(country,catch,year) %>%
  filter(year=="2000") %>%
  arrange(desc(catch))
```

```
## # A tibble: 8,793 × 3
##    country                  catch  year
##    <fct>                    <dbl> <dbl>
##  1 China                     9068  2000
##  2 Peru                      5717  2000
##  3 Russian Federation        5065  2000
##  4 Viet Nam                  4945  2000
##  5 Chile                     4299  2000
##  6 China                     3288  2000
##  7 China                     2782  2000
##  8 United States of America  2438  2000
##  9 China                     1234  2000
## 10 Philippines                999  2000
## # ℹ 8,783 more rows
```
China

7. Which country caught the most sardines (_Sardina pilchardus_) between the years 1990-2000?

```r
fisheries_refocus %>%
  group_by(country) %>%
  filter(asfis_species_name=="Sardina pilchardus") %>%
  arrange(desc(catch))
```

```
## # A tibble: 1,655 × 6
## # Groups:   country [46]
##    country  isscaap_taxonomic_gr…¹ asfis_species_name asfis_species_number  year
##    <fct>    <fct>                  <fct>              <fct>                <dbl>
##  1 Spain    Herrings, sardines, a… Sardina pilchardus 1210506401            1989
##  2 Un. Sov… Herrings, sardines, a… Sardina pilchardus 1210506401            1976
##  3 Morocco  Herrings, sardines, a… Sardina pilchardus 1210506401            1994
##  4 Ukraine  Herrings, sardines, a… Sardina pilchardus 1210506401            1989
##  5 Spain    Herrings, sardines, a… Sardina pilchardus 1210506401            1987
##  6 Morocco  Herrings, sardines, a… Sardina pilchardus 1210506401            1996
##  7 Russian… Herrings, sardines, a… Sardina pilchardus 1210506401            1988
##  8 Spain    Herrings, sardines, a… Sardina pilchardus 1210506401            1996
##  9 Morocco  Herrings, sardines, a… Sardina pilchardus 1210506401            1960
## 10 Morocco  Herrings, sardines, a… Sardina pilchardus 1210506401            1965
## # ℹ 1,645 more rows
## # ℹ abbreviated name: ¹​isscaap_taxonomic_group
## # ℹ 1 more variable: catch <dbl>
```
Spain

8. Which five countries caught the most cephalopods between 2008-2012?

```r
ceph <- fisheries_tidy %>%
  filter(asfis_species_name=="Cephalopoda") %>%
  filter(year>2008 & year<2012) %>%
  arrange(desc(catch))
ceph
```

```
## # A tibble: 44 × 10
##    country common_name     isscaap_group_number isscaap_taxonomic_group        
##    <fct>   <fct>                          <dbl> <fct>                          
##  1 India   Cephalopods nei                   57 Squids, cuttlefishes, octopuses
##  2 India   Cephalopods nei                   57 Squids, cuttlefishes, octopuses
##  3 China   Cephalopods nei                   57 Squids, cuttlefishes, octopuses
##  4 India   Cephalopods nei                   57 Squids, cuttlefishes, octopuses
##  5 India   Cephalopods nei                   57 Squids, cuttlefishes, octopuses
##  6 India   Cephalopods nei                   57 Squids, cuttlefishes, octopuses
##  7 Algeria Cephalopods nei                   57 Squids, cuttlefishes, octopuses
##  8 China   Cephalopods nei                   57 Squids, cuttlefishes, octopuses
##  9 Algeria Cephalopods nei                   57 Squids, cuttlefishes, octopuses
## 10 India   Cephalopods nei                   57 Squids, cuttlefishes, octopuses
## # ℹ 34 more rows
## # ℹ 6 more variables: asfis_species_number <fct>, asfis_species_name <fct>,
## #   fao_major_fishing_area <dbl>, measure <fct>, year <dbl>, catch <dbl>
```
India, China , Algeria, France, Spain

9. Which species had the highest catch total between 2008-2012? (hint: Osteichthyes is not a species)

```r
fisheries_refocus %>%
  filter(year>2008 & year<2012) %>%
  arrange(desc(catch))
```

```
## # A tibble: 30,633 × 6
##    country  isscaap_taxonomic_gr…¹ asfis_species_name asfis_species_number  year
##    <fct>    <fct>                  <fct>              <fct>                <dbl>
##  1 Viet Nam Marine fishes not ide… Osteichthyes       199XXXXXXX010         2010
##  2 Viet Nam Marine fishes not ide… Osteichthyes       199XXXXXXX010         2009
##  3 China    Miscellaneous demersa… Trichiurus leptur… 1750600302            2011
##  4 Norway   Herrings, sardines, a… Clupea harengus    1210500105            2009
##  5 China    Miscellaneous demersa… Trichiurus leptur… 1750600302            2010
##  6 Russian… Cods, hakes, haddocks  Theragra chalcogr… 1480401601            2009
##  7 Russian… Cods, hakes, haddocks  Theragra chalcogr… 1480401601            2011
##  8 Peru     Herrings, sardines, a… Engraulis ringens  1210600208            2011
##  9 Peru     Herrings, sardines, a… Engraulis ringens  1210600208            2009
## 10 United … Cods, hakes, haddocks  Theragra chalcogr… 1480401601            2011
## # ℹ 30,623 more rows
## # ℹ abbreviated name: ¹​isscaap_taxonomic_group
## # ℹ 1 more variable: catch <dbl>
```
2010

10. Use the data to do at least one analysis of your choice.

```r
tabyl(fisheries_refocus$year)
```

```
##  fisheries_refocus$year     n     percent
##                    1950  2131 0.005655955
##                    1951  2142 0.005685151
##                    1952  2191 0.005815203
##                    1953  2253 0.005979760
##                    1954  2275 0.006038150
##                    1955  2310 0.006131045
##                    1956  2361 0.006266406
##                    1957  2439 0.006473428
##                    1958  2554 0.006778653
##                    1959  2582 0.006852969
##                    1960  2676 0.007102457
##                    1961  2770 0.007351946
##                    1962  2891 0.007673096
##                    1963  2985 0.007922584
##                    1964  3343 0.008872764
##                    1965  3451 0.009159410
##                    1966  3505 0.009302733
##                    1967  3645 0.009674311
##                    1968  3729 0.009897259
##                    1969  3772 0.010011386
##                    1970  4579 0.012153271
##                    1971  4767 0.012652248
##                    1972  4943 0.013119375
##                    1973  5019 0.013321089
##                    1974  5109 0.013559961
##                    1975  5359 0.014223494
##                    1976  5515 0.014637538
##                    1977  5678 0.015070162
##                    1978  5845 0.015513402
##                    1979  5872 0.015585064
##                    1980  5945 0.015778815
##                    1981  5979 0.015869056
##                    1982  6134 0.016280446
##                    1983  6239 0.016559130
##                    1984  6383 0.016941325
##                    1985  6480 0.017198776
##                    1986  6606 0.017533197
##                    1987  6762 0.017947241
##                    1988  7199 0.019107097
##                    1989  7257 0.019261037
##                    1990  7399 0.019637923
##                    1991  7328 0.019449480
##                    1992  7257 0.019261037
##                    1993  7170 0.019030127
##                    1994  7292 0.019353931
##                    1995  7544 0.020022772
##                    1996  7702 0.020442125
##                    1997  7919 0.021018072
##                    1998  8113 0.021532974
##                    1999  8427 0.022366371
##                    2000  8793 0.023337783
##                    2001  9046 0.024009279
##                    2002  9102 0.024157910
##                    2003  9309 0.024707316
##                    2004  9715 0.025784893
##                    2005  9870 0.026196284
##                    2006  9993 0.026522742
##                    2007 10103 0.026814696
##                    2008 10025 0.026607674
##                    2009 10119 0.026857163
##                    2010 10193 0.027053568
##                    2011 10321 0.027393297
##                    2012 10356 0.027486192
```

## Push your final code to GitHub!
Please be sure that you check the `keep md` file in the knit preferences.   
```
