Automated Pitching Command Metric: Command Zone Rating (CZR)
================
David Kramer
Spring 2022

# Load Necessary Packages

``` r
#Import necessary packages
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.2

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.1     v forcats 0.5.1

    ## Warning: package 'tibble' was built under R version 4.1.2

    ## Warning: package 'readr' was built under R version 4.1.2

    ## Warning: package 'forcats' was built under R version 4.1.2

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(janitor)
```

    ## Warning: package 'janitor' was built under R version 4.1.2

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(ggridges)
```

    ## Warning: package 'ggridges' was built under R version 4.1.2

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(fastDummies)
```

    ## Warning: package 'fastDummies' was built under R version 4.1.3

``` r
library(imputeTS)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
library(xgboost)
```

    ## Warning: package 'xgboost' was built under R version 4.1.2

    ## 
    ## Attaching package: 'xgboost'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     slice

``` r
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:gridExtra':
    ## 
    ##     combine

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

## Preparing the Pre-Game Metric Data

The Command Zone Rating metric takes four sets of data sets as its
input. We will input the first set of data, the pre-game metric data for
both hitters and pitchers, in the section below.

``` r
#Import the pregame hitting and pregame pitching data sets
pregamepitching <- read.csv("C:/Users/david/Desktop/Pitcher Pre-Game Metrics.csv", header = TRUE)

pregamehitting <- read.csv("C:/Users/david/Desktop/Hitter Pre-Game Metrics.csv", header = TRUE)
```

Given the tendency for additional NA columns and rows to pollute the
uploaded CSV files, we will reduce the data sets to the columns of
interest and eliminate rows containing NA values. Will we also ensure
that the name of the “Date” column is appropriately named.

``` r
#Conduct initial cleaning to remove all NA rows and columns
pregamepitching <- pregamepitching[, 1:20]
pregamepitching <- na.omit(pregamepitching)

pregamehitting <- pregamehitting[, 1:18]
pregamehitting <- na.omit(pregamehitting)

colnames(pregamepitching)[1] = c("Date")
colnames(pregamehitting)[1] = c("Date")
```

In anticipation of final data set merging, we will distinguish pre-game
pitching metric names from pre-game hitting metric names with
appropriate suffixes:

``` r
for(i in 4: ncol(pregamepitching)){
  colnames(pregamepitching)[i] <- paste("pre_", 
                                        paste(colnames(pregamepitching)[i], "_pitch", sep =""), 
                                        sep = "")
}

for(i in 4: ncol(pregamehitting)){
  colnames(pregamehitting)[i] <- paste("pre_", 
                                       paste(colnames(pregamehitting)[i], "_hit", sep =""),
                                       sep = "")
}

pregamepitching <- separate(pregamepitching, 
                            "pre_w.l_pitch", 
                            into = c("pre_w_pitch", "pre_l_pitch"))
head(pregamepitching)
```

    ##        Date Number                Player pre_ERA_pitch pre_w_pitch pre_l_pitch
    ## 1 3/25/2022     28 John Michael Bertrand          1.72           4           0
    ## 2 3/25/2022     23         Austin Temple          2.42           2           0
    ## 3 3/25/2022     17          Aidan Tyrell          3.86           3           0
    ## 4 3/25/2022     24          Jack Findlay          0.00           2           0
    ## 5 3/25/2022     41       Jackson Dennies          0.00           0           0
    ## 6 3/25/2022     33        Radek Birkholz          1.93           0           0
    ##   pre_app_pitch pre_gs_pitch pre_cg_pitch pre_sho_pitch pre_sv_pitch
    ## 1             5            5            0           0/0            0
    ## 2             5            4            0           0/1            1
    ## 3             5            5            0           0/0            0
    ## 4             2            2            0           0/0            0
    ## 5             3            0            0           0/0            0
    ## 6             5            0            0           0/0            0
    ##   pre_ip_pitch pre_h_pitch pre_r_pitch pre_er_pitch pre_bb_pitch pre_so_pitch
    ## 1         31.1          23           8            6            9           33
    ## 2         26.0          13           7            7            9           34
    ## 3         21.0          23          10            9            4           20
    ## 4          5.1           3           0            0            0            5
    ## 5          4.0           3           1            0            0            5
    ## 6          4.2           3           2            1            5            5
    ##   pre_X2b_pitch pre_X3b_pitch pre_hr_pitch pre_b.avg_pitch
    ## 1             3             0            0           0.211
    ## 2             0             0            2           0.157
    ## 3             4             0            1           0.280
    ## 4             0             0            0           0.176
    ## 5             1             0            0           0.200
    ## 6             1             0            0           0.214

Next, we will convert the columns of the pre-game data sets to their
appropriate types.

``` r
pregamepitching <- pregamepitching %>%
  clean_names() %>%
  mutate(pre_w_pitch = as.integer(pre_w_pitch),
         pre_l_pitch = as.integer(pre_l_pitch))

pregamehitting <- pregamehitting %>%
  clean_names() %>%
  rename("batter" = "name")
  

head(pregamepitching, 15)
```

    ##         date number                player pre_era_pitch pre_w_pitch pre_l_pitch
    ## 1  3/25/2022     28 John Michael Bertrand          1.72           4           0
    ## 2  3/25/2022     23         Austin Temple          2.42           2           0
    ## 3  3/25/2022     17          Aidan Tyrell          3.86           3           0
    ## 4  3/25/2022     24          Jack Findlay          0.00           2           0
    ## 5  3/25/2022     41       Jackson Dennies          0.00           0           0
    ## 6  3/25/2022     33        Radek Birkholz          1.93           0           0
    ## 7  3/25/2022     52        Ryan McLinskey          2.13           0           1
    ## 8  3/25/2022     45              Alex Rao          2.79           0           0
    ## 9  3/25/2022     31            Caden Aoki          3.86           0           2
    ## 10 3/25/2022     43           Will Mercer          6.00           0           0
    ## 11 3/25/2022     29            Liam Simon          6.75           0           0
    ## 12 3/25/2022     37           Matt Lazaro          6.75           0           0
    ## 13 3/25/2022     50          Sammy Cooper          9.00           0           0
    ## 14 3/25/2022     26         Roman Kimball          9.00           0           0
    ## 15 3/25/2022      9        Jack Brannigan         11.57           1           1
    ##    pre_app_pitch pre_gs_pitch pre_cg_pitch pre_sho_pitch pre_sv_pitch
    ## 1              5            5            0           0/0            0
    ## 2              5            4            0           0/1            1
    ## 3              5            5            0           0/0            0
    ## 4              2            2            0           0/0            0
    ## 5              3            0            0           0/0            0
    ## 6              5            0            0           0/0            0
    ## 7              6            0            0           0/1            2
    ## 8              5            0            0           0/1            0
    ## 9              4            0            0           0/0            0
    ## 10             2            0            0           0/0            1
    ## 11             3            0            0           0/0            0
    ## 12             2            0            0           0/0            0
    ## 13             1            0            0           0/0            0
    ## 14             3            0            0           0/0            0
    ## 15             5            0            0           0/1            0
    ##    pre_ip_pitch pre_h_pitch pre_r_pitch pre_er_pitch pre_bb_pitch pre_so_pitch
    ## 1          31.1          23           8            6            9           33
    ## 2          26.0          13           7            7            9           34
    ## 3          21.0          23          10            9            4           20
    ## 4           5.1           3           0            0            0            5
    ## 5           4.0           3           1            0            0            5
    ## 6           4.2           3           2            1            5            5
    ## 7          12.2           9           5            3            4           22
    ## 8           9.2           8           3            3            7           14
    ## 9           9.1           7           4            4            2           10
    ## 10          3.0           2           2            2            3            4
    ## 11          4.0           1           3            3            5            8
    ## 12          2.2           1           3            2            1            4
    ## 13          2.0           3           2            2            2            3
    ## 14          2.0           0           2            2            3            3
    ## 15          4.2           3           6            6            4           11
    ##    pre_x2b_pitch pre_x3b_pitch pre_hr_pitch pre_b_avg_pitch
    ## 1              3             0            0           0.211
    ## 2              0             0            2           0.157
    ## 3              4             0            1           0.280
    ## 4              0             0            0           0.176
    ## 5              1             0            0           0.200
    ## 6              1             0            0           0.214
    ## 7              2             0            0           0.200
    ## 8              1             0            0           0.222
    ## 9              2             0            2           0.200
    ## 10             1             0            0           0.182
    ## 11             0             0            1           0.083
    ## 12             0             0            1           0.100
    ## 13             0             0            1           0.300
    ## 14             0             0            0           0.000
    ## 15             0             0            2           0.167

``` r
head(pregamehitting, 15)
```

    ##         date number         batter pre_ab_hit pre_r_hit pre_h_hit pre_x2b_hit
    ## 1  3/25/2022      3  David LaManna         32         8        12           1
    ## 2  3/25/2022     42 Brooks Coetzee         60        12        21           4
    ## 3  3/25/2022      6    TJ Williams         38        12        13           1
    ## 4  3/25/2022      4    Carter Putz         59        19        20           1
    ## 5  3/25/2022      9 Jack Brannigan         59        15        19           8
    ## 6  3/25/2022      1      Ryan Cole         58        13        16           3
    ## 7  3/25/2022     30   DM Jefferson         30         4         8           0
    ## 8  3/25/2022     16   Jared Miller         46        13        12           3
    ## 9  3/25/2022      8     Danny Neri         25         2         6           2
    ## 10 3/25/2022     14  Zach Prajzner         52        15        12           3
    ## 11 3/25/2022      2  Spencer Myers         49         9         9           0
    ## 12 3/25/2022     47 Tony Lindwedel          1         3         1           0
    ## 13 3/25/2022     18    Nick Juaire          7         2         4           1
    ## 14 3/25/2022      7     Jack Zyska         21         6         9           1
    ## 15 3/25/2022     11    Jack Penney         17         5         5           3
    ##    pre_x3b_hit pre_hr_hit pre_rbi_hit pre_tb_hit pre_slg_hit pre_bb_hit
    ## 1            0          0           6         13       0.406          4
    ## 2            0          5          16         40       0.667          4
    ## 3            2          1           7         21       0.553          2
    ## 4            2          2          15         31       0.525          8
    ## 5            1          2          14         35       0.593          5
    ## 6            0          1           9         22       0.379          5
    ## 7            0          0           8          8       0.267          1
    ## 8            0          0           7         15       0.326          9
    ## 9            1          0           7         10       0.400          2
    ## 10           0          1           7         18       0.346          9
    ## 11           1          0           4         11       0.224          4
    ## 12           0          0           0          1       1.000          1
    ## 13           0          0           3          5       0.714          1
    ## 14           0          4           7         22       1.048          0
    ## 15           1          0           5         10       0.588          2
    ##    pre_hp_hit pre_so_hit pre_obp_hit pre_sf_hit pre_sh_hit
    ## 1           2          3       0.462          1          0
    ## 2           2         11       0.403          1          0
    ## 3           1          5       0.390          0          2
    ## 4           0         17       0.406          2          0
    ## 5           2          9       0.382          2          0
    ## 6           7         14       0.400          0          0
    ## 7           0          5       0.290          0          0
    ## 8           2          7       0.390          2          0
    ## 9           1          6       0.310          1          0
    ## 10          2         12       0.359          1          1
    ## 11          0          8       0.245          0          5
    ## 12          0          0       1.000          0          0
    ## 13          0          0       0.625          0          0
    ## 14          0          2       0.429          0          0
    ## 15          0          4       0.350          1          0

This procedure makes everything in the Pre-Game Metrics data sets clean
and ready for model deployment.

# Preparing the TrackMan and Pitch Zone Data: An Automated Iterative Approach

Now, we turn to merging the pre-game data with TrackMan data sets and
pitch tagging data sets.

We will boost the efficiency of this merging process through iterative
automation. So long as the user of the Command Zone Rating metrics
employs the same naming conventions for each file, they will simply need
to update the list of game dates below with the additional date, and all
files will be loaded and merged accordingly.

### Please follow the given naming conventions:

For TrackMan files: Title = “Month_Day_Year_TrackMan.csv”

For Zone Tagging files: Title = “Month_Day_Year_Pitching_Zones.csv”

These conventions maintain the full automation of the Command Zone
Rating calculation.

``` r
#Create a list of game dates, separated by underscores
game_dates <- c("3_25_2022", "3_29_2022", "4_05_2022", "4_08_2022", 
                "4_09_2022", "4_10_2022", "4_12_2022", "4_19_2022", 
                "4_20_2022", "4_22_2022", "4_23_2022", "4_24_2022")
game_dates
```

    ##  [1] "3_25_2022" "3_29_2022" "4_05_2022" "4_08_2022" "4_09_2022" "4_10_2022"
    ##  [7] "4_12_2022" "4_19_2022" "4_20_2022" "4_22_2022" "4_23_2022" "4_24_2022"

#### The interative join function

The following iterative loop takes each appropriately named TrackMan and
Pitching Zones files as inputs and merges them in the following way:

First, the TrackMan and Pitching Zone files from a given game date are
joined.

Second, the pre-game pitching and hitting metrics from that game are
left joined.

Then, the merged data frame object is stored as an element in the list

Finally, the rows of all data frame objects (one for each game date) are
combined using the rbind() function.

``` r
#Create empty lists for use in data merging. Each list contains one element for each game date in the final metric data set
zones <- vector("list", length(game_dates))
trackman <- vector("list", length(game_dates))
joined_zones_trackman <- vector("list", length(game_dates))
joined_add_pre_pitching <- vector("list", length(game_dates))
joined_add_pre_hitting <- vector("list", length(game_dates))

#Set the root of the working data, to be adjusted as needed
directory <- "C:/Users/david/Desktop/"

#Iterate through each TrackMan and Pitching Zones file, adding them as dataframes to their respective lists for each game. This process involves removing NA columns and rows, cleaning the data feature names, and establishing appropriate column names for "index" features. 
for(i in 1:length(game_dates)){
    trackman_name <- paste(game_dates[i], "_TrackMan.csv", sep = "")
    zones_name <- paste(game_dates[i], "_Pitching_Zones.csv", sep = "")
    print(trackman_name)
    print(zones_name)
    zones[[i]] <- read_csv(paste(directory, zones_name, sep = ""))
    zones[[i]] <- zones[[i]][, 1:6] %>%
      na_if("na") %>%
      na.omit() %>%
      na_if("Null") %>%
      na.omit() %>%
      clean_names() %>%
      mutate(pitch_number = as.integer(pitch_number),
             prepitch = as.character(prepitch),
             postpitch = as.character(postpitch))
    print(colnames(zones[[i]]))
    trackman[[i]] <- read.csv(paste(directory, trackman_name, sep = ""))
    trackman[[i]] <- trackman[[i]] %>%
      clean_names() %>%
      mutate(date = mdy(date),
             game_foreign_id = as.character(game_foreign_id)) %>%
      separate(batter, into = c("last", "first"), sep = ", ") %>%
      unite("batter", c("first", "last"), sep = " ", remove = TRUE) %>%
      rename(pitch_number = pitch_no)
    joined_zones_trackman[[i]] <- inner_join(zones[[i]], 
                                             trackman[[i]], 
                                             by = "pitch_number") %>%
      select(-date.y) %>%
      rename("date" = "date.x")
    joined_add_pre_pitching[[i]] <- left_join(joined_zones_trackman[[i]], 
                                               pregamepitching,
                                               by = c("date", "player"))
    joined_add_pre_hitting[[i]] <- left_join(joined_add_pre_pitching[[i]],
                                             pregamehitting,
                                             by = c("date", "batter"))
}
```

    ## [1] "3_25_2022_TrackMan.csv"
    ## [1] "3_25_2022_Pitching_Zones.csv"

    ## Rows: 334 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (5): Date, Team, Player, prepitch, postpitch
    ## dbl (1): Pitch Number

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## [1] "date"         "pitch_number" "team"         "player"       "prepitch"    
    ## [6] "postpitch"

    ## Warning: All formats failed to parse. No formats found.

    ## [1] "3_29_2022_TrackMan.csv"
    ## [1] "3_29_2022_Pitching_Zones.csv"

    ## Rows: 317 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (5): Date, Team, Player, prepitch, postpitch
    ## dbl (1): Pitch Number

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## [1] "date"         "pitch_number" "team"         "player"       "prepitch"    
    ## [6] "postpitch"

    ## Warning: All formats failed to parse. No formats found.

    ## [1] "4_05_2022_TrackMan.csv"
    ## [1] "4_05_2022_Pitching_Zones.csv"

    ## Rows: 258 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (4): Date, Team, Player, prepitch
    ## dbl (2): Pitch Number, postpitch

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## [1] "date"         "pitch_number" "team"         "player"       "prepitch"    
    ## [6] "postpitch"   
    ## [1] "4_08_2022_TrackMan.csv"
    ## [1] "4_08_2022_Pitching_Zones.csv"

    ## Rows: 259 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (4): Date, Team, Player, prepitch
    ## dbl (2): Pitch Number, postpitch

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## [1] "date"         "pitch_number" "team"         "player"       "prepitch"    
    ## [6] "postpitch"   
    ## [1] "4_09_2022_TrackMan.csv"
    ## [1] "4_09_2022_Pitching_Zones.csv"

    ## Rows: 314 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (4): Date, Team, Player, prepitch
    ## dbl (2): Pitch Number, postpitch

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## [1] "date"         "pitch_number" "team"         "player"       "prepitch"    
    ## [6] "postpitch"   
    ## [1] "4_10_2022_TrackMan.csv"
    ## [1] "4_10_2022_Pitching_Zones.csv"

    ## Rows: 323 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (5): Date, Team, Player, prepitch, postpitch
    ## dbl (1): Pitch Number

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## [1] "date"         "pitch_number" "team"         "player"       "prepitch"    
    ## [6] "postpitch"   
    ## [1] "4_12_2022_TrackMan.csv"
    ## [1] "4_12_2022_Pitching_Zones.csv"

    ## Rows: 354 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (4): Date, Team, Player, prepitch
    ## dbl (2): Pitch Number, postpitch

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## [1] "date"         "pitch_number" "team"         "player"       "prepitch"    
    ## [6] "postpitch"

    ## Warning: All formats failed to parse. No formats found.

    ## [1] "4_19_2022_TrackMan.csv"
    ## [1] "4_19_2022_Pitching_Zones.csv"

    ## Rows: 228 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (5): Date, Team, Player, prepitch, postpitch
    ## dbl (1): Pitch Number

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## [1] "date"         "pitch_number" "team"         "player"       "prepitch"    
    ## [6] "postpitch"

    ## Warning: All formats failed to parse. No formats found.

    ## [1] "4_20_2022_TrackMan.csv"
    ## [1] "4_20_2022_Pitching_Zones.csv"

    ## Rows: 307 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (4): Date, Team, Player, prepitch
    ## dbl (2): Pitch Number, postpitch

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## [1] "date"         "pitch_number" "team"         "player"       "prepitch"    
    ## [6] "postpitch"

    ## Warning: All formats failed to parse. No formats found.

    ## [1] "4_22_2022_TrackMan.csv"
    ## [1] "4_22_2022_Pitching_Zones.csv"

    ## Rows: 299 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (5): Date, Team, Player, prepitch, postpitch
    ## dbl (1): Pitch Number

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## [1] "date"         "pitch_number" "team"         "player"       "prepitch"    
    ## [6] "postpitch"

    ## Warning: All formats failed to parse. No formats found.

    ## [1] "4_23_2022_TrackMan.csv"
    ## [1] "4_23_2022_Pitching_Zones.csv"

    ## Rows: 370 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (4): Date, Team, Player, prepitch
    ## dbl (2): Pitch Number, postpitch

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## [1] "date"         "pitch_number" "team"         "player"       "prepitch"    
    ## [6] "postpitch"

    ## Warning: All formats failed to parse. No formats found.

    ## [1] "4_24_2022_TrackMan.csv"
    ## [1] "4_24_2022_Pitching_Zones.csv"

    ## Rows: 361 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (5): Date, Team, Player, prepitch, postpitch
    ## dbl (1): Pitch Number

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## [1] "date"         "pitch_number" "team"         "player"       "prepitch"    
    ## [6] "postpitch"

    ## Warning: All formats failed to parse. No formats found.

``` r
#Join the rows of each merged data frame object in the condensed list of TrackMan / Pitching Zones / Pre-Game Metrics data frames

full_metric_data <- joined_add_pre_hitting %>%
  bind_rows() %>%
  mutate(prepitch = as.integer(prepitch),
         postpitch = as.integer(postpitch),
         date = mdy(date)) %>%
  drop_na(prepitch)
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

``` r
#Inspect the full metric data frame 
head(full_metric_data, 20)
```

    ## # A tibble: 20 x 206
    ##    date       pitch_number team  player    prepitch postpitch time  p_aof_inning
    ##    <date>            <int> <chr> <chr>        <int>     <int> <chr>        <int>
    ##  1 2022-03-25           11 VT    Griffin ~       18        17 16:0~            1
    ##  2 2022-03-25           12 VT    Griffin ~       19         6 16:0~            2
    ##  3 2022-03-25           13 VT    Griffin ~       19        14 16:0~            2
    ##  4 2022-03-25           14 VT    Griffin ~       19         7 16:0~            2
    ##  5 2022-03-25           15 VT    Griffin ~       19        12 16:0~            2
    ##  6 2022-03-25           16 VT    Griffin ~       17        25 16:0~            3
    ##  7 2022-03-25           17 VT    Griffin ~       19        19 16:0~            3
    ##  8 2022-03-25           18 VT    Griffin ~       19         2 16:0~            4
    ##  9 2022-03-25           19 VT    Griffin ~       19        12 16:1~            4
    ## 10 2022-03-25           20 VT    Griffin ~       19         9 16:1~            4
    ## 11 2022-03-25           21 VT    Griffin ~       19         7 16:1~            4
    ## 12 2022-03-25           22 VT    Griffin ~       19        19 16:1~            4
    ## 13 2022-03-25           23 VT    Griffin ~       15         7 16:1~            4
    ## 14 2022-03-25           24 VT    Griffin ~       19         7 16:1~            4
    ## 15 2022-03-25           25 VT    Griffin ~       18        NA 16:1~            4
    ## 16 2022-03-25           26 VT    Griffin ~       18        11 16:1~            4
    ## 17 2022-03-25           27 VT    Griffin ~       18        20 16:1~            4
    ## 18 2022-03-25           28 ND    John Mic~       20        21 16:1~            1
    ## 19 2022-03-25           29 ND    John Mic~       20        18 16:1~            1
    ## 20 2022-03-25           30 ND    John Mic~       25        12 16:1~            1
    ## # ... with 198 more variables: pitchof_pa <int>, pitcher <chr>,
    ## #   pitcher_id <dbl>, pitcher_throws <chr>, pitcher_team <chr>, batter <chr>,
    ## #   batter_id <int>, batter_side <chr>, batter_team <chr>, pitcher_set <chr>,
    ## #   inning <int>, top_bottom <chr>, outs <int>, balls <int>, strikes <int>,
    ## #   tagged_pitch_type <chr>, auto_pitch_type <chr>, pitch_call <chr>,
    ## #   kor_bb <chr>, tagged_hit_type <chr>, play_result <chr>, outs_on_play <int>,
    ## #   runs_scored <int>, notes <lgl>, rel_speed <dbl>, vert_rel_angle <dbl>, ...

``` r
dim(full_metric_data)
```

    ## [1] 3585  206

## Zone Difference: Calculation

Before the iterative model-building calculation begins, we need to
calculate the absolute zone difference of each pitch thrown in the full
metric data set.

The Zone Difference variable will sum the absolute value of horizontal
zone differential and the absolute value of horizontal zone differential
for a total Zone Differential score.

In preparation for this summation, we will create a data frame that
represents the 25-zone grid for the sake of Zone Differential
Calculation.

``` r
zone_grid <- matrix(0, ncol = 5, nrow = 5)
zone_grid
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    0    0    0    0    0
    ## [2,]    0    0    0    0    0
    ## [3,]    0    0    0    0    0
    ## [4,]    0    0    0    0    0
    ## [5,]    0    0    0    0    0

We will fill this 5 x 5 matrix with the corresponding zone numbers:

``` r
for(i in 1:nrow(zone_grid)){
  for(j in 1:ncol(zone_grid)){
    zone_grid[i,j] = 5*(i-1) + j
  }
}
zone_grid
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    1    2    3    4    5
    ## [2,]    6    7    8    9   10
    ## [3,]   11   12   13   14   15
    ## [4,]   16   17   18   19   20
    ## [5,]   21   22   23   24   25

This grid will allow us to calculate the absolute zone differential (the
sum of absolute horizontal zone difference and absolute vertical zone
difference), as shown in an iterative approach below:

``` r
zone_difference <- rep(NA, nrow(full_metric_data))
for(i in 1:nrow(full_metric_data)){
  pre_pitch_zone <- full_metric_data$prepitch[i]
  post_pitch_zone <- full_metric_data$postpitch[i]
  pre_pitch_zone_row <- which(zone_grid == pre_pitch_zone, arr.ind = TRUE)[1]
  pre_pitch_zone_column <- which(zone_grid == pre_pitch_zone, arr.ind=TRUE)[2]
  post_pitch_zone_row <- which(zone_grid == post_pitch_zone, arr.ind = TRUE)[1]
  post_pitch_zone_column <- which(zone_grid == post_pitch_zone, arr.ind=TRUE)[2]
  
  zone_difference[i] <- abs(pre_pitch_zone_row - post_pitch_zone_row) + abs(pre_pitch_zone_column - post_pitch_zone_column)
}
```

With all zone differential values calculated in a list, we will append
the list to the full metric data set for future analysis and
incorporation into the Command Zone Rating calculation.

``` r
full_metric_data <- full_metric_data %>%
  mutate(zone_diff = zone_difference)

head(full_metric_data[, 205:207], 20)
```

    ## # A tibble: 20 x 3
    ##    pre_sf_hit pre_sh_hit zone_diff
    ##         <int>      <int>     <int>
    ##  1          0          2         1
    ##  2          0          0         5
    ##  3          0          0         1
    ##  4          0          0         4
    ##  5          0          0         3
    ##  6          2          0         4
    ##  7          2          0         0
    ##  8          2          0         5
    ##  9          2          0         3
    ## 10          2          0         2
    ## 11          2          0         4
    ## 12          2          0         0
    ## 13          2          0         4
    ## 14          2          0         4
    ## 15          2          0        NA
    ## 16          2          0         3
    ## 17          2          0         2
    ## 18          1          1         5
    ## 19          1          1         2
    ## 20          1          1         5

## Zone Difference: Visualization and Data Exploration

Let’s explore whether or not any relationships exist in the data set
between zone differential and the handedness of the batter and pitcher.

``` r
#Plot 1: Zone difference by pitch type, as tagged by the TrackMan operator at ND Baseball
plot1 <- ggplot(full_metric_data, aes(x = zone_diff, 
                                      y = reorder(tagged_pitch_type, 
                                                  zone_diff), 
                                      fill = tagged_pitch_type)) + 
  geom_density_ridges(alpha = 0.5) + # Use geom density ridges for 3d density
   theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) +  
  labs(x = "Zone Difference", 
       y = "Pitch Type",  # Set labels
       title = "Zone Difference by Pitch Type",
       subtitle = "Distribution Shapes") +
  guides(fill = FALSE)
```

    ## Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> =
    ## "none")` instead.

``` r
#Plot 2: Zone difference by Right-Handed vs. Left-Handed batters
plot2 <- ggplot(full_metric_data, aes(x = zone_diff, 
                                      y = reorder(batter_side, 
                                                  zone_diff), 
                                      fill = batter_side)) + 
  geom_density_ridges(alpha = 0.5) + 
   theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) + 
  labs(x = "Zone Difference", 
       y = "Batter Handedness", 
       title = "Zone Difference by Batter Handedness",
       subtitle = "Distribution Shapes") +
  guides(fill = FALSE)
```

    ## Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> =
    ## "none")` instead.

``` r
#Plot 3: Zone difference by Right-Handed vs. Left-Handed Pitchers
plot3 <- ggplot(full_metric_data, aes(x = zone_diff, y = reorder(pitcher_throws, zone_diff), fill = pitcher_throws)) + 
  geom_density_ridges(alpha = 0.5) + 
   theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) + 
  labs(x = "Zone Difference", 
       y = "Pitcher Handedness",  
       title = "Zone Difference by Pitcher Handedness",
       subtitle = "Distribution Shapes") +
  guides(fill = FALSE)
```

    ## Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> =
    ## "none")` instead.

``` r
#View the plot distributions
plot1
```

    ## Picking joint bandwidth of 0.335

    ## Warning: Removed 8 rows containing non-finite values (stat_density_ridges).

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/zone%20difference%20visualizations-1.png)<!-- -->

``` r
grid.arrange(plot2, plot3, ncol=2)
```

    ## Picking joint bandwidth of 0.271

    ## Warning: Removed 8 rows containing non-finite values (stat_density_ridges).

    ## Picking joint bandwidth of 0.265

    ## Warning: Removed 8 rows containing non-finite values (stat_density_ridges).

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/zone%20difference%20visualizations-2.png)<!-- -->

For a more granular look at the analysis presented above, let’s combine
pitcher and batter handedness to consider if L / L and R / R match-ups
present command problems:

``` r
full_metric_data <- full_metric_data %>%
  unite("batter_pitcher", c("batter_side", "pitcher_throws"), remove = FALSE)

plot4 <- ggplot(full_metric_data, aes(x = zone_diff, y = reorder(batter_pitcher, zone_diff), fill = batter_pitcher)) + # Set aesthetics
  geom_density_ridges(alpha = 0.5) + # Use geom density ridges for 3d density
   theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) + # Remove grid 
  labs(x = "Zone Difference", 
       y = "Pitcher Handedness",  # Set labels
       title = "Zone Difference by Pitcher Handedness",
       subtitle = "Distribution Shapes") +
  guides(fill = FALSE)
```

    ## Warning: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> =
    ## "none")` instead.

``` r
plot4
```

    ## Picking joint bandwidth of 0.315

    ## Warning: Removed 8 rows containing non-finite values (stat_density_ridges).

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/more%20zone%20differnce%20visualizations-1.png)<!-- -->

Finally, we will aggregate the zone differentials into an overall
histogram, filtered by pitch type, for a high-level look at the variable
distribution:

``` r
ggplot(data = full_metric_data, mapping = aes(x = zone_diff, fill = tagged_pitch_type)) +
  geom_bar() +
  labs(x = "Zone Differential", 
       y = "Frequency", 
       fill = "Pitch Type",
       title = "Zone Differential by Pitch Type", 
       subtitle = "Overall Frequency") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
```

    ## Warning: Removed 8 rows containing non-finite values (stat_count).

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/zone%20difference%20historgram-1.png)<!-- -->

Given its automation, the above visualizations will vary as new data
sets are introduced into the Command Zone Rating metric calculation. The
plot images may be extracted for advanced pitching analysis as
requested.

## Variable Selection for the Model-Building Process

This merged data set contains a large proportion of features that are
not needed for the Command Zone Rating calculation. The following
variable reduction modifications become necessary:

1.  Given the collection of pre-game hitting data throughout the season,
    we will limit the included pre-game metrics to standardized
    composite metrics that are independent of the number of games
    played. As such, we ensure that the included metrics (batting
    average, on-base percentage, slugging percentage, etc.) do not place
    inaccurate weight on these features in the model based on their wide
    variance.

2.  Given the collection of pre-game pitching data throughout the
    season, we will limit the included pre-game metrics to standardized
    composite metrics that are independent of the number of games
    played. As such, we ensure that the included metrics (ERA, WHIP, K/9
    IP, wins + saves per appearance, etc.) do not place inaccurate
    weight on these features in the model based on their wide variance.

3.  Many of the variables from the raw TrackMan file that document the
    ball flight are independent of the question of pitching command.
    While the individual pitching metrics like velocity, vertical
    approach angle, and spin rate are relevant for Command Zone Rating
    model-building, some of the ball-in-play metrics, such as exit
    velocity and launch angle, are not reflective of individual pitcher
    command. In preparation for model predictions, which take the
    average TrackMan pitching metrics and NCAA league average hitting
    metrics as inputs for raw probability scores, we will limit the use
    of TrackMan to these pitcher-specific measurements.

4.  Duplicate ID columns and redundant measurements may be removed.

5.  Ultimately, we will segment the data based on “Ball Zones” (16 zones
    beyond the 9 strike zones) and “Base Hit Zones” (9 zones within the
    strike zones) and convert the Play Result to binary categorical
    variables of Ball Called / No Balled Called and Base Hit / No Base
    Hit, respectively.

Let’s make a list of the variables that we will need in this merged data
set in anticipation of removal. To do that, we will first need the full
list of column names:

``` r
colnames(full_metric_data)
```

    ##   [1] "date"                              "pitch_number"                     
    ##   [3] "team"                              "player"                           
    ##   [5] "prepitch"                          "postpitch"                        
    ##   [7] "time"                              "p_aof_inning"                     
    ##   [9] "pitchof_pa"                        "pitcher"                          
    ##  [11] "pitcher_id"                        "batter_pitcher"                   
    ##  [13] "pitcher_throws"                    "pitcher_team"                     
    ##  [15] "batter"                            "batter_id"                        
    ##  [17] "batter_side"                       "batter_team"                      
    ##  [19] "pitcher_set"                       "inning"                           
    ##  [21] "top_bottom"                        "outs"                             
    ##  [23] "balls"                             "strikes"                          
    ##  [25] "tagged_pitch_type"                 "auto_pitch_type"                  
    ##  [27] "pitch_call"                        "kor_bb"                           
    ##  [29] "tagged_hit_type"                   "play_result"                      
    ##  [31] "outs_on_play"                      "runs_scored"                      
    ##  [33] "notes"                             "rel_speed"                        
    ##  [35] "vert_rel_angle"                    "horz_rel_angle"                   
    ##  [37] "spin_rate"                         "spin_axis"                        
    ##  [39] "tilt"                              "rel_height"                       
    ##  [41] "rel_side"                          "extension"                        
    ##  [43] "vert_break"                        "induced_vert_break"               
    ##  [45] "horz_break"                        "plate_loc_height"                 
    ##  [47] "plate_loc_side"                    "zone_speed"                       
    ##  [49] "vert_appr_angle"                   "horz_appr_angle"                  
    ##  [51] "zone_time"                         "exit_speed"                       
    ##  [53] "angle"                             "direction"                        
    ##  [55] "hit_spin_rate"                     "position_at110x"                  
    ##  [57] "position_at110y"                   "position_at110z"                  
    ##  [59] "distance"                          "last_tracked_distance"            
    ##  [61] "bearing"                           "hang_time"                        
    ##  [63] "pfxx"                              "pfxz"                             
    ##  [65] "x0"                                "y0"                               
    ##  [67] "z0"                                "vx0"                              
    ##  [69] "vy0"                               "vz0"                              
    ##  [71] "ax0"                               "ay0"                              
    ##  [73] "az0"                               "home_team"                        
    ##  [75] "away_team"                         "stadium"                          
    ##  [77] "level"                             "league"                           
    ##  [79] "game_id"                           "pitch_uid"                        
    ##  [81] "effective_velo"                    "max_height"                       
    ##  [83] "measured_duration"                 "speed_drop"                       
    ##  [85] "pitch_last_measured_x"             "pitch_last_measured_y"            
    ##  [87] "pitch_last_measured_z"             "contact_position_x"               
    ##  [89] "contact_position_y"                "contact_position_z"               
    ##  [91] "game_uid"                          "utc_date"                         
    ##  [93] "utc_time"                          "local_date_time"                  
    ##  [95] "utc_date_time"                     "auto_hit_type"                    
    ##  [97] "system"                            "home_team_foreign_id"             
    ##  [99] "away_team_foreign_id"              "game_foreign_id"                  
    ## [101] "catcher"                           "catcher_id"                       
    ## [103] "catcher_throws"                    "catcher_team"                     
    ## [105] "play_id"                           "pitch_trajectory_xc0"             
    ## [107] "pitch_trajectory_xc1"              "pitch_trajectory_xc2"             
    ## [109] "pitch_trajectory_yc0"              "pitch_trajectory_yc1"             
    ## [111] "pitch_trajectory_yc2"              "pitch_trajectory_zc0"             
    ## [113] "pitch_trajectory_zc1"              "pitch_trajectory_zc2"             
    ## [115] "hit_spin_axis"                     "hit_trajectory_xc0"               
    ## [117] "hit_trajectory_xc1"                "hit_trajectory_xc2"               
    ## [119] "hit_trajectory_xc3"                "hit_trajectory_xc4"               
    ## [121] "hit_trajectory_xc5"                "hit_trajectory_xc6"               
    ## [123] "hit_trajectory_xc7"                "hit_trajectory_xc8"               
    ## [125] "hit_trajectory_yc0"                "hit_trajectory_yc1"               
    ## [127] "hit_trajectory_yc2"                "hit_trajectory_yc3"               
    ## [129] "hit_trajectory_yc4"                "hit_trajectory_yc5"               
    ## [131] "hit_trajectory_yc6"                "hit_trajectory_yc7"               
    ## [133] "hit_trajectory_yc8"                "hit_trajectory_zc0"               
    ## [135] "hit_trajectory_zc1"                "hit_trajectory_zc2"               
    ## [137] "hit_trajectory_zc3"                "hit_trajectory_zc4"               
    ## [139] "hit_trajectory_zc5"                "hit_trajectory_zc6"               
    ## [141] "hit_trajectory_zc7"                "hit_trajectory_zc8"               
    ## [143] "throw_speed"                       "pop_time"                         
    ## [145] "exchange_time"                     "time_to_base"                     
    ## [147] "catch_position_x"                  "catch_position_y"                 
    ## [149] "catch_position_z"                  "throw_position_x"                 
    ## [151] "throw_position_y"                  "throw_position_z"                 
    ## [153] "base_position_x"                   "base_position_y"                  
    ## [155] "base_position_z"                   "throw_trajectory_xc0"             
    ## [157] "throw_trajectory_xc1"              "throw_trajectory_xc2"             
    ## [159] "throw_trajectory_yc0"              "throw_trajectory_yc1"             
    ## [161] "throw_trajectory_yc2"              "throw_trajectory_zc0"             
    ## [163] "throw_trajectory_zc1"              "throw_trajectory_zc2"             
    ## [165] "pitch_release_confidence"          "pitch_location_confidence"        
    ## [167] "pitch_movement_confidence"         "hit_launch_confidence"            
    ## [169] "hit_landing_confidence"            "catcher_throw_catch_confidence"   
    ## [171] "catcher_throw_release_confidence"  "catcher_throw_location_confidence"
    ## [173] "number.x"                          "pre_era_pitch"                    
    ## [175] "pre_w_pitch"                       "pre_l_pitch"                      
    ## [177] "pre_app_pitch"                     "pre_gs_pitch"                     
    ## [179] "pre_cg_pitch"                      "pre_sho_pitch"                    
    ## [181] "pre_sv_pitch"                      "pre_ip_pitch"                     
    ## [183] "pre_h_pitch"                       "pre_r_pitch"                      
    ## [185] "pre_er_pitch"                      "pre_bb_pitch"                     
    ## [187] "pre_so_pitch"                      "pre_x2b_pitch"                    
    ## [189] "pre_x3b_pitch"                     "pre_hr_pitch"                     
    ## [191] "pre_b_avg_pitch"                   "number.y"                         
    ## [193] "pre_ab_hit"                        "pre_r_hit"                        
    ## [195] "pre_h_hit"                         "pre_x2b_hit"                      
    ## [197] "pre_x3b_hit"                       "pre_hr_hit"                       
    ## [199] "pre_rbi_hit"                       "pre_tb_hit"                       
    ## [201] "pre_slg_hit"                       "pre_bb_hit"                       
    ## [203] "pre_hp_hit"                        "pre_so_hit"                       
    ## [205] "pre_obp_hit"                       "pre_sf_hit"                       
    ## [207] "pre_sh_hit"                        "zone_diff"

Next, remove the metrics pertaining to ball flight and other irrelevant
measures with respect to command:

``` r
metric_vars_list <- c("date",
                      "team",
                      "player",
                      "prepitch",
                      "postpitch",
                      "batter_side",
                      "outs",
                      "balls",
                      "strikes",
                      "inning",
                      "tagged_pitch_type",
                      "pitch_call",
                      "play_result",
                      "kor_bb",
                      "rel_speed",
                      "vert_rel_angle",
                      "horz_rel_angle",
                      "spin_rate",
                      "spin_axis",
                      "tilt",
                      "rel_height",
                      "rel_side",
                      "extension",
                      "vert_break",
                      "induced_vert_break",
                      "horz_break",
                      "zone_speed",
                      "vert_appr_angle",
                      "horz_appr_angle",
                      "pre_era_pitch",
                      "pre_w_pitch",
                      "pre_l_pitch",
                      "pre_sv_pitch",
                      "pre_app_pitch",
                      "pre_ip_pitch",
                      "pre_h_pitch",
                      "pre_bb_pitch",
                      "pre_so_pitch",
                      "pre_x2b_pitch",
                      "pre_x3b_pitch",
                      "pre_hr_pitch",
                      "pre_b_avg_pitch",
                      "pre_ab_hit",
                      "pre_h_hit",
                      "pre_x2b_hit",
                      "pre_x3b_hit",
                      "pre_hr_hit",
                      "pre_rbi_hit",
                      "pre_tb_hit",
                      "pre_bb_hit",
                      "pre_so_hit",
                      "pre_obp_hit",
                      "pre_slg_hit",
                      "zone_diff")

metrics_reduced_model_data <- subset(full_metric_data, select = metric_vars_list)

head(metrics_reduced_model_data)
```

    ## # A tibble: 6 x 54
    ##   date       team  player     prepitch postpitch batter_side  outs balls strikes
    ##   <date>     <chr> <chr>         <int>     <int> <chr>       <int> <int>   <int>
    ## 1 2022-03-25 VT    Griffin G~       18        17 Right           0     0       0
    ## 2 2022-03-25 VT    Griffin G~       19         6 Left            1     0       0
    ## 3 2022-03-25 VT    Griffin G~       19        14 Left            1     1       0
    ## 4 2022-03-25 VT    Griffin G~       19         7 Left            1     1       1
    ## 5 2022-03-25 VT    Griffin G~       19        12 Left            1     1       2
    ## 6 2022-03-25 VT    Griffin G~       17        25 Right           2     0       0
    ## # ... with 45 more variables: inning <int>, tagged_pitch_type <chr>,
    ## #   pitch_call <chr>, play_result <chr>, kor_bb <chr>, rel_speed <dbl>,
    ## #   vert_rel_angle <dbl>, horz_rel_angle <dbl>, spin_rate <dbl>,
    ## #   spin_axis <dbl>, tilt <chr>, rel_height <dbl>, rel_side <dbl>,
    ## #   extension <dbl>, vert_break <dbl>, induced_vert_break <dbl>,
    ## #   horz_break <dbl>, zone_speed <dbl>, vert_appr_angle <dbl>,
    ## #   horz_appr_angle <dbl>, pre_era_pitch <dbl>, pre_w_pitch <int>, ...

## Variable Creation: Standardized Hitting and Pitching Metrics

In this section, we will create a list of pitching and hitting metrics
that can be compared over time as pre-game metric data sets from various
points in the NCAA season are introduced.

The following standardized metrics will be calculated, among others:

1.  Batting Average

2.  Walks + Hits / Innings Pitched (WHIP)

3.  Strikeouts per Inning Pitched (K/IP)

4.  Wins + Saves / Appearances (WS / APP)

5.  Fielding Independent Pitching (FIP)

6.  Total Bases / Innings Pitched (Proxy for Opponent OPS)

Pitching Metrics: Fielding Independent Pitching, OPPOPS, Strikeouts per
IP, WHIP, WS / APP:

``` r
metrics_reduced_model_data <- metrics_reduced_model_data %>%
  mutate(pre_w_pitch = as.double(pre_w_pitch),
         pre_sv_pitch = as.double(pre_sv_pitch),
         pre_app_pitch = as.integer(pre_app_pitch)) %>%
  mutate(pre_w_pitch = if_else(is.na(pre_w_pitch), 0, pre_w_pitch)) %>%
  mutate(fip = (13 * pre_hr_pitch  + 3 * pre_bb_pitch - 2 * pre_so_pitch) / pre_ip_pitch,
         opp_ops = (2*pre_x2b_pitch + 
                      3*pre_x3b_pitch + 
                      4*pre_hr_pitch + 
                      (pre_h_pitch - pre_x2b_pitch - pre_x3b_pitch - pre_hr_pitch)*1)/ pre_ip_pitch,
         wins_saves_app = (pre_w_pitch + pre_sv_pitch) / pre_app_pitch,
         k_per_ip = pre_so_pitch / pre_ip_pitch,
         whip = (pre_bb_pitch + pre_h_pitch)/ pre_bb_pitch)
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

``` r
head(metrics_reduced_model_data) 
```

    ## # A tibble: 6 x 59
    ##   date       team  player     prepitch postpitch batter_side  outs balls strikes
    ##   <date>     <chr> <chr>         <int>     <int> <chr>       <int> <int>   <int>
    ## 1 2022-03-25 VT    Griffin G~       18        17 Right           0     0       0
    ## 2 2022-03-25 VT    Griffin G~       19         6 Left            1     0       0
    ## 3 2022-03-25 VT    Griffin G~       19        14 Left            1     1       0
    ## 4 2022-03-25 VT    Griffin G~       19         7 Left            1     1       1
    ## 5 2022-03-25 VT    Griffin G~       19        12 Left            1     1       2
    ## 6 2022-03-25 VT    Griffin G~       17        25 Right           2     0       0
    ## # ... with 50 more variables: inning <int>, tagged_pitch_type <chr>,
    ## #   pitch_call <chr>, play_result <chr>, kor_bb <chr>, rel_speed <dbl>,
    ## #   vert_rel_angle <dbl>, horz_rel_angle <dbl>, spin_rate <dbl>,
    ## #   spin_axis <dbl>, tilt <chr>, rel_height <dbl>, rel_side <dbl>,
    ## #   extension <dbl>, vert_break <dbl>, induced_vert_break <dbl>,
    ## #   horz_break <dbl>, zone_speed <dbl>, vert_appr_angle <dbl>,
    ## #   horz_appr_angle <dbl>, pre_era_pitch <dbl>, pre_w_pitch <dbl>, ...

Hitting Metrics: OPS, BB / AB, SO / AB

``` r
metrics_reduced_model_data <- metrics_reduced_model_data %>%
  mutate(ops = pre_obp_hit + pre_slg_hit,
         bb_per_ab = pre_bb_hit / pre_ab_hit,
         pre_so_hit = as.numeric(pre_so_hit),
         so_per_ab = pre_so_hit / pre_ab_hit)

head(metrics_reduced_model_data)
```

    ## # A tibble: 6 x 62
    ##   date       team  player     prepitch postpitch batter_side  outs balls strikes
    ##   <date>     <chr> <chr>         <int>     <int> <chr>       <int> <int>   <int>
    ## 1 2022-03-25 VT    Griffin G~       18        17 Right           0     0       0
    ## 2 2022-03-25 VT    Griffin G~       19         6 Left            1     0       0
    ## 3 2022-03-25 VT    Griffin G~       19        14 Left            1     1       0
    ## 4 2022-03-25 VT    Griffin G~       19         7 Left            1     1       1
    ## 5 2022-03-25 VT    Griffin G~       19        12 Left            1     1       2
    ## 6 2022-03-25 VT    Griffin G~       17        25 Right           2     0       0
    ## # ... with 53 more variables: inning <int>, tagged_pitch_type <chr>,
    ## #   pitch_call <chr>, play_result <chr>, kor_bb <chr>, rel_speed <dbl>,
    ## #   vert_rel_angle <dbl>, horz_rel_angle <dbl>, spin_rate <dbl>,
    ## #   spin_axis <dbl>, tilt <chr>, rel_height <dbl>, rel_side <dbl>,
    ## #   extension <dbl>, vert_break <dbl>, induced_vert_break <dbl>,
    ## #   horz_break <dbl>, zone_speed <dbl>, vert_appr_angle <dbl>,
    ## #   horz_appr_angle <dbl>, pre_era_pitch <dbl>, pre_w_pitch <dbl>, ...

Now, reduce the variables in the metric model data set to the
standardized metrics used for filtering and modeling for CZR:

``` r
metrics_reduced_model_data <- metrics_reduced_model_data %>%
  select(date,
         player,
         team,
         pitch_call,
         play_result,
         zone_diff,
         postpitch, 
         batter_side, 
         outs, 
         balls, 
         strikes, 
         inning, 
         tagged_pitch_type,
         pitch_call,
         play_result,
         rel_speed,
         vert_rel_angle,
         horz_rel_angle,
         spin_rate,
         spin_axis,
         extension,
         vert_break,
         induced_vert_break,
         horz_break,
         zone_speed,
         vert_appr_angle,
         horz_appr_angle,
         pre_era_pitch,
         pre_b_avg_pitch,
         fip,
         opp_ops,
         wins_saves_app,
         k_per_ip,
         ops,
         bb_per_ab,
         so_per_ab,
         whip) %>%
  mutate(batter_side = as.factor(batter_side),
         outs = as.factor(outs),
         balls = as.factor(balls),
         strikes = as.factor(strikes),
         inning = as.factor(inning),
         tagged_pitch_type = as.factor(tagged_pitch_type))

head(metrics_reduced_model_data)
```

    ## # A tibble: 6 x 35
    ##   date       player team  pitch_call play_result zone_diff postpitch batter_side
    ##   <date>     <chr>  <chr> <chr>      <chr>           <int>     <int> <fct>      
    ## 1 2022-03-25 Griff~ VT    InPlay     Out                 1        17 Right      
    ## 2 2022-03-25 Griff~ VT    BallCalled Undefined           5         6 Left       
    ## 3 2022-03-25 Griff~ VT    StrikeCal~ Undefined           1        14 Left       
    ## 4 2022-03-25 Griff~ VT    StrikeSwi~ Undefined           4         7 Left       
    ## 5 2022-03-25 Griff~ VT    StrikeSwi~ Undefined           3        12 Left       
    ## 6 2022-03-25 Griff~ VT    BallCalled Undefined           4        25 Right      
    ## # ... with 27 more variables: outs <fct>, balls <fct>, strikes <fct>,
    ## #   inning <fct>, tagged_pitch_type <fct>, rel_speed <dbl>,
    ## #   vert_rel_angle <dbl>, horz_rel_angle <dbl>, spin_rate <dbl>,
    ## #   spin_axis <dbl>, extension <dbl>, vert_break <dbl>,
    ## #   induced_vert_break <dbl>, horz_break <dbl>, zone_speed <dbl>,
    ## #   vert_appr_angle <dbl>, horz_appr_angle <dbl>, pre_era_pitch <dbl>,
    ## #   pre_b_avg_pitch <dbl>, fip <dbl>, opp_ops <dbl>, wins_saves_app <dbl>, ...

# Create the Results Data Frame

In anticipation of model building, we must build a data frame that
contains the following:

1.  25 factor variables across 25 columns, one corresponding to each
    zone, with ones in the diagonal for a total of 25 rows

2.  The average pitching metrics for a given pitcher’s pitch type by
    hitter handedness (R/L)

3.  The aggregate mean NCAA hitting metrics among faced teams in the
    data set

4.  An empty column in which corresponding test predictions on these
    observational rows will be placed

5.  The pitch splits of each pitcher for the sake of filtering out
    pitches thrown less than 20 percent of the time

I will begin by calculating the pitch splits by player among all pitches
thrown in the data set:

``` r
metrics_reduced_model_data <- metrics_reduced_model_data %>%
  mutate(team = if_else(team == "ND", "Notre Dame", team)) 

by_pitch <- metrics_reduced_model_data %>%
  filter(team == "Notre Dame") %>%
  group_by(player, tagged_pitch_type) %>%
  count()

by_pitch
```

    ## # A tibble: 54 x 3
    ## # Groups:   player, tagged_pitch_type [54]
    ##    player        tagged_pitch_type     n
    ##    <chr>         <fct>             <int>
    ##  1 Aidan Tyrell  ChangeUp             13
    ##  2 Aidan Tyrell  Curveball             4
    ##  3 Aidan Tyrell  Fastball             67
    ##  4 Aidan Tyrell  Slider               28
    ##  5 Alex Rao      ChangeUp             43
    ##  6 Alex Rao      Curveball             1
    ##  7 Alex Rao      Fastball             69
    ##  8 Alex Rao      Slider                3
    ##  9 Austin Temple ChangeUp              6
    ## 10 Austin Temple Curveball            45
    ## # ... with 44 more rows

``` r
by_total_pitches <- metrics_reduced_model_data %>%
  filter(team == "Notre Dame") %>%
  group_by(player) %>%
  count()

by_total_pitches
```

    ## # A tibble: 15 x 2
    ## # Groups:   player [15]
    ##    player                    n
    ##    <chr>                 <int>
    ##  1 Aidan Tyrell            112
    ##  2 Alex Rao                116
    ##  3 Austin Temple           165
    ##  4 Jack Brannigan           86
    ##  5 Jack Findlay            198
    ##  6 Jackson Dennies         130
    ##  7 John Michael Bertrand   279
    ##  8 Liam Simmon              10
    ##  9 Liam Simon              144
    ## 10 Matt Lazzaro             34
    ## 11 Radek Birkholz           99
    ## 12 Roman Kimball           158
    ## 13 Ryan McLinskey          106
    ## 14 Sammy Cooper             12
    ## 15 Will Mercer              56

``` r
by_pitcher_joined <- by_pitch %>%
  left_join(by_total_pitches,
            by = "player") %>%
  mutate(pitch_split = n.x / n.y) %>%
  select(-c(n.x, n.y)) %>%
  filter(pitch_split >= 0.2)

by_pitcher_joined
```

    ## # A tibble: 29 x 3
    ## # Groups:   player, tagged_pitch_type [29]
    ##    player         tagged_pitch_type pitch_split
    ##    <chr>          <fct>                   <dbl>
    ##  1 Aidan Tyrell   Fastball                0.598
    ##  2 Aidan Tyrell   Slider                  0.25 
    ##  3 Alex Rao       ChangeUp                0.371
    ##  4 Alex Rao       Fastball                0.595
    ##  5 Austin Temple  Curveball               0.273
    ##  6 Austin Temple  Fastball                0.448
    ##  7 Austin Temple  Slider                  0.242
    ##  8 Jack Brannigan Fastball                0.5  
    ##  9 Jack Brannigan Slider                  0.279
    ## 10 Jack Findlay   Fastball                0.768
    ## # ... with 19 more rows

Next, I will create dummy variables for post pitch zones and convert
them into factors

``` r
metric_results <- metrics_reduced_model_data %>%
  select(postpitch) %>%
  dummy_cols() %>%
  unique() %>%
  arrange(postpitch) %>%
  drop_na() %>%
  select(-c(postpitch, postpitch_NA)) %>%
  mutate(across(.cols = everything(), as.factor))

head(metric_results)
```

    ## # A tibble: 6 x 25
    ##   postpitch_1 postpitch_2 postpitch_3 postpitch_4 postpitch_5 postpitch_6
    ##   <fct>       <fct>       <fct>       <fct>       <fct>       <fct>      
    ## 1 1           0           0           0           0           0          
    ## 2 0           1           0           0           0           0          
    ## 3 0           0           1           0           0           0          
    ## 4 0           0           0           1           0           0          
    ## 5 0           0           0           0           1           0          
    ## 6 0           0           0           0           0           1          
    ## # ... with 19 more variables: postpitch_7 <fct>, postpitch_8 <fct>,
    ## #   postpitch_9 <fct>, postpitch_10 <fct>, postpitch_11 <fct>,
    ## #   postpitch_12 <fct>, postpitch_13 <fct>, postpitch_14 <fct>,
    ## #   postpitch_15 <fct>, postpitch_16 <fct>, postpitch_17 <fct>,
    ## #   postpitch_18 <fct>, postpitch_19 <fct>, postpitch_20 <fct>,
    ## #   postpitch_21 <fct>, postpitch_22 <fct>, postpitch_23 <fct>,
    ## #   postpitch_24 <fct>, postpitch_25 <fct>

The following combinations grid creates a set of 25 eligible test rows
per model (dependent on a given player, batter side, pitch type
combination and tested on the corresponding rows):

``` r
combinations_grid <- by_pitcher_joined %>%
   dplyr::slice(rep(1:n(), each = 2))

batter_side <- rep(c("Left", "Right"), nrow(combinations_grid)/2)

combinations_grid$batter_side <- as.factor(batter_side)

head(combinations_grid, 20)
```

    ## # A tibble: 20 x 4
    ## # Groups:   player, tagged_pitch_type [10]
    ##    player         tagged_pitch_type pitch_split batter_side
    ##    <chr>          <fct>                   <dbl> <fct>      
    ##  1 Aidan Tyrell   Fastball                0.598 Left       
    ##  2 Aidan Tyrell   Fastball                0.598 Right      
    ##  3 Aidan Tyrell   Slider                  0.25  Left       
    ##  4 Aidan Tyrell   Slider                  0.25  Right      
    ##  5 Alex Rao       ChangeUp                0.371 Left       
    ##  6 Alex Rao       ChangeUp                0.371 Right      
    ##  7 Alex Rao       Fastball                0.595 Left       
    ##  8 Alex Rao       Fastball                0.595 Right      
    ##  9 Austin Temple  Curveball               0.273 Left       
    ## 10 Austin Temple  Curveball               0.273 Right      
    ## 11 Austin Temple  Fastball                0.448 Left       
    ## 12 Austin Temple  Fastball                0.448 Right      
    ## 13 Austin Temple  Slider                  0.242 Left       
    ## 14 Austin Temple  Slider                  0.242 Right      
    ## 15 Jack Brannigan Fastball                0.5   Left       
    ## 16 Jack Brannigan Fastball                0.5   Right      
    ## 17 Jack Brannigan Slider                  0.279 Left       
    ## 18 Jack Brannigan Slider                  0.279 Right      
    ## 19 Jack Findlay   Fastball                0.768 Left       
    ## 20 Jack Findlay   Fastball                0.768 Right

The following aggregations calculate the following:

1.  The pregame metrics of a pitcher from his last appearance in a game
    in the data set

2.  The average hitting metrics across NCAA hitters faced by Notre Dame

3.  The average TrackMan pitching metrics by player, pitch type, and
    batter handedness

The resulting data sets are joined on the data frame of bounded eligible
test rows (thus forming the basis for predictions on each model as
features):

``` r
head(metrics_reduced_model_data)
```

    ## # A tibble: 6 x 35
    ##   date       player team  pitch_call play_result zone_diff postpitch batter_side
    ##   <date>     <chr>  <chr> <chr>      <chr>           <int>     <int> <fct>      
    ## 1 2022-03-25 Griff~ VT    InPlay     Out                 1        17 Right      
    ## 2 2022-03-25 Griff~ VT    BallCalled Undefined           5         6 Left       
    ## 3 2022-03-25 Griff~ VT    StrikeCal~ Undefined           1        14 Left       
    ## 4 2022-03-25 Griff~ VT    StrikeSwi~ Undefined           4         7 Left       
    ## 5 2022-03-25 Griff~ VT    StrikeSwi~ Undefined           3        12 Left       
    ## 6 2022-03-25 Griff~ VT    BallCalled Undefined           4        25 Right      
    ## # ... with 27 more variables: outs <fct>, balls <fct>, strikes <fct>,
    ## #   inning <fct>, tagged_pitch_type <fct>, rel_speed <dbl>,
    ## #   vert_rel_angle <dbl>, horz_rel_angle <dbl>, spin_rate <dbl>,
    ## #   spin_axis <dbl>, extension <dbl>, vert_break <dbl>,
    ## #   induced_vert_break <dbl>, horz_break <dbl>, zone_speed <dbl>,
    ## #   vert_appr_angle <dbl>, horz_appr_angle <dbl>, pre_era_pitch <dbl>,
    ## #   pre_b_avg_pitch <dbl>, fip <dbl>, opp_ops <dbl>, wins_saves_app <dbl>, ...

``` r
case_when_for_first_appearance <- function(x) {
  case_when(is.na(x) ~ mean(x, na.rm=TRUE),
            TRUE ~ as.numeric(x))
}

average_trackman_metrics_by_pitcher <- metrics_reduced_model_data %>%
  filter(team == "Notre Dame") %>%
  select(c(player, tagged_pitch_type, batter_side, 14:25)) %>%
  group_by(player, tagged_pitch_type, batter_side) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  distinct()

last_pitching_appearance_pregame <- metrics_reduced_model_data %>%
  filter(team == "Notre Dame") %>%
  group_by(player) %>%
  summarise(max(date)) %>%
  rename("date" = "max(date)") %>%
  left_join(metrics_reduced_model_data,
            by = c("player", "date")) %>%
  select(c(1, 8, 13, 26:31, 35)) %>%
  mutate(across(4:10, case_when_for_first_appearance)) %>%
  distinct()
  
average_ncaa_hitting_metrics <- metrics_reduced_model_data %>%
  mutate(across(32:34, mean, na.rm = TRUE)) %>%
  select(32:34) %>%
  distinct()


metric_results_matrix_list <- vector("list", nrow(combinations_grid))
for(i in 1:length(metric_results_matrix_list)){
  metric_results_matrix_list[[i]] <- metric_results
  metric_results_matrix_list[[i]]$player <- rep(combinations_grid$player[i], 25)
  metric_results_matrix_list[[i]]$batter_side <- rep(combinations_grid$batter_side[i], 25)
  metric_results_matrix_list[[i]]$tagged_pitch_type <- rep(combinations_grid$tagged_pitch_type[i], 25)
  metric_results_matrix_list[[i]]$pitch_split <- rep(combinations_grid$pitch_split[i], 25)
  metric_results_matrix_list[[i]]$ops <- rep(average_ncaa_hitting_metrics$ops, 25)
  metric_results_matrix_list[[i]]$bb_per_ab <- rep(average_ncaa_hitting_metrics$bb_per_ab, 25)
  metric_results_matrix_list[[i]]$so_per_ab <- rep(average_ncaa_hitting_metrics$so_per_ab, 25)
  metric_results_matrix_list[[i]] <- metric_results_matrix_list[[i]] %>%
    left_join(average_trackman_metrics_by_pitcher,
              by = c("player",
                     "batter_side",
                     "tagged_pitch_type")) %>%
    left_join(last_pitching_appearance_pregame,
              by = c("player",
                     "batter_side",
                     "tagged_pitch_type"))
  metric_results_matrix_list[[i]]$model_number <- paste("model_", i)
}

bounded_results_matrix <- metric_results_matrix_list %>%
  bind_rows() %>%
  mutate(across(1:25, as.character)) %>%
  mutate(across(1:25, as.integer))


head(bounded_results_matrix, 200)
```

    ## # A tibble: 200 x 52
    ##    postpitch_1 postpitch_2 postpitch_3 postpitch_4 postpitch_5 postpitch_6
    ##          <int>       <int>       <int>       <int>       <int>       <int>
    ##  1           1           0           0           0           0           0
    ##  2           0           1           0           0           0           0
    ##  3           0           0           1           0           0           0
    ##  4           0           0           0           1           0           0
    ##  5           0           0           0           0           1           0
    ##  6           0           0           0           0           0           1
    ##  7           0           0           0           0           0           0
    ##  8           0           0           0           0           0           0
    ##  9           0           0           0           0           0           0
    ## 10           0           0           0           0           0           0
    ## # ... with 190 more rows, and 46 more variables: postpitch_7 <int>,
    ## #   postpitch_8 <int>, postpitch_9 <int>, postpitch_10 <int>,
    ## #   postpitch_11 <int>, postpitch_12 <int>, postpitch_13 <int>,
    ## #   postpitch_14 <int>, postpitch_15 <int>, postpitch_16 <int>,
    ## #   postpitch_17 <int>, postpitch_18 <int>, postpitch_19 <int>,
    ## #   postpitch_20 <int>, postpitch_21 <int>, postpitch_22 <int>,
    ## #   postpitch_23 <int>, postpitch_24 <int>, postpitch_25 <int>, ...

``` r
dim(bounded_results_matrix)
```

    ## [1] 1450   52

Here, we would expand the predictions by individual game situations and
their respective WPA / EPA implications (pitch count, inning, pitch
number in the outing, number of base runners, runner positions, pitch
number in the at bat). However, for the sake of simplicity in this
model, we will move forward will reducing the variable list in the test
observations to those necessary for model predictions. We will also
ensure that both the training set and “test” predictions set possess the
same variable names and types:

``` r
final_metric_vars_list <- c(colnames(bounded_results_matrix), "postpitch", "zone_diff", "pitch_call", "play_result")
final_metric_vars_list <- final_metric_vars_list[final_metric_vars_list != c("pitch_split", "model_number")]
final_metric_vars_list
```

    ##  [1] "postpitch_1"        "postpitch_2"        "postpitch_3"       
    ##  [4] "postpitch_4"        "postpitch_5"        "postpitch_6"       
    ##  [7] "postpitch_7"        "postpitch_8"        "postpitch_9"       
    ## [10] "postpitch_10"       "postpitch_11"       "postpitch_12"      
    ## [13] "postpitch_13"       "postpitch_14"       "postpitch_15"      
    ## [16] "postpitch_16"       "postpitch_17"       "postpitch_18"      
    ## [19] "postpitch_19"       "postpitch_20"       "postpitch_21"      
    ## [22] "postpitch_22"       "postpitch_23"       "postpitch_24"      
    ## [25] "postpitch_25"       "player"             "batter_side"       
    ## [28] "tagged_pitch_type"  "ops"                "bb_per_ab"         
    ## [31] "so_per_ab"          "rel_speed"          "vert_rel_angle"    
    ## [34] "horz_rel_angle"     "spin_rate"          "spin_axis"         
    ## [37] "extension"          "vert_break"         "induced_vert_break"
    ## [40] "horz_break"         "zone_speed"         "vert_appr_angle"   
    ## [43] "horz_appr_angle"    "pre_era_pitch"      "pre_b_avg_pitch"   
    ## [46] "fip"                "opp_ops"            "wins_saves_app"    
    ## [49] "k_per_ip"           "whip"               "postpitch"         
    ## [52] "zone_diff"          "pitch_call"         "play_result"

``` r
final_metric_model_data <- metrics_reduced_model_data %>%
  mutate(postpitch = as.factor(postpitch)) %>%
  dummy_cols(select_columns = "postpitch") %>%
  select(final_metric_vars_list)
```

    ## Note: Using an external vector in selections is ambiguous.
    ## i Use `all_of(final_metric_vars_list)` instead of `final_metric_vars_list` to silence this message.
    ## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.

``` r
head(final_metric_model_data)
```

    ## # A tibble: 6 x 54
    ##   postpitch_1 postpitch_2 postpitch_3 postpitch_4 postpitch_5 postpitch_6
    ##         <int>       <int>       <int>       <int>       <int>       <int>
    ## 1           0           0           0           0           0           0
    ## 2           0           0           0           0           0           1
    ## 3           0           0           0           0           0           0
    ## 4           0           0           0           0           0           0
    ## 5           0           0           0           0           0           0
    ## 6           0           0           0           0           0           0
    ## # ... with 48 more variables: postpitch_7 <int>, postpitch_8 <int>,
    ## #   postpitch_9 <int>, postpitch_10 <int>, postpitch_11 <int>,
    ## #   postpitch_12 <int>, postpitch_13 <int>, postpitch_14 <int>,
    ## #   postpitch_15 <int>, postpitch_16 <int>, postpitch_17 <int>,
    ## #   postpitch_18 <int>, postpitch_19 <int>, postpitch_20 <int>,
    ## #   postpitch_21 <int>, postpitch_22 <int>, postpitch_23 <int>,
    ## #   postpitch_24 <int>, postpitch_25 <int>, player <chr>, ...

Next, I will create lists of ball zones and strike zones for use in the
two models’ applications:

``` r
ball_zone_list <- c(1:6, 10:11, 15:16, 20:25)
strike_zone_list <- c(7:9, 12:14, 17:19)
```

I will store the total zones per player in a data frame in anticipation
of the final metric calculation:

``` r
total_zones_by_pitcher <- metrics_reduced_model_data %>%
  filter(team == "Notre Dame") %>%
  group_by(player) %>%
  summarise(total_zone_diff = sum(zone_diff, na.rm = TRUE))
```

I will divide final metric model data into pitches missed in ball zones
and pitches missed in strike zones:

``` r
final_metric_ball_data <- final_metric_model_data %>%
  mutate(ball_called = if_else(pitch_call == "BallCalled", 1, 0)) %>%
  select(-c("pitch_call", "play_result")) %>%
  filter(zone_diff > 0 & postpitch %in% ball_zone_list) %>%
  select(-c("zone_diff", "postpitch")) %>%
  mutate(ball_called = as.factor(ball_called))


head(final_metric_ball_data)
```

    ## # A tibble: 6 x 51
    ##   postpitch_1 postpitch_2 postpitch_3 postpitch_4 postpitch_5 postpitch_6
    ##         <int>       <int>       <int>       <int>       <int>       <int>
    ## 1           0           0           0           0           0           1
    ## 2           0           0           0           0           0           0
    ## 3           0           1           0           0           0           0
    ## 4           0           0           0           0           0           0
    ## 5           0           0           0           0           0           0
    ## 6           0           0           0           0           0           0
    ## # ... with 45 more variables: postpitch_7 <int>, postpitch_8 <int>,
    ## #   postpitch_9 <int>, postpitch_10 <int>, postpitch_11 <int>,
    ## #   postpitch_12 <int>, postpitch_13 <int>, postpitch_14 <int>,
    ## #   postpitch_15 <int>, postpitch_16 <int>, postpitch_17 <int>,
    ## #   postpitch_18 <int>, postpitch_19 <int>, postpitch_20 <int>,
    ## #   postpitch_21 <int>, postpitch_22 <int>, postpitch_23 <int>,
    ## #   postpitch_24 <int>, postpitch_25 <int>, player <chr>, ...

``` r
unique(final_metric_model_data$pitch_call)
```

    ## [1] "InPlay"         "BallCalled"     "StrikeCalled"   "StrikeSwinging"
    ## [5] "FoulBall"       "HitByPitch"     "BallinDirt"

``` r
final_metric_strike_data <- final_metric_model_data %>%
  mutate(strike_hit = if_else(pitch_call %in% c("FoulBall", "InPlay"), 1, 0)) %>%
  select(-c("pitch_call", "play_result")) %>%
  filter(zone_diff > 0 & postpitch %in% strike_zone_list) %>%
  select(-c("zone_diff", "postpitch")) %>%
  mutate(strike_hit = as.factor(strike_hit))

head(final_metric_strike_data, 10)
```

    ## # A tibble: 10 x 51
    ##    postpitch_1 postpitch_2 postpitch_3 postpitch_4 postpitch_5 postpitch_6
    ##          <int>       <int>       <int>       <int>       <int>       <int>
    ##  1           0           0           0           0           0           0
    ##  2           0           0           0           0           0           0
    ##  3           0           0           0           0           0           0
    ##  4           0           0           0           0           0           0
    ##  5           0           0           0           0           0           0
    ##  6           0           0           0           0           0           0
    ##  7           0           0           0           0           0           0
    ##  8           0           0           0           0           0           0
    ##  9           0           0           0           0           0           0
    ## 10           0           0           0           0           0           0
    ## # ... with 45 more variables: postpitch_7 <int>, postpitch_8 <int>,
    ## #   postpitch_9 <int>, postpitch_10 <int>, postpitch_11 <int>,
    ## #   postpitch_12 <int>, postpitch_13 <int>, postpitch_14 <int>,
    ## #   postpitch_15 <int>, postpitch_16 <int>, postpitch_17 <int>,
    ## #   postpitch_18 <int>, postpitch_19 <int>, postpitch_20 <int>,
    ## #   postpitch_21 <int>, postpitch_22 <int>, postpitch_23 <int>,
    ## #   postpitch_24 <int>, postpitch_25 <int>, player <chr>, ...

``` r
bounded_results_matrix$preds <- as.numeric(rep(NA, nrow(bounded_results_matrix)))

head(bounded_results_matrix, 10)
```

    ## # A tibble: 10 x 53
    ##    postpitch_1 postpitch_2 postpitch_3 postpitch_4 postpitch_5 postpitch_6
    ##          <int>       <int>       <int>       <int>       <int>       <int>
    ##  1           1           0           0           0           0           0
    ##  2           0           1           0           0           0           0
    ##  3           0           0           1           0           0           0
    ##  4           0           0           0           1           0           0
    ##  5           0           0           0           0           1           0
    ##  6           0           0           0           0           0           1
    ##  7           0           0           0           0           0           0
    ##  8           0           0           0           0           0           0
    ##  9           0           0           0           0           0           0
    ## 10           0           0           0           0           0           0
    ## # ... with 47 more variables: postpitch_7 <int>, postpitch_8 <int>,
    ## #   postpitch_9 <int>, postpitch_10 <int>, postpitch_11 <int>,
    ## #   postpitch_12 <int>, postpitch_13 <int>, postpitch_14 <int>,
    ## #   postpitch_15 <int>, postpitch_16 <int>, postpitch_17 <int>,
    ## #   postpitch_18 <int>, postpitch_19 <int>, postpitch_20 <int>,
    ## #   postpitch_21 <int>, postpitch_22 <int>, postpitch_23 <int>,
    ## #   postpitch_24 <int>, postpitch_25 <int>, player <chr>, ...

As mentioned, the variables in the testing grid above must match the
variable types and names of the variables in the training set for
XGBoost. Please confirm before continuing.

# Model Building

## First Potential Model: XGBoost

If XGBoost is your preferred modeling technique within CZR, please
conduct the following small-scale test, evaluate the results, and
replace random forest with XGBoost in the iterative loop for CZR below

To begin, I will build two models to test the structure of the iterative
loop:

### Models 1 and 2 Conditions: Player = John Michael Bertrand, Pitch Type = Fastball, Batter = Right

Model 1: John Michael Bertrand, Fastball, RHH, Strike Zones

``` r
bertrand_model1_data <- final_metric_strike_data %>%
  filter(player == "John Michael Bertrand",
         batter_side == "Right",
         tagged_pitch_type == "Fastball") %>%
  select(-c(26:28)) %>%
  na_mean()

rownames(bertrand_model1_data) <- NULL

bertrand_model1_data
```

    ## # A tibble: 34 x 48
    ##    postpitch_1 postpitch_2 postpitch_3 postpitch_4 postpitch_5 postpitch_6
    ##          <int>       <int>       <int>       <int>       <int>       <int>
    ##  1           0           0           0           0           0           0
    ##  2           0           0           0           0           0           0
    ##  3           0           0           0           0           0           0
    ##  4           0           0           0           0           0           0
    ##  5           0           0           0           0           0           0
    ##  6           0           0           0           0           0           0
    ##  7           0           0           0           0           0           0
    ##  8           0           0           0           0           0           0
    ##  9           0           0           0           0           0           0
    ## 10           0           0           0           0           0           0
    ## # ... with 24 more rows, and 42 more variables: postpitch_7 <int>,
    ## #   postpitch_8 <int>, postpitch_9 <int>, postpitch_10 <int>,
    ## #   postpitch_11 <int>, postpitch_12 <int>, postpitch_13 <int>,
    ## #   postpitch_14 <int>, postpitch_15 <int>, postpitch_16 <int>,
    ## #   postpitch_17 <int>, postpitch_18 <int>, postpitch_19 <int>,
    ## #   postpitch_20 <int>, postpitch_21 <int>, postpitch_22 <int>,
    ## #   postpitch_23 <int>, postpitch_24 <int>, postpitch_25 <int>, ops <dbl>, ...

``` r
scale_weight = sum(bertrand_model1_data$strike_hit == 0) / sum(bertrand_model1_data$strike_hit == 1)
```

Build the training set, build the model, and evaluate the predictions on
the testing set (where eligible):

``` r
dtrain <- xgb.DMatrix(data = as.matrix(bertrand_model1_data[,-48]), 
                      label = as.numeric(bertrand_model1_data$strike_hit) -1)

set.seed(574)
bst_bertrand_1 <- xgboost(data = dtrain, 
               nrounds = 100,
               verbose = 1, 
               nthread = 1,
               print_every_n = 20, 
               objective = "binary:logistic", 
               eval_metric = "auc",
               eval_metric = "error",
               scale_pos_weight = scale_weight)
```

    ## [1]  train-auc:0.875458  train-error:0.235294 
    ## [21] train-auc:1.000000  train-error:0.029412 
    ## [41] train-auc:1.000000  train-error:0.000000 
    ## [61] train-auc:1.000000  train-error:0.000000 
    ## [81] train-auc:1.000000  train-error:0.000000 
    ## [100]    train-auc:1.000000  train-error:0.000000

Extract the variable importance to verify if XGBoost accounts for
pitching zones as important factors in the model (if not, random forest
may be a better fit in light of the data availability):

``` r
# Extract importance
imp_mat <- xgb.importance(model = bst_bertrand_1)
# Plot importance (top 10 variables)
xgb.plot.importance(imp_mat, 
                    top_n = 10, 
                    main = "John Michael Bertrand, Model 1: Variable Importance")
```

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Filter for the pertinent, eligible testing rows for Model 1’s
conditions:

``` r
test_row <- bounded_results_matrix %>%
  filter(player == "John Michael Bertrand",
         batter_side == "Right",
         tagged_pitch_type == "Fastball") %>%
  select(-c("preds", "player", "batter_side", "tagged_pitch_type", "model_number", "pitch_split"))  %>%
  filter(row_number() %in% strike_zone_list)

test_row$strike_hit <- as.factor(0)

test_row
```

    ## # A tibble: 9 x 48
    ##   postpitch_1 postpitch_2 postpitch_3 postpitch_4 postpitch_5 postpitch_6
    ##         <int>       <int>       <int>       <int>       <int>       <int>
    ## 1           0           0           0           0           0           0
    ## 2           0           0           0           0           0           0
    ## 3           0           0           0           0           0           0
    ## 4           0           0           0           0           0           0
    ## 5           0           0           0           0           0           0
    ## 6           0           0           0           0           0           0
    ## 7           0           0           0           0           0           0
    ## 8           0           0           0           0           0           0
    ## 9           0           0           0           0           0           0
    ## # ... with 42 more variables: postpitch_7 <int>, postpitch_8 <int>,
    ## #   postpitch_9 <int>, postpitch_10 <int>, postpitch_11 <int>,
    ## #   postpitch_12 <int>, postpitch_13 <int>, postpitch_14 <int>,
    ## #   postpitch_15 <int>, postpitch_16 <int>, postpitch_17 <int>,
    ## #   postpitch_18 <int>, postpitch_19 <int>, postpitch_20 <int>,
    ## #   postpitch_21 <int>, postpitch_22 <int>, postpitch_23 <int>,
    ## #   postpitch_24 <int>, postpitch_25 <int>, ops <dbl>, bb_per_ab <dbl>, ...

Apply the predictions (9 here):

``` r
dtest <- xgb.DMatrix(data = as.matrix(test_row[,-48]), 
                      label = as.numeric(test_row$strike_hit) -1)
xgb_preds_1 <- predict(bst_bertrand_1, dtest)

xgb_preds_1
```

    ## [1] 0.9245772 0.9245772 0.9245772 0.9245772 0.9245772 0.9245772 0.9245772
    ## [8] 0.9245772 0.9245772

Model 2: Bertrand, Fastball, RHH, Ball Zones

I will perform the same procedure on all pitches thrown in the ball
zones that match the Model 2 conditions:

``` r
bertrand_model2_data <- final_metric_ball_data %>%
  filter(player == "John Michael Bertrand",
         batter_side == "Right",
         tagged_pitch_type == "Fastball") %>%
  select(-c(26:28)) %>%
  na_mean()

rownames(bertrand_model2_data) <- NULL

bertrand_model2_data
```

    ## # A tibble: 30 x 48
    ##    postpitch_1 postpitch_2 postpitch_3 postpitch_4 postpitch_5 postpitch_6
    ##          <int>       <int>       <int>       <int>       <int>       <int>
    ##  1           0           0           0           0           0           0
    ##  2           0           0           0           0           0           0
    ##  3           0           0           0           0           1           0
    ##  4           0           1           0           0           0           0
    ##  5           0           0           0           0           0           1
    ##  6           0           0           0           0           0           0
    ##  7           0           0           0           0           0           0
    ##  8           0           0           0           0           0           0
    ##  9           0           0           0           0           0           0
    ## 10           0           0           0           0           0           0
    ## # ... with 20 more rows, and 42 more variables: postpitch_7 <int>,
    ## #   postpitch_8 <int>, postpitch_9 <int>, postpitch_10 <int>,
    ## #   postpitch_11 <int>, postpitch_12 <int>, postpitch_13 <int>,
    ## #   postpitch_14 <int>, postpitch_15 <int>, postpitch_16 <int>,
    ## #   postpitch_17 <int>, postpitch_18 <int>, postpitch_19 <int>,
    ## #   postpitch_20 <int>, postpitch_21 <int>, postpitch_22 <int>,
    ## #   postpitch_23 <int>, postpitch_24 <int>, postpitch_25 <int>, ops <dbl>, ...

``` r
scale_weight = sum(bertrand_model2_data$ball_called == 0) / sum(bertrand_model2_data$ball_called == 1)
```

Form the training set and build the ball called model:

``` r
dtrain <- xgb.DMatrix(data = as.matrix(bertrand_model2_data[,-48]), 
                      label = as.numeric(bertrand_model2_data$ball_called) -1)

set.seed(574)
bst_bertrand_2 <- xgboost(data = dtrain, 
               nrounds = 100,
               verbose = 1, 
               nthread = 1,
               print_every_n = 20, 
               objective = "binary:logistic", 
               eval_metric = "auc",
               eval_metric = "error",
               scale_pos_weight = scale_weight)
```

    ## [1]  train-auc:0.815000  train-error:0.333333 
    ## [21] train-auc:1.000000  train-error:0.033333 
    ## [41] train-auc:1.000000  train-error:0.000000 
    ## [61] train-auc:1.000000  train-error:0.000000 
    ## [81] train-auc:1.000000  train-error:0.000000 
    ## [100]    train-auc:1.000000  train-error:0.000000

As above, verify that certain zones possess an impact on the model (if
not, opt for random forest modeling to ensure zone impact):

``` r
# Extract importance
imp_mat_2 <- xgb.importance(model = bst_bertrand_2)
# Plot importance (top 10 variables)
xgb.plot.importance(imp_mat_2, 
                    top_n = 10, 
                    main = "John Michael Bertrand, Model 2: Variable Importance")
```

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

Detect the pertinent, eligible testing rows, and apply the testing rows
on the model for the sake of prediction scores:

``` r
test_row_2 <- bounded_results_matrix %>%
  filter(player == "John Michael Bertrand",
         batter_side == "Right",
         tagged_pitch_type == "Fastball") %>%
  filter(row_number() %in% ball_zone_list) %>%
  select(-c("preds", "player", "batter_side", "tagged_pitch_type", "model_number", "pitch_split")) 

test_row_2$ball_called <- as.factor(0)

test_row_2
```

    ## # A tibble: 16 x 48
    ##    postpitch_1 postpitch_2 postpitch_3 postpitch_4 postpitch_5 postpitch_6
    ##          <int>       <int>       <int>       <int>       <int>       <int>
    ##  1           1           0           0           0           0           0
    ##  2           0           1           0           0           0           0
    ##  3           0           0           1           0           0           0
    ##  4           0           0           0           1           0           0
    ##  5           0           0           0           0           1           0
    ##  6           0           0           0           0           0           1
    ##  7           0           0           0           0           0           0
    ##  8           0           0           0           0           0           0
    ##  9           0           0           0           0           0           0
    ## 10           0           0           0           0           0           0
    ## 11           0           0           0           0           0           0
    ## 12           0           0           0           0           0           0
    ## 13           0           0           0           0           0           0
    ## 14           0           0           0           0           0           0
    ## 15           0           0           0           0           0           0
    ## 16           0           0           0           0           0           0
    ## # ... with 42 more variables: postpitch_7 <int>, postpitch_8 <int>,
    ## #   postpitch_9 <int>, postpitch_10 <int>, postpitch_11 <int>,
    ## #   postpitch_12 <int>, postpitch_13 <int>, postpitch_14 <int>,
    ## #   postpitch_15 <int>, postpitch_16 <int>, postpitch_17 <int>,
    ## #   postpitch_18 <int>, postpitch_19 <int>, postpitch_20 <int>,
    ## #   postpitch_21 <int>, postpitch_22 <int>, postpitch_23 <int>,
    ## #   postpitch_24 <int>, postpitch_25 <int>, ops <dbl>, bb_per_ab <dbl>, ...

Calculate the prediction scores (16 out of 16 eligible in this case due
to factorless approach in XGBoost):

``` r
dtest <- xgb.DMatrix(data = as.matrix(test_row_2[,-48]), 
                      label = as.numeric(test_row_2$ball_called) -1)
xgb_preds_1 <- predict(bst_bertrand_2, dtest)

xgb_preds_1
```

    ##  [1] 0.4444663 0.4444663 0.4444663 0.4444663 0.4444663 0.4444663 0.4444663
    ##  [8] 0.4444663 0.4444663 0.4444663 0.4444663 0.4444663 0.4444663 0.4444663
    ## [15] 0.4444663 0.4444663

## Second Model Option: Random Forest

To ensure that zone locations generate an impact in the model’s
structure, random forest modeling may be beneficial for CZR
calculations. Here, we investigate the same model conditions (Bertrand,
fastball, RHH, Strike Zones and Ball Zones) for Models 1 and 2,
respectively, noticing the prediction score adjustments when accounting
for zone locations.

We form the data sets, build the first model, and test which of the
eligible rows in the testing set correspond to any observations in the
training set. If no rows in the training set correspond to a pitch in a
testing row’s respective zone, remove that row from the data frame of
testing rows for the Model 1 conditions and continue:

``` r
bertrand_model1_data <- bertrand_model1_data %>%
  mutate(across(1:25, as.factor))

bertrand_model2_data <- bertrand_model2_data %>%
  mutate(across(1:25, as.factor))

set.seed(574)
bertrand_rf_mod_1 <- randomForest(strike_hit ~., 
                         data = bertrand_model1_data, 
                         ntree = 100,
                         nodesize = 1,
                         mtry = 47) 

test_row <- bounded_results_matrix %>%
  filter(player == "John Michael Bertrand",
         batter_side == "Right",
         tagged_pitch_type == "Fastball") %>%
  select(-c("preds", "player", "batter_side", "tagged_pitch_type", "model_number", "pitch_split"))

bertrand_model1_data <- bertrand_model1_data %>%
  mutate(across(1:25, as.character)) %>%
  mutate(across(1:25, as.integer))

test_row$strike_hit <- as.factor(0)

row_list <- vector()

for(i in 1:25){
  if(sum(bertrand_model1_data[, i]) == 0){
    row_list <- append(row_list, i)
    }
}

test_row <- test_row[-row_list, ]

test_row <- test_row %>%
  mutate(across(1:25, as.factor))
```

Applying the model on the remaining testing rows and evaluating the
prediction scores (9 out of 9 in this case):

``` r
rf_bertrand_preds <- predict(bertrand_rf_mod_1, test_row, type = "prob") 

rf_bertrand_preds
```

    ##      0    1
    ## 1 0.22 0.78
    ## 2 0.21 0.79
    ## 3 0.21 0.79
    ## 4 0.19 0.81
    ## 5 0.20 0.80
    ## 6 0.21 0.79
    ## 7 0.21 0.79
    ## 8 0.21 0.79
    ## 9 0.27 0.73
    ## attr(,"class")
    ## [1] "matrix" "array"  "votes"

``` r
nrow(rf_bertrand_preds)
```

    ## [1] 9

We build the second model and test which of the eligible rows in the
testing set correspond to any observations in the training set. If no
rows in the training set correspond to a pitch in a testing row’s
respective zone, remove that row from the data frame of testing rows for
the Model 2 conditions and continue:

``` r
set.seed(574)
bertrand_rf_mod_2 <- randomForest(ball_called ~., 
                         data = bertrand_model2_data, 
                         ntree = 100,
                         nodesize = 1,
                         mtry = 47) 

bertrand_model2_data <- bertrand_model2_data %>%
  mutate(across(1:25, as.character)) %>%
  mutate(across(1:25, as.integer))

test_row_2 <- bounded_results_matrix %>%
  filter(player == "John Michael Bertrand",
         batter_side == "Right",
         tagged_pitch_type == "Fastball") %>%
  select(-c("preds", "player", "batter_side", "tagged_pitch_type", "model_number", "pitch_split")) 

test_row_2$ball_called <- as.factor(0)

test_row_2
```

    ## # A tibble: 25 x 48
    ##    postpitch_1 postpitch_2 postpitch_3 postpitch_4 postpitch_5 postpitch_6
    ##          <int>       <int>       <int>       <int>       <int>       <int>
    ##  1           1           0           0           0           0           0
    ##  2           0           1           0           0           0           0
    ##  3           0           0           1           0           0           0
    ##  4           0           0           0           1           0           0
    ##  5           0           0           0           0           1           0
    ##  6           0           0           0           0           0           1
    ##  7           0           0           0           0           0           0
    ##  8           0           0           0           0           0           0
    ##  9           0           0           0           0           0           0
    ## 10           0           0           0           0           0           0
    ## # ... with 15 more rows, and 42 more variables: postpitch_7 <int>,
    ## #   postpitch_8 <int>, postpitch_9 <int>, postpitch_10 <int>,
    ## #   postpitch_11 <int>, postpitch_12 <int>, postpitch_13 <int>,
    ## #   postpitch_14 <int>, postpitch_15 <int>, postpitch_16 <int>,
    ## #   postpitch_17 <int>, postpitch_18 <int>, postpitch_19 <int>,
    ## #   postpitch_20 <int>, postpitch_21 <int>, postpitch_22 <int>,
    ## #   postpitch_23 <int>, postpitch_24 <int>, postpitch_25 <int>, ops <dbl>, ...

``` r
row_list <- vector()

for(i in 1:25){
  if(sum(bertrand_model2_data[, i]) == 0){
    row_list <- append(row_list, i)
    }
}

test_row_2 <- test_row_2[-row_list, ]

test_row_2 <- test_row_2 %>%
  mutate(across(1:25, as.factor))
```

Applying the model on the remaining testing rows for predictions, and
evaluating the prediction scores:

``` r
rf_bertrand_preds_2 <- predict(bertrand_rf_mod_2, test_row_2, type = "prob") 

rf_bertrand_preds_2[, 2]
```

    ##    1    2    3    4    5    6    7    8    9   10   11   12 
    ## 0.77 0.80 0.80 0.80 0.78 0.73 0.80 0.79 0.80 0.80 0.74 0.80

``` r
nrow(rf_bertrand_preds_2)
```

    ## [1] 12

``` r
zones_list <- seq(1, 25, 1)
cbind()
```

    ## NULL

# Part 1: Given Thrown In Strike Zone, Base Hit Probabilities

Now that one case of successful predictions has been conducted, the
above calculation can be repeated in a nested for loop for all other
Pitcher, Pitch Type, and Batter Side combinations in the data set for
Notre Dame pitchers. The following for loop conducts the following for
Strike Zone pitches:

1.  Iterate through Pitchers, Batter Sides, and Pitch Types, building a
    model on the corresponding observations for each combination

2.  Replace infinite WHIP values with a manageable replacement proxy
    (near 100)

3.  Test if the model contains enough data to build a tree ensemble. If
    only one class exists in the response value, the model’s conditions
    fail, and a given model is skipped. All predictions are negated.

4.  Test if a testing row’s corresponding zone is in the training set.
    If no association exists, remove the row from a given model’s
    corresponding testing rows and continue.

5.  Append the model predictions to the matrix of bounded testing
    results, one for each zone. These prediction scores, along with the
    total misses per zone and total pitches thrown in a zone as grouped
    by the model conditions, will be used to calculate the Command Score
    (the impact component of CZR).

All models will be either skipped or built for immediate application on
corresponding available test rows (in this case, limited to the 9
eligible strike zones):

``` r
bounded_individual_pitchers_strike <- vector("list", dim(bounded_results_matrix)[1] / 25)

identifiers <- bounded_results_matrix %>%
  select(26:28) %>%
  distinct() %>%
  mutate(batter_side = as.character(batter_side),
         tagged_pitch_type = as.character(tagged_pitch_type))

player_list <- as.list.data.frame(identifiers$player)

batter_list <- as.list.data.frame(identifiers$batter_side)

pitch_list <- as.list.data.frame(identifiers$tagged_pitch_type)

for(i in 1:length(player_list)){
      strike_model_data <- final_metric_strike_data %>%
        filter(player == player_list[i],
               as.character(batter_side) == batter_list[i],
               as.character(tagged_pitch_type) == pitch_list[i]) %>%
        select(-c(26:28)) %>%
        na_mean() %>%
        mutate(whip = replace(whip, whip == Inf, 99))
      
      strike_model_data_int <- strike_model_data %>%
        mutate(strike_hit = as.character(strike_hit),
               strike_hit = as.integer(strike_hit))
      
      if(sum(strike_model_data_int$strike_hit) == 0 | sum(strike_model_data_int$strike_hit) == nrow(strike_model_data_int)){
        cat("Not Enough Data for ", player_list[i], ", ", batter_list[i], ", ", pitch_list[i], "Strike Model")
        cat("\n")
        next
      } else{
        print("Model Satisfies Conditions")
        cat("\n")
        rownames(strike_model_data) <- NULL
      
      strike_model_data <- strike_model_data %>%
        mutate(across(1:25, as.factor))
      
      wn = sum(strike_model_data$strike_hit == 0)/nrow(strike_model_data)
      wy = sum(strike_model_data$strike_hit == 1)/nrow(strike_model_data)
    
      set.seed(574)
      strike_rf_model <- randomForest(strike_hit ~., 
                         data = strike_model_data, 
                         ntree = 100,
                         nodesize = 1,
                         mtry = 47,
                         classwt = c(wn, wy)) 
      
      test_row <- bounded_results_matrix %>%
       filter(player == player_list[i],
               as.character(batter_side) == batter_list[i],
               as.character(tagged_pitch_type) == pitch_list[i]) %>%
        select(-c("preds", "player", "batter_side", "tagged_pitch_type", "model_number", "pitch_split"))

      strike_model_data <- strike_model_data %>%
        mutate(across(1:25, as.character)) %>%
        mutate(across(1:25, as.integer))

      test_row$strike_hit <- as.factor(0)

      row_list <- vector()

      for(x in 1:25){
        if(sum(strike_model_data[, x]) == 0){
          row_list <- append(row_list, x)
          }
      }
      
      test_row$index <- seq(1, 25)
      
      test_row <- test_row[-row_list, ]

      test_row <- test_row %>%
        mutate(across(1:25, as.factor))
      
      test_row_no_index <- test_row %>%
        select(-index)
      
      strike_model_preds <- predict(strike_rf_model, test_row_no_index, type = "prob")
      
      bounded_preds <- cbind(test_row, strike_model_preds[ ,2])
      
      colnames(bounded_preds) <- c(colnames(test_row), "model_preds")
      
      bounded_individual_pitchers_strike[[i]] <- bounded_results_matrix %>%
       filter(player == player_list[i],
               as.character(batter_side) == batter_list[i],
               as.character(tagged_pitch_type) == pitch_list[i])
      
      for(z in 1:25){
        if(z %in% row_list){
           bounded_individual_pitchers_strike[[i]][z, 53] <- NA
        } else{
           bounded_individual_pitchers_strike[[i]][z, 53] <- as.list(bounded_preds[which(bounded_preds$index == z), 50])
        }
      }
      }
}
```

    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## Not Enough Data for  Aidan Tyrell ,  Right ,  Slider Strike Model
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## Not Enough Data for  Jack Brannigan ,  Left ,  Slider Strike Model
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"

    ## Warning: imputeTS: No imputation performed for column 41 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 42 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 43 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 44 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 45 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 46 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 47 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Not Enough Data for  Liam Simmon ,  Left ,  Fastball Strike Model

    ## Warning: imputeTS: No imputation performed for column 41 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 42 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 43 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 44 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 45 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 46 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 47 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Not Enough Data for  Liam Simmon ,  Right ,  Fastball Strike Model
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## Not Enough Data for  Liam Simon ,  Left ,  Slider Strike Model
    ## [1] "Model Satisfies Conditions"

    ## Warning: imputeTS: No imputation performed for column 41 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 42 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 43 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 44 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 45 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 46 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 47 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Not Enough Data for  Matt Lazzaro ,  Left ,  Curveball Strike Model

    ## Warning: imputeTS: No imputation performed for column 26 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 27 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 28 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 41 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 42 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 43 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 44 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 45 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 46 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 47 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Not Enough Data for  Matt Lazzaro ,  Right ,  Curveball Strike Model
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## Not Enough Data for  Radek Birkholz ,  Left ,  Slider Strike Model
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## Not Enough Data for  Roman Kimball ,  Left ,  Slider Strike Model
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## Not Enough Data for  Ryan McLinskey ,  Left ,  Slider Strike Model
    ## [1] "Model Satisfies Conditions"
    ## 
    ## Not Enough Data for  Sammy Cooper ,  Left ,  Fastball Strike Model
    ## Not Enough Data for  Sammy Cooper ,  Right ,  Fastball Strike Model
    ## Not Enough Data for  Sammy Cooper ,  Left ,  Slider Strike Model
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## Not Enough Data for  Will Mercer ,  Left ,  Slider Strike Model
    ## [1] "Model Satisfies Conditions"

``` r
final_bounded_results_matrix_strike <- bounded_individual_pitchers_strike %>%
  bind_rows() %>%
  filter(!is.na(preds))

colnames(final_bounded_results_matrix_strike) <- str_replace(colnames(final_bounded_results_matrix_strike), "postpitch_", "")

final_bounded_results_matrix_strike <- final_bounded_results_matrix_strike %>%
  pivot_longer(1:25, names_to = "column", values_to = "in_zone") %>%
  filter(in_zone != 0) %>%
  select(-in_zone) %>%
  rename("zone" = column) %>%
  mutate(zone = as.integer(zone)) %>%
  select(zone, player, batter_side, tagged_pitch_type, pitch_split, preds)
```

# Part 2: Given Thrown Outside Strike Zone, Ball Called Probabilities

The for loop below conducts the following for Strike Zone pitches:

1.  Iterate through Pitchers, Batter Sides, and Pitch Types, building a
    model on the corresponding observations for each combination

2.  Replace infinite WHIP values with a manageable replacement proxy
    (near 100)

3.  Test if the model contains enough data to build a tree ensemble. If
    only one class exists in the response value, the model’s conditions
    fail, and a given model is skipped. All predictions are negated.

4.  Test if a testing row’s corresponding zone is in the training set.
    If no association exists, remove the row from a given model’s
    corresponding testing rows and continue.

5.  Append the model predictions to the matrix of bounded testing
    results, one for each zone. These prediction scores, along with the
    total misses per zone and total pitches thrown in a zone as grouped
    by the model conditions, will be used to calculate the Command Score
    (the impact component of CZR).

All models will be either skipped or built for immediate application on
corresponding available test rows (in this case, limited to the 16
eligible ball zones):

``` r
bounded_individual_pitchers_ball <- vector("list", dim(bounded_results_matrix)[1] / 25)

identifiers <- bounded_results_matrix %>%
  select(26:28) %>%
  distinct() %>%
  mutate(batter_side = as.character(batter_side),
         tagged_pitch_type = as.character(tagged_pitch_type))

player_list <- as.list.data.frame(identifiers$player)

batter_list <- as.list.data.frame(identifiers$batter_side)

pitch_list <- as.list.data.frame(identifiers$tagged_pitch_type)

for(i in 1:length(player_list)){
      ball_model_data <- final_metric_ball_data %>%
        filter(player == player_list[i],
               as.character(batter_side) == batter_list[i],
               as.character(tagged_pitch_type) == pitch_list[i]) %>%
        select(-c(26:28)) %>%
        na_mean() %>%
        mutate(whip = replace(whip, whip == Inf, 99))
      
      ball_model_data_int <- ball_model_data %>%
        mutate(ball_called = as.character(ball_called),
               ball_called = as.integer(ball_called))
      
      if(sum(ball_model_data_int$ball_called) == 0 | sum(ball_model_data_int$ball_called) == nrow(ball_model_data_int)){
        cat("Not Enough Data for ", player_list[i], ", ", batter_list[i], ", ", pitch_list[i], "Ball Model")
        cat("\n")
        next
      } else{
        print("Model Satisfies Conditions")
        cat("\n")
        rownames(ball_model_data) <- NULL
      
      ball_model_data <- ball_model_data %>%
        mutate(across(1:25, as.factor))
      
      wn = sum(ball_model_data$ball_called == 0)/nrow(ball_model_data)
      wy = sum(ball_model_data$ball_called == 1)/nrow(ball_model_data)
    
      set.seed(574)
      ball_rf_model <- randomForest(ball_called ~., 
                         data = ball_model_data, 
                         ntree = 100,
                         nodesize = 1,
                         mtry = 47,
                         classwt = c(wn, wy)) 
      
      test_row <- bounded_results_matrix %>%
       filter(player == player_list[i],
               as.character(batter_side) == batter_list[i],
               as.character(tagged_pitch_type) == pitch_list[i]) %>%
        select(-c("preds", "player", "batter_side", "tagged_pitch_type", "model_number", "pitch_split"))

      ball_model_data <- ball_model_data %>%
        mutate(across(1:25, as.character)) %>%
        mutate(across(1:25, as.integer))

      test_row$ball_called <- as.factor(0)

      row_list <- vector()

      for(x in 1:25){
        if(sum(ball_model_data[, x]) == 0){
          row_list <- append(row_list, x)
          }
      }
      
      test_row$index <- seq(1, 25)
      
      test_row <- test_row[-row_list, ]

      test_row <- test_row %>%
        mutate(across(1:25, as.factor))
      
      test_row_no_index <- test_row %>%
        select(-index)
      
      ball_model_preds <- predict(ball_rf_model, test_row_no_index, type = "prob")
      
      bounded_preds <- cbind(test_row, ball_model_preds[ ,2])
      
      colnames(bounded_preds) <- c(colnames(test_row), "model_preds")
      
      bounded_individual_pitchers_ball[[i]] <- bounded_results_matrix %>%
       filter(player == player_list[i],
               as.character(batter_side) == batter_list[i],
               as.character(tagged_pitch_type) == pitch_list[i])
      
      for(z in 1:25){
        if(z %in% row_list){
           bounded_individual_pitchers_ball[[i]][z, 53] <- NA
        } else{
           bounded_individual_pitchers_ball[[i]][z, 53] <- as.list(bounded_preds[which(bounded_preds$index == z), 50])
        }
      }
      }
}
```

    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## Not Enough Data for  Jack Brannigan ,  Left ,  Slider Ball Model
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"

    ## Warning: imputeTS: No imputation performed for column 41 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 42 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 43 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 44 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 45 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 46 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 47 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Not Enough Data for  Liam Simmon ,  Left ,  Fastball Ball Model

    ## Warning: imputeTS: No imputation performed for column 41 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 42 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 43 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 44 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 45 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 46 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Warning: imputeTS: No imputation performed for column 47 because of this Error in na_mean(data[, i], option, maxgap): Input data has only NAs. Input data needs at least 1 non-NA data point for applying na_mean

    ## Not Enough Data for  Liam Simmon ,  Right ,  Fastball Ball Model
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## Not Enough Data for  Matt Lazzaro ,  Left ,  Curveball Ball Model
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## Not Enough Data for  Ryan McLinskey ,  Left ,  Slider Ball Model
    ## [1] "Model Satisfies Conditions"
    ## 
    ## Not Enough Data for  Sammy Cooper ,  Left ,  Fastball Ball Model
    ## Not Enough Data for  Sammy Cooper ,  Right ,  Fastball Ball Model
    ## Not Enough Data for  Sammy Cooper ,  Left ,  Slider Ball Model
    ## Not Enough Data for  Sammy Cooper ,  Right ,  Slider Ball Model
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"
    ## 
    ## [1] "Model Satisfies Conditions"

``` r
final_bounded_results_matrix_ball <- bounded_individual_pitchers_ball %>%
  bind_rows() %>%
  filter(!is.na(preds))

colnames(final_bounded_results_matrix_ball) <- str_replace(colnames(final_bounded_results_matrix_ball), "postpitch_", "")

final_bounded_results_matrix_ball <- final_bounded_results_matrix_ball %>%
  pivot_longer(1:25, names_to = "column", values_to = "in_zone") %>%
  filter(in_zone != 0) %>%
  select(-in_zone) %>%
  rename("zone" = column) %>%
  mutate(zone = as.integer(zone)) %>%
  select(zone, player, batter_side, tagged_pitch_type, pitch_split, preds)
```

# Command Zone Rating: The Calculation

## Component 1: Magnitude - Average Zone Difference

The magnitude component of CZR comes in the form of average zone
differential by pitcher across all pitches thrown, regardless of zone.
This stand-alone value is calculated and appended to a table in the code
below:

``` r
final_zone_difference_table <- metrics_reduced_model_data %>%
  filter(team == "Notre Dame") %>%
  group_by(player) %>%
  mutate(total_zone_diff = sum(zone_diff, na.rm = TRUE)) %>%
  mutate(total_pitches = n()) %>%
  select(player, total_zone_diff, total_pitches) %>%
  distinct() %>%
  mutate(zone_diff_per_pitch = total_zone_diff / total_pitches) %>%
  arrange(zone_diff_per_pitch)
```

## Component 2: Impact - Command Score

The average impact of a pitch thrown by pitchers is found by calculating
the expected value of balls called and strikes hit for contact across
all missed pitches, divided by the total number of pitches thrown across
eligible pitcher, batter side, and pitch type combinations (those
through which a model was built).

Command Score requires the total number of missed pitches in each zone
by batter side and pitch type for all eligible pitchers, as stored
below:

``` r
final_misses_thrown_by_zone_side_type <- metrics_reduced_model_data %>%
  filter(team == "Notre Dame") %>%
  filter(zone_diff > 0) %>%
  group_by(player, batter_side, tagged_pitch_type, postpitch) %>%
  summarise(total_misses_by_zone_side_type = n()) %>%
  drop_na() %>%
  rename("zone" = postpitch)
```

    ## `summarise()` has grouped output by 'player', 'batter_side', 'tagged_pitch_type'. You can override using the `.groups` argument.

The Score also requires the total number of pitches thrown in a zone by
batter side and tagged pitch type, regardless of it being classified by
a miss in the model:

``` r
final_pitches_thrown_by_zone_side_type <- metrics_reduced_model_data %>%
  filter(team == "Notre Dame") %>%
  group_by(player, batter_side, tagged_pitch_type, postpitch) %>%
  summarise(total_pitches_by_zone_side_type = n()) %>%
  drop_na() %>%
  rename("zone" = postpitch)
```

    ## `summarise()` has grouped output by 'player', 'batter_side', 'tagged_pitch_type'. You can override using the `.groups` argument.

We are able to gather all of our necessary components for CZR into one
dataframe through nested joins, as performed here on the final matrices
of bounded predictions from the models built on pitcher, batter side,
and pitch type combinations above:

``` r
final_bounded_results_matrix_strike <- final_bounded_results_matrix_strike %>%
  left_join(final_zone_difference_table, 
            by = "player") %>%
  left_join(final_misses_thrown_by_zone_side_type,
            by = c("player", "batter_side", "tagged_pitch_type", "zone")) %>%
  left_join(final_pitches_thrown_by_zone_side_type,
            by = c("player", "batter_side", "tagged_pitch_type", "zone")) %>%
  select(-c("total_pitches", "pitch_split"))

final_bounded_results_matrix_ball <- final_bounded_results_matrix_ball %>%
  left_join(final_zone_difference_table, 
            by = "player") %>%
  left_join(final_misses_thrown_by_zone_side_type,
            by = c("player", "batter_side", "tagged_pitch_type", "zone")) %>%
  left_join(final_pitches_thrown_by_zone_side_type,
            by = c("player", "batter_side", "tagged_pitch_type", "zone")) %>%
  select(-c("total_pitches", "pitch_split"))

head(final_bounded_results_matrix_strike, 20)
```

    ## # A tibble: 20 x 9
    ##     zone player       batter_side tagged_pitch_type preds total_zone_diff
    ##    <int> <chr>        <fct>       <fct>             <dbl>           <int>
    ##  1     7 Aidan Tyrell Left        Fastball           0.39             268
    ##  2     9 Aidan Tyrell Left        Fastball           0.39             268
    ##  3    13 Aidan Tyrell Left        Fastball           0.39             268
    ##  4    18 Aidan Tyrell Left        Fastball           0.39             268
    ##  5     8 Aidan Tyrell Right       Fastball           0.45             268
    ##  6    12 Aidan Tyrell Right       Fastball           0.45             268
    ##  7    13 Aidan Tyrell Right       Fastball           0.45             268
    ##  8    14 Aidan Tyrell Right       Fastball           0.45             268
    ##  9    17 Aidan Tyrell Right       Fastball           0.45             268
    ## 10    18 Aidan Tyrell Right       Fastball           0.45             268
    ## 11    19 Aidan Tyrell Right       Fastball           0.45             268
    ## 12     7 Aidan Tyrell Left        Slider             0.38             268
    ## 13     9 Aidan Tyrell Left        Slider             0.4              268
    ## 14    14 Aidan Tyrell Left        Slider             0.4              268
    ## 15    17 Aidan Tyrell Left        Slider             0.4              268
    ## 16    18 Aidan Tyrell Left        Slider             0.4              268
    ## 17    19 Aidan Tyrell Left        Slider             0.4              268
    ## 18    12 Alex Rao     Left        ChangeUp           0.11             273
    ## 19    14 Alex Rao     Left        ChangeUp           0.11             273
    ## 20    18 Alex Rao     Left        ChangeUp           0.11             273
    ## # ... with 3 more variables: zone_diff_per_pitch <dbl>,
    ## #   total_misses_by_zone_side_type <int>, total_pitches_by_zone_side_type <int>

``` r
head(final_bounded_results_matrix_ball, 50)
```

    ## # A tibble: 50 x 9
    ##     zone player       batter_side tagged_pitch_type preds total_zone_diff
    ##    <int> <chr>        <fct>       <fct>             <dbl>           <int>
    ##  1     3 Aidan Tyrell Left        Fastball           0.98             268
    ##  2     4 Aidan Tyrell Left        Fastball           0.98             268
    ##  3     5 Aidan Tyrell Left        Fastball           0.98             268
    ##  4    10 Aidan Tyrell Left        Fastball           0.96             268
    ##  5    11 Aidan Tyrell Left        Fastball           0.98             268
    ##  6    15 Aidan Tyrell Left        Fastball           0.98             268
    ##  7    20 Aidan Tyrell Left        Fastball           0.98             268
    ##  8    22 Aidan Tyrell Left        Fastball           0.98             268
    ##  9    24 Aidan Tyrell Left        Fastball           0.98             268
    ## 10    25 Aidan Tyrell Left        Fastball           0.98             268
    ## # ... with 40 more rows, and 3 more variables: zone_diff_per_pitch <dbl>,
    ## #   total_misses_by_zone_side_type <int>, total_pitches_by_zone_side_type <int>

``` r
final_bounded_results <- rbind(final_bounded_results_matrix_ball, final_bounded_results_matrix_strike)
final_bounded_results <- final_bounded_results %>%
  arrange(player)

head(final_bounded_results)
```

    ## # A tibble: 6 x 9
    ##    zone player       batter_side tagged_pitch_type preds total_zone_diff
    ##   <int> <chr>        <fct>       <fct>             <dbl>           <int>
    ## 1     3 Aidan Tyrell Left        Fastball           0.98             268
    ## 2     4 Aidan Tyrell Left        Fastball           0.98             268
    ## 3     5 Aidan Tyrell Left        Fastball           0.98             268
    ## 4    10 Aidan Tyrell Left        Fastball           0.96             268
    ## 5    11 Aidan Tyrell Left        Fastball           0.98             268
    ## 6    15 Aidan Tyrell Left        Fastball           0.98             268
    ## # ... with 3 more variables: zone_diff_per_pitch <dbl>,
    ## #   total_misses_by_zone_side_type <int>, total_pitches_by_zone_side_type <int>

The above data frame provides all individual pieces of CZR. To calculate
Command Score, multiply the predictions by their corresponding
quantities of missed pitches within the zone, and sum the products
together in the form of a linear combination (Total Probability)

``` r
final_metric_command_scores <- final_bounded_results %>%
  mutate(command_score_total = (preds * total_misses_by_zone_side_type)) %>%
  group_by(player) %>%
  summarise(total_probability = sum(command_score_total))
```

Find the total number of pitches thrown across eligible pitch type,
player, batter side combinations, storing the sums in a table for the
CZR calculation:

``` r
final_metric_command_scores_scales <- final_bounded_results%>%
  group_by(player) %>%
  summarise(total_pitches = sum(total_pitches_by_zone_side_type))
```

The following code calculates the Command Score for each pitcher by
dividing the total expected balls called and strikes for contact on
misses by the total number of pitches thrown.

We also calculate the Command Zone Rating (CZR) by multiplying the
Command Score of a given pitcher (the Impact) by their corresponding
Average Zone Differential per pitch (the Magnitude):

``` r
final_czr_table <- final_bounded_results %>%
  left_join(final_metric_command_scores,
            by = "player") %>%
  left_join(final_metric_command_scores_scales,
            by = "player") %>%
  select(-c(total_misses_by_zone_side_type, total_pitches_by_zone_side_type, zone, batter_side, tagged_pitch_type, total_zone_diff, preds)) %>%
  distinct() %>%
  mutate(command_score = total_probability / total_pitches,
         command_zone_rating = command_score * zone_diff_per_pitch) %>%
  select(player, command_score, zone_diff_per_pitch, command_zone_rating) %>%
  arrange(command_zone_rating)

final_czr_table
```

    ## # A tibble: 14 x 4
    ##    player                command_score zone_diff_per_pitch command_zone_rating
    ##    <chr>                         <dbl>               <dbl>               <dbl>
    ##  1 Liam Simon                    0.409                2.43               0.994
    ##  2 Radek Birkholz                0.406                2.58               1.04 
    ##  3 Roman Kimball                 0.466                2.30               1.07 
    ##  4 Jackson Dennies               0.486                2.38               1.16 
    ##  5 Jack Brannigan                0.523                2.37               1.24 
    ##  6 Austin Temple                 0.586                2.16               1.26 
    ##  7 Jack Findlay                  0.578                2.22               1.28 
    ##  8 Alex Rao                      0.553                2.35               1.30 
    ##  9 Ryan McLinskey                0.632                2.30               1.46 
    ## 10 John Michael Bertrand         0.610                2.48               1.51 
    ## 11 Will Mercer                   0.683                2.21               1.51 
    ## 12 Aidan Tyrell                  0.642                2.39               1.54 
    ## 13 Sammy Cooper                  0.75                 2.08               1.56 
    ## 14 Matt Lazzaro                  0.697                2.47               1.72

``` r
final_czr_table_mean <- final_czr_table %>%
  summarize(mean_czr = mean(command_zone_rating))

final_czr_table$mean_czr <- rep(final_czr_table_mean$mean_czr[1], nrow(final_czr_table))
```

While the raw scores of CZR may prove satisfactory, normalizing the
results on a 100 scale may make the CZR scores more interpretable.
Hence, we find the CZR+ scores here:

``` r
final_czr_table <- final_czr_table %>%
  mutate(command_zone_rating_normalized = round(command_zone_rating * 100 / mean_czr, 0)) %>%
  select(-mean_czr)

final_czr_table
```

    ## # A tibble: 14 x 5
    ##    player    command_score zone_diff_per_pi~ command_zone_ra~ command_zone_rati~
    ##    <chr>             <dbl>             <dbl>            <dbl>              <dbl>
    ##  1 Liam Sim~         0.409              2.43            0.994                 75
    ##  2 Radek Bi~         0.406              2.58            1.04                  78
    ##  3 Roman Ki~         0.466              2.30            1.07                  80
    ##  4 Jackson ~         0.486              2.38            1.16                  87
    ##  5 Jack Bra~         0.523              2.37            1.24                  93
    ##  6 Austin T~         0.586              2.16            1.26                  95
    ##  7 Jack Fin~         0.578              2.22            1.28                  96
    ##  8 Alex Rao          0.553              2.35            1.30                  98
    ##  9 Ryan McL~         0.632              2.30            1.46                 109
    ## 10 John Mic~         0.610              2.48            1.51                 113
    ## 11 Will Mer~         0.683              2.21            1.51                 114
    ## 12 Aidan Ty~         0.642              2.39            1.54                 115
    ## 13 Sammy Co~         0.75               2.08            1.56                 117
    ## 14 Matt Laz~         0.697              2.47            1.72                 129

As such, scores below 100 correspond to above-average command. Scores
above 100 correspond to below-average command.

We can arrange this final table by each component of the CZR (Impact and
Magnitude):

``` r
final_czr_table %>%
  arrange(command_score)
```

    ## # A tibble: 14 x 5
    ##    player    command_score zone_diff_per_pi~ command_zone_ra~ command_zone_rati~
    ##    <chr>             <dbl>             <dbl>            <dbl>              <dbl>
    ##  1 Radek Bi~         0.406              2.58            1.04                  78
    ##  2 Liam Sim~         0.409              2.43            0.994                 75
    ##  3 Roman Ki~         0.466              2.30            1.07                  80
    ##  4 Jackson ~         0.486              2.38            1.16                  87
    ##  5 Jack Bra~         0.523              2.37            1.24                  93
    ##  6 Alex Rao          0.553              2.35            1.30                  98
    ##  7 Jack Fin~         0.578              2.22            1.28                  96
    ##  8 Austin T~         0.586              2.16            1.26                  95
    ##  9 John Mic~         0.610              2.48            1.51                 113
    ## 10 Ryan McL~         0.632              2.30            1.46                 109
    ## 11 Aidan Ty~         0.642              2.39            1.54                 115
    ## 12 Will Mer~         0.683              2.21            1.51                 114
    ## 13 Matt Laz~         0.697              2.47            1.72                 129
    ## 14 Sammy Co~         0.75               2.08            1.56                 117

``` r
final_czr_table %>%
  arrange(zone_diff_per_pitch)
```

    ## # A tibble: 14 x 5
    ##    player    command_score zone_diff_per_pi~ command_zone_ra~ command_zone_rati~
    ##    <chr>             <dbl>             <dbl>            <dbl>              <dbl>
    ##  1 Sammy Co~         0.75               2.08            1.56                 117
    ##  2 Austin T~         0.586              2.16            1.26                  95
    ##  3 Will Mer~         0.683              2.21            1.51                 114
    ##  4 Jack Fin~         0.578              2.22            1.28                  96
    ##  5 Roman Ki~         0.466              2.30            1.07                  80
    ##  6 Ryan McL~         0.632              2.30            1.46                 109
    ##  7 Alex Rao          0.553              2.35            1.30                  98
    ##  8 Jack Bra~         0.523              2.37            1.24                  93
    ##  9 Jackson ~         0.486              2.38            1.16                  87
    ## 10 Aidan Ty~         0.642              2.39            1.54                 115
    ## 11 Liam Sim~         0.409              2.43            0.994                 75
    ## 12 Matt Laz~         0.697              2.47            1.72                 129
    ## 13 John Mic~         0.610              2.48            1.51                 113
    ## 14 Radek Bi~         0.406              2.58            1.04                  78

# Command Zone Rating: Visualizations for Analysis

## Preliminary Visualizations

As with any new metric, we are interested in determining if any
relationship exists between the results of CZR and established pitching
performance metrics (in this case, ERA and WHIP).

We will begin by plotting ERA and WHIP over time to gain an
understanding of the metrics’ development alongside team failures and
successes:

``` r
# Aggregate ERA by game date
agg_era <-
  aggregate(pregamepitching$pre_era_pitch, list("Game Date" = pregamepitching$date), mean, na.rm = TRUE)

agg_era
```

    ##    Game Date         x
    ## 1  3/25/2022  5.700294
    ## 2  3/29/2022  7.062500
    ## 3  4/10/2022  4.620312
    ## 4  4/12/2022  9.931613
    ## 5  4/19/2022  9.575588
    ## 6  4/20/2022 12.179722
    ## 7  4/22/2022  8.638438
    ## 8  4/23/2022  8.634375
    ## 9  4/24/2022  8.573226
    ## 10 4/25/2022  8.640000
    ## 11  4/5/2022  8.083429
    ## 12  4/8/2022  4.589062
    ## 13  4/9/2022  4.579063

``` r
# Create time series plot
gg1 <- ggplot(data = agg_era, aes(x = agg_era$`Game Date`, y = agg_era$x, group = 1)) +
  geom_line(linetype = "dashed", color = "navy") +
  geom_point(color = "gold") +
  xlab("Game Date") +
  ylab("Team ERA") +
  ggtitle("Team Average ERA Over the Season") +
  theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank())

gg1
```

    ## Warning: Use of `agg_era$`Game Date`` is discouraged. Use `Game Date` instead.

    ## Warning: Use of `agg_era$x` is discouraged. Use `x` instead.

    ## Warning: Use of `agg_era$`Game Date`` is discouraged. Use `Game Date` instead.

    ## Warning: Use of `agg_era$x` is discouraged. Use `x` instead.

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

Noteworthy performances may be added accordingly below:

``` r
# Aggregate ERA by game date
agg_era <-
  aggregate(pregamepitching$pre_era_pitch, list("Game Date" = pregamepitching$date), mean, na.rm = TRUE)

agg_era <- agg_era %>%
  clean_names() %>%
  mutate(game_date = mdy(game_date))

# Create time series plot
gg2 <- ggplot(data = agg_era, aes(x = agg_era$game_date, y = agg_era$x, group = 1)) +
  geom_line(linetype = "dashed", color = "navy") +
  geom_point(color = "gold") +
  xlab("Game Date") +
  ylab("Team ERA") +
  ggtitle("Team Average ERA Over the Season") +
  theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) +
  geom_vline(xintercept = mdy("4/19/2022"), color = "red") +
  geom_text(aes(x = mdy("4/12/2022"), y = 11.0 , label = "Duke Weekend Ends: 25 Runs Allowed"), color = "red")  

gg2
```

    ## Warning: Use of `agg_era$game_date` is discouraged. Use `game_date` instead.

    ## Warning: Use of `agg_era$x` is discouraged. Use `x` instead.

    ## Warning: Use of `agg_era$game_date` is discouraged. Use `game_date` instead.

    ## Warning: Use of `agg_era$x` is discouraged. Use `x` instead.

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

For greater granularity, we will investigate the average ERA by pitcher
over time:

``` r
# Aggregate ERA by game date and pitcher
agg_plyr_era <- aggregate(pregamepitching$pre_era_pitch ~ pregamepitching$date + pregamepitching$player, data = pregamepitching, FUN = mean, na.rm = TRUE)

agg_plyr_era <- agg_plyr_era %>%
  clean_names() %>%
  rename(game_date = "pregamepitching_date",
         player = "pregamepitching_player",
         pre_era_pitch = "pregamepitching_pre_era_pitch") %>%
  mutate(game_date = mdy(game_date))

# Create data sets for starting pitchers

tyrell_era <- agg_plyr_era %>% 
  filter(player == "Aidan Tyrell")
tyrell_era
```

    ##     game_date       player pre_era_pitch
    ## 1  2022-03-25 Aidan Tyrell          3.86
    ## 2  2022-03-29 Aidan Tyrell          4.15
    ## 3  2022-04-10 Aidan Tyrell          3.29
    ## 4  2022-04-19 Aidan Tyrell          3.23
    ## 5  2022-04-20 Aidan Tyrell          3.03
    ## 6  2022-04-22 Aidan Tyrell          3.03
    ## 7  2022-04-23 Aidan Tyrell          3.03
    ## 8  2022-04-24 Aidan Tyrell          3.15
    ## 9  2022-04-05 Aidan Tyrell          3.29
    ## 10 2022-04-08 Aidan Tyrell          3.29
    ## 11 2022-04-09 Aidan Tyrell          3.29

``` r
temple_era <- agg_plyr_era %>% 
  filter(player == "Austin Temple")
temple_era
```

    ##     game_date        player pre_era_pitch
    ## 1  2022-03-25 Austin Temple          2.42
    ## 2  2022-03-29 Austin Temple          2.42
    ## 3  2022-04-10 Austin Temple          2.76
    ## 4  2022-04-19 Austin Temple          3.03
    ## 5  2022-04-20 Austin Temple          3.03
    ## 6  2022-04-22 Austin Temple          3.03
    ## 7  2022-04-23 Austin Temple          3.03
    ## 8  2022-04-24 Austin Temple          3.00
    ## 9  2022-04-05 Austin Temple          3.42
    ## 10 2022-04-08 Austin Temple          3.29
    ## 11 2022-04-09 Austin Temple          3.29

``` r
jm_era <- agg_plyr_era %>% 
  filter(player == "John Michael Bertrand")
jm_era
```

    ##    game_date                player pre_era_pitch
    ## 1 2022-03-25 John Michael Bertrand          1.72
    ## 2 2022-03-29 John Michael Bertrand          2.19
    ## 3 2022-04-20 John Michael Bertrand          1.68
    ## 4 2022-04-05 John Michael Bertrand          1.81
    ## 5 2022-04-08 John Michael Bertrand          1.81
    ## 6 2022-04-09 John Michael Bertrand          1.53

``` r
findlay_era <- agg_plyr_era %>% 
  filter(player == "Jack Findlay")
findlay_era
```

    ##     game_date       player pre_era_pitch
    ## 1  2022-03-25 Jack Findlay          0.00
    ## 2  2022-03-29 Jack Findlay          0.00
    ## 3  2022-04-10 Jack Findlay          0.73
    ## 4  2022-04-12 Jack Findlay          0.73
    ## 5  2022-04-19 Jack Findlay          0.64
    ## 6  2022-04-20 Jack Findlay          0.64
    ## 7  2022-04-22 Jack Findlay          0.53
    ## 8  2022-04-23 Jack Findlay          0.53
    ## 9  2022-04-24 Jack Findlay          0.44
    ## 10 2022-04-05 Jack Findlay          0.96
    ## 11 2022-04-08 Jack Findlay          0.73
    ## 12 2022-04-09 Jack Findlay          0.73

``` r
# Plot average ERA for starters over the season
gg2 <- ggplot() +
  geom_line(data = tyrell_era, aes(x = game_date, y = pre_era_pitch, group = 1, colour = "Tyrell"), size = 1.5) +
  geom_line(data = temple_era, aes(x = game_date, y = pre_era_pitch, group = 1, colour = "Temple"), size = 1.5) +
  geom_line(data = jm_era, aes(x = game_date, y = pre_era_pitch, group = 1, colour = "Bertrand"), size = 1.5) +
  geom_line(data = findlay_era, aes(x = game_date, y = pre_era_pitch, group = 1, colour = "Findlay"), size = 1.5) +
  ylim(0, 6) +
  xlab("Game Date") +
  ylab("ERA") +
  ggtitle("Average ERA for Starting Pitchers") +
  scale_color_manual(name = "Players", values = c("Tyrell" = "gray", "Temple" = "navy", "Bertrand"="gold", "Findlay" = "green")) +
   theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank())

gg2
```

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

The same procedure for WHIP is conducted here (investigating team WHIP
and individual starter WHIP over time):

``` r
# Convert infinite values to NA
metrics_reduced_model_data$whip[!is.finite(metrics_reduced_model_data$whip)] <- NA

# Remove NA values for WHIP
whip_data <- metrics_reduced_model_data %>% 
  filter(as.character(metrics_reduced_model_data$whip) != "NA" 
)

# Aggregate WHIP by game date
agg_whip <-
  aggregate(whip_data$whip, list("Game Date" = whip_data$date), mean)

agg_whip <- agg_whip %>%
  clean_names() 


# Create time series plot
gg3 <- ggplot(data = agg_whip, aes(x = agg_whip$game_date, y = agg_whip$x, group = 1)) +
  geom_line(linetype = "dashed", color = "navy") +
  geom_point(color = "gold") +
  ylim(0, NA) +
  xlab("Game Date") +
  ylab("Team WHIP") +
  ggtitle("Team Average WHIP Over the Season") +
  geom_vline(xintercept = mdy("4/19/2022"), color = "red") +
  geom_text(aes(x = mdy("4/12/2022"), y = 6.0 , label = "Duke Weekend Ends: 25 Runs Allowed"), color = "red") +
  theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank())
  
gg3
```

    ## Warning: Use of `agg_whip$game_date` is discouraged. Use `game_date` instead.

    ## Warning: Use of `agg_whip$x` is discouraged. Use `x` instead.

    ## Warning: Use of `agg_whip$game_date` is discouraged. Use `game_date` instead.

    ## Warning: Use of `agg_whip$x` is discouraged. Use `x` instead.

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

The following plot attains more granularity by viewing the average WHIP
over time by starting pitcher:

``` r
# Aggregate WHIP by game date and pitcher
agg_plyr_whip <- aggregate(whip_data$whip ~ whip_data$date + whip_data$player, data = whip_data, FUN = mean, na.rm = TRUE)

agg_plyr_whip
```

    ##    whip_data$date      whip_data$player whip_data$whip
    ## 1      2022-04-12         Ahmad Harajli       1.666667
    ## 2      2022-03-25          Aidan Tyrell       6.750000
    ## 3      2022-04-19          Aidan Tyrell       5.285714
    ## 4      2022-04-23          Aidan Tyrell       5.428571
    ## 5      2022-04-24          Aidan Tyrell       2.733333
    ## 6      2022-03-25              Alex Rao       2.142857
    ## 7      2022-04-10              Alex Rao       2.100000
    ## 8      2022-04-22              Alex Rao       1.800000
    ## 9      2022-04-24              Alex Rao       1.875000
    ## 10     2022-04-09         Austin Gordon       2.750000
    ## 11     2022-04-09         Austin Temple       2.166667
    ## 12     2022-04-23         Austin Temple       2.571429
    ## 13     2022-04-12      Avery Goldensoph       4.333333
    ## 14     2022-04-23        Brennen Oxford       1.750000
    ## 15     2022-04-20         Bryce Martens       2.400000
    ## 16     2022-04-24        Camden Minacci       2.800000
    ## 17     2022-04-09         Casey Tallent       2.400000
    ## 18     2022-03-29      Connor Langreder       3.000000
    ## 19     2022-04-19       Connor Lockwood       7.333333
    ## 20     2022-04-24         Crawford Wade       3.250000
    ## 21     2022-04-23            Derek Crum       2.555556
    ## 22     2022-03-29           Drew Hasson       1.600000
    ## 23     2022-03-25       Drue Hackenberg       9.200000
    ## 24     2022-04-20           Enas Hayden       2.000000
    ## 25     2022-04-23            Eric Adler       1.500000
    ## 26     2022-04-10      Geoffrey Gilbert       2.833333
    ## 27     2022-03-25        Graham Firoved       2.666667
    ## 28     2022-04-19       Grant Jablonski       9.500000
    ## 29     2022-03-25         Griffin Green       5.125000
    ## 30     2022-03-25         Henry Weycker       4.000000
    ## 31     2022-03-25        Jack Brannigan       1.750000
    ## 32     2022-04-19        Jack Brannigan       2.000000
    ## 33     2022-04-24        Jack Brannigan       2.166667
    ## 34     2022-04-12          Jack Findlay       2.666667
    ## 35     2022-04-20          Jack Findlay       2.600000
    ## 36     2022-04-23          Jack Findlay       2.600000
    ## 37     2022-04-09       Jackson Dennies       4.000000
    ## 38     2022-04-12       Jackson Dennies       2.500000
    ## 39     2022-04-22       Jackson Dennies       3.200000
    ## 40     2022-04-24       Jackson Dennies       3.200000
    ## 41     2022-04-10       Jackson Lindley       3.250000
    ## 42     2022-04-22      Jacob Grzebinski       2.000000
    ## 43     2022-04-19      Jacob Rosenkranz      13.000000
    ## 44     2022-04-12           Jake Keaser       1.875000
    ## 45     2022-04-08              Jay Dill       2.857143
    ## 46     2022-04-10              Jay Dill       2.750000
    ## 47     2022-04-12          Jaylen Jones       1.250000
    ## 48     2022-03-25 John Michael Bertrand       3.555556
    ## 49     2022-04-08 John Michael Bertrand       3.750000
    ## 50     2022-04-23           Josh Hartle       4.357143
    ## 51     2022-04-20           Kyle Maurer       1.666667
    ## 52     2022-03-29            Liam Simon       1.200000
    ## 53     2022-04-12            Liam Simon       1.250000
    ## 54     2022-04-20            Liam Simon       1.700000
    ## 55     2022-04-08           Mack Anglin       1.730769
    ## 56     2022-04-20          Matt Lazzaro       2.000000
    ## 57     2022-04-20        Michael Madura       3.142857
    ## 58     2022-03-29             Nick Bonk       2.454545
    ## 59     2022-04-10          Nick Clayton       4.625000
    ## 60     2022-04-09         Nick Hoffmann       7.000000
    ## 61     2022-04-09         P.J. Labriola       3.500000
    ## 62     2022-03-25        Radek Birkholz       1.600000
    ## 63     2022-03-29        Radek Birkholz       1.600000
    ## 64     2022-04-09        Radek Birkholz       1.875000
    ## 65     2022-04-12        Radek Birkholz       1.875000
    ## 66     2022-04-20        Radek Birkholz       2.000000
    ## 67     2022-04-22          Reed Mascolo       3.666667
    ## 68     2022-04-20            Rex Stills       1.947368
    ## 69     2022-04-22          Rhett Lowder       3.705882
    ## 70     2022-03-29            RJ Nowicki       2.142857
    ## 71     2022-03-29         Roman Kimball       1.000000
    ## 72     2022-04-10         Roman Kimball       1.400000
    ## 73     2022-04-19         Roman Kimball       2.142857
    ## 74     2022-04-20         Roman Kimball       2.285714
    ## 75     2022-04-22         Roman Kimball       2.285714
    ## 76     2022-04-23         Roman Kimball       2.285714
    ## 77     2022-04-24         Roman Kimball       2.285714
    ## 78     2022-04-10           Ryan Ammons       2.571429
    ## 79     2022-03-25        Ryan McLinskey       3.250000
    ## 80     2022-04-08        Ryan McLinskey       3.166667
    ## 81     2022-04-09        Ryan McLinskey       3.166667
    ## 82     2022-04-20          Ryan Robison       3.000000
    ## 83     2022-04-12           Ryan Zimmer       1.833333
    ## 84     2022-04-23          Sammy Cooper       2.333333
    ## 85     2022-04-23           Seth Keener       4.400000
    ## 86     2022-04-24          Teddy McGraw       2.476190
    ## 87     2022-04-09           Ty Olenchuk       3.800000
    ## 88     2022-04-12      Walker Cleveland       2.375000
    ## 89     2022-04-10           Will Mercer       2.333333
    ## 90     2022-04-24           Will Mercer       2.800000
    ## 91     2022-04-12          Willie Weiss       2.000000
    ## 92     2022-04-22            Zach Grace       4.000000

``` r
# Create data sets for each starter
tyrell_whip <- agg_plyr_whip %>% 
  filter(agg_plyr_whip$`whip_data$player` == "Aidan Tyrell")
tyrell_whip
```

    ##   whip_data$date whip_data$player whip_data$whip
    ## 1     2022-03-25     Aidan Tyrell       6.750000
    ## 2     2022-04-19     Aidan Tyrell       5.285714
    ## 3     2022-04-23     Aidan Tyrell       5.428571
    ## 4     2022-04-24     Aidan Tyrell       2.733333

``` r
temple_whip <- agg_plyr_whip %>% 
  filter(agg_plyr_whip$`whip_data$player` == "Austin Temple")
temple_whip
```

    ##   whip_data$date whip_data$player whip_data$whip
    ## 1     2022-04-09    Austin Temple       2.166667
    ## 2     2022-04-23    Austin Temple       2.571429

``` r
jm_whip <- agg_plyr_whip %>% 
  filter(agg_plyr_whip$`whip_data$player` == "John Michael Bertrand")
jm_whip
```

    ##   whip_data$date      whip_data$player whip_data$whip
    ## 1     2022-03-25 John Michael Bertrand       3.555556
    ## 2     2022-04-08 John Michael Bertrand       3.750000

``` r
findlay_whip <- agg_plyr_whip %>% 
  filter(agg_plyr_whip$`whip_data$player` == "Jack Findlay")
findlay_whip
```

    ##   whip_data$date whip_data$player whip_data$whip
    ## 1     2022-04-12     Jack Findlay       2.666667
    ## 2     2022-04-20     Jack Findlay       2.600000
    ## 3     2022-04-23     Jack Findlay       2.600000

``` r
# Create scatter plot for starters' WHIP

gg4 <- ggplot() +
  geom_point(data = tyrell_whip, aes(x = `whip_data$date`, y = `whip_data$whip`, group = 1, colour = "Tyrell")) +
  geom_point(data = temple_whip, aes(x = `whip_data$date`, y = `whip_data$whip`, group = 1, colour = "Temple")) +
  geom_point(data = jm_whip, aes(x = `whip_data$date`, y = `whip_data$whip`, group = 1, colour = "Bertrand")) +
  geom_point(data = findlay_whip, aes(x = `whip_data$date`, y = `whip_data$whip`, group = 1, colour = "Findlay")) +
  ylim(0, 8) +
  xlab("Game Date") +
  ylab("WHIP") +
  ggtitle("Average WHIP for Starting Pitchers") +
  scale_color_manual(name = "Players", values = c("Tyrell" = "gray", "Temple" = "navy", "Bertrand"="gold", "Findlay" = "green")) +
  theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank())

gg4
```

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

``` r
# Aggregate WHIP by players 

plyr_whip <- aggregate(whip_data$whip ~  whip_data$player, data = whip_data, FUN = mean, na.rm = TRUE)

plyr_whip
```

    ##         whip_data$player whip_data$whip
    ## 1          Ahmad Harajli       1.666667
    ## 2           Aidan Tyrell       4.609088
    ## 3               Alex Rao       1.956743
    ## 4          Austin Gordon       2.750000
    ## 5          Austin Temple       2.336406
    ## 6       Avery Goldensoph       4.333333
    ## 7         Brennen Oxford       1.750000
    ## 8          Bryce Martens       2.400000
    ## 9         Camden Minacci       2.800000
    ## 10         Casey Tallent       2.400000
    ## 11      Connor Langreder       3.000000
    ## 12       Connor Lockwood       7.333333
    ## 13         Crawford Wade       3.250000
    ## 14            Derek Crum       2.555556
    ## 15           Drew Hasson       1.600000
    ## 16       Drue Hackenberg       9.200000
    ## 17           Enas Hayden       2.000000
    ## 18            Eric Adler       1.500000
    ## 19      Geoffrey Gilbert       2.833333
    ## 20        Graham Firoved       2.666667
    ## 21       Grant Jablonski       9.500000
    ## 22         Griffin Green       5.125000
    ## 23         Henry Weycker       4.000000
    ## 24        Jack Brannigan       2.059109
    ## 25          Jack Findlay       2.620635
    ## 26       Jackson Dennies       3.222222
    ## 27       Jackson Lindley       3.250000
    ## 28      Jacob Grzebinski       2.000000
    ## 29      Jacob Rosenkranz      13.000000
    ## 30           Jake Keaser       1.875000
    ## 31              Jay Dill       2.790179
    ## 32          Jaylen Jones       1.250000
    ## 33 John Michael Bertrand       3.667236
    ## 34           Josh Hartle       4.357143
    ## 35           Kyle Maurer       1.666667
    ## 36            Liam Simon       1.396528
    ## 37           Mack Anglin       1.730769
    ## 38          Matt Lazzaro       2.000000
    ## 39        Michael Madura       3.142857
    ## 40             Nick Bonk       2.454545
    ## 41          Nick Clayton       4.625000
    ## 42         Nick Hoffmann       7.000000
    ## 43         P.J. Labriola       3.500000
    ## 44        Radek Birkholz       1.807432
    ## 45          Reed Mascolo       3.666667
    ## 46            Rex Stills       1.947368
    ## 47          Rhett Lowder       3.705882
    ## 48            RJ Nowicki       2.142857
    ## 49         Roman Kimball       1.666606
    ## 50           Ryan Ammons       2.571429
    ## 51        Ryan McLinskey       3.207071
    ## 52          Ryan Robison       3.000000
    ## 53           Ryan Zimmer       1.833333
    ## 54          Sammy Cooper       2.333333
    ## 55           Seth Keener       4.400000
    ## 56          Teddy McGraw       2.476190
    ## 57           Ty Olenchuk       3.800000
    ## 58      Walker Cleveland       2.375000
    ## 59           Will Mercer       2.566667
    ## 60          Willie Weiss       2.000000
    ## 61            Zach Grace       4.000000

``` r
# Filter by starters
starters_whip <- plyr_whip %>% 
  filter(plyr_whip$`whip_data$player` == "Aidan Tyrell" | plyr_whip$`whip_data$player` == "Austin Temple" | plyr_whip$`whip_data$player` == "John Michael Bertrand" | plyr_whip$`whip_data$player` == "Jack Findlay")

# Create bar chart for starters' average WHIP
gg5 <- ggplot(data = starters_whip, aes(x = starters_whip$`whip_data$player`, y = starters_whip$`whip_data$whip`, fill = starters_whip$`whip_data$player`)) +
  geom_col() +
  theme(legend.position="none") +
  xlab("Player") +
  ylab("Average WHIP") +
  ggtitle("Average WHIP for Starting Pitchers") +
  theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) +
   scale_fill_manual(values = c("gray", "navy", "gold", "green"))

gg5
```

    ## Warning: Use of `starters_whip$`whip_data$player`` is discouraged. Use
    ## `whip_data$player` instead.

    ## Warning: Use of `starters_whip$`whip_data$whip`` is discouraged. Use
    ## `whip_data$whip` instead.

    ## Warning: Use of `starters_whip$`whip_data$player`` is discouraged. Use
    ## `whip_data$player` instead.

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/unnamed-chunk-42-2.png)<!-- -->

## Command Zone Rating (CZR) Visualizations

With a baseline for ERA and WHIP established, we can investigate the
relationships between CZR and rigid pitching performance metrics (if
any)

We begin with the relationship between ERA and CZR. Ideally, a positive
relationship appears in the data (low CZR values associated with high
pitching command success and, as a result, lower ERA scores over time):

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
czr_plot_data <- last_pitching_appearance_pregame %>%
  filter(player %in% player_list) %>%
  select(player, pre_era_pitch, pre_b_avg_pitch, whip) %>%
  distinct() %>%
  left_join(final_czr_table,
            by = "player") %>%
  select(-c(command_score, zone_diff_per_pitch, command_zone_rating)) 

gg7 <- ggplot(data = czr_plot_data, aes(x = command_zone_rating_normalized, y = pre_era_pitch)) +
  geom_point(color = "navy", size = 1.5) +
  geom_smooth(se = FALSE, color = "gold", size = 1.5) +
  xlab("Command Zone Rating (CZR)") +
  ylab("ERA") +
  ggtitle("CZR vs. Earned Run Average (ERA)") +
   theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank())

gg7
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

Next, we will investigate the relationship between WHIP and CZR.
Ideally, a positive relationship appears in the data (low CZR values
associated with high pitching command success and, as a result, lower
WHIP scores over time):

``` r
gg8 <- ggplot(data = czr_plot_data, aes(x = command_zone_rating_normalized, y = whip)) +
  geom_point(color = "navy", size = 1.5) +
  geom_smooth(se = FALSE, color = "gold", size = 1.5) +
  xlab("Command Zone Rating (CZR)") +
  ylab("WHIP") +
  ggtitle("CZR vs. WHIP") +
   theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) +
  geom_text(x = 86, y = 3.2, label = "Jackson Dennies", vjust = 1) +
  geom_text(x = 96, y = 1.875, label = "Alex Rao", vjust = 1)

gg8
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

For additional reference, the relationship between opponent batting
average and CZR is shown below:

``` r
gg9 <- ggplot(data = czr_plot_data, aes(x = command_zone_rating_normalized, y = pre_b_avg_pitch)) +
  geom_point(color = "navy", size = 1.5) +
  geom_smooth(se = FALSE, color = "gold", size = 1.5) +
  xlab("Command Zone Rating (CZR)") +
  ylab("Opponent Batting Average") +
  ggtitle("CZR vs. Opponent Batting Average") +
   theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) +
  geom_text(x = 116, y = 0.364, label = "Sammy Cooper", vjust = 1)

gg9
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](HPA-Command-Zone-Rating-Final_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

## CZR Scores Analysis Workspace

As mentioned, the final CZR table, as well as its associated predictions
table, allows for various calculations and analysis by zone. The
following space serves as for additional visualizations, aggregations,
and grouping based on the structure of the prediction results:

``` r
final_bounded_results %>%
  filter(player == "John Michael Bertrand",
         batter_side == "Right",
         tagged_pitch_type == "Fastball")
```

    ## # A tibble: 21 x 9
    ##     zone player               batter_side tagged_pitch_ty~ preds total_zone_diff
    ##    <int> <chr>                <fct>       <fct>            <dbl>           <int>
    ##  1     2 John Michael Bertra~ Right       Fastball          0.77             692
    ##  2     3 John Michael Bertra~ Right       Fastball          0.8              692
    ##  3     5 John Michael Bertra~ Right       Fastball          0.8              692
    ##  4     6 John Michael Bertra~ Right       Fastball          0.8              692
    ##  5    11 John Michael Bertra~ Right       Fastball          0.78             692
    ##  6    15 John Michael Bertra~ Right       Fastball          0.73             692
    ##  7    16 John Michael Bertra~ Right       Fastball          0.8              692
    ##  8    20 John Michael Bertra~ Right       Fastball          0.79             692
    ##  9    22 John Michael Bertra~ Right       Fastball          0.8              692
    ## 10    23 John Michael Bertra~ Right       Fastball          0.8              692
    ## # ... with 11 more rows, and 3 more variables: zone_diff_per_pitch <dbl>,
    ## #   total_misses_by_zone_side_type <int>, total_pitches_by_zone_side_type <int>

``` r
bounded_results_matrix %>%
  select(player, pre_era_pitch, whip) %>%
  arrange(whip) %>%
  distinct()
```

    ## # A tibble: 16 x 3
    ##    player                pre_era_pitch  whip
    ##    <chr>                         <dbl> <dbl>
    ##  1 Liam Simon                     7.2   1.7 
    ##  2 Alex Rao                       3.13  1.88
    ##  3 Matt Lazzaro                   6     2   
    ##  4 Radek Birkholz                 4.82  2   
    ##  5 Jack Brannigan                 8.68  2.17
    ##  6 Roman Kimball                  5.06  2.29
    ##  7 Sammy Cooper                  18     2.33
    ##  8 John Michael Bertrand          4.59  2.43
    ##  9 Liam Simmon                    4.59  2.43
    ## 10 Austin Temple                  3.03  2.57
    ## 11 Jack Findlay                   0.53  2.6 
    ## 12 Aidan Tyrell                   3.15  2.73
    ## 13 Will Mercer                    4.5   2.8 
    ## 14 Ryan McLinskey                 2.7   3.17
    ## 15 Jackson Dennies                1.35  3.2 
    ## 16 Sammy Cooper                  NA    NA

Command Zone Rating (CZR)
