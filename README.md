Project 1
================
Nermin Bibic
6/20/2021

-   [Reading and Summarizing data from the National Hockey League’s
    (NHL)
    API](#reading-and-summarizing-data-from-the-national-hockey-leagues-nhl-api)
    -   [Packages required to run code](#packages-required-to-run-code)
    -   [Functions](#functions)

# Reading and Summarizing data from the National Hockey League’s (NHL) API

### Packages required to run code

``` r
library(knitr)
library(tidyverse)
library(ggplot2)
library(readr)
library(DT)
```

### Functions

• You should write functions to contact the NHL records API for the
endpoints listed below. The functions should return well-formatted,
parsed data (usually a data frame). Where possible, the user should have
the option to specify the franchise of choice by both name and ID number
- you’ll need to map the names to ID numbers yourself. – /franchise
(Returns id, firstSeasonId and lastSeasonId and name of every team in
the history of the NHL) – /franchise-team-totals (Returns Total stats
for every franchise (ex roadTies, roadWins, etc)) –
/site/api/franchise-season-records?cayenneExp=franchiseId=ID (Drill-down
into season records for a specific franchise) –
/franchise-goalie-records?cayenneExp=franchiseId=ID (Goalie records for
the specified franchise) –
/franchise-skater-records?cayenneExp=franchiseId=ID (Skater records,
same interaction as goalie endpoint) –
/site/api/franchise-detail?cayenneExp=mostRecentTeamId=ID (Admin history
and retired num- bers)
