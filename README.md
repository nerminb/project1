Project 1
================
Nermin Bibic
6/20/2021

-   [Reading and Summarizing data from the National Hockey League’s
    (NHL)
    API](#reading-and-summarizing-data-from-the-national-hockey-leagues-nhl-api)
    -   [Packages required to run code](#packages-required-to-run-code)
    -   [Functions to contact the NHL records
        API](#functions-to-contact-the-nhl-records-api)
        -   [Function returning id, firstSeasonId, lastSeasonId, and
            name of every team in the history of the
            NHL.](#function-returning-id-firstseasonid-lastseasonid-and-name-of-every-team-in-the-history-of-the-nhl)
        -   [Function returning total stats for every franchise (ex:
            roadTies, roadWins,
            etc.)](#function-returning-total-stats-for-every-franchise-ex-roadties-roadwins-etc)
        -   [Function returning season records for a specific
            franchise](#function-returning-season-records-for-a-specific-franchise)
        -   [Function returning season records for a specific
            franchise](#function-returning-season-records-for-a-specific-franchise-1)
        -   [Function returning skater records for a specific
            franchise](#function-returning-skater-records-for-a-specific-franchise)

# Reading and Summarizing data from the National Hockey League’s (NHL) API

### Packages required to run code

``` r
library(knitr)
library(tidyverse)
library(httr)
library(jsonlite)
library(ggplot2)
library(readr)
library(DT)
```

## Functions to contact the NHL records API

The below functions return NHL records in well-formatted, parsed data
frames, with options to specify the franchise of choice by both name and
ID number.

Our base URL is: <https://records.nhl.com/site/api>.

``` r
baseURL <- "https://records.nhl.com/site/api/"
```

### Function returning id, firstSeasonId, lastSeasonId, and name of every team in the history of the NHL.

``` r
franchise <- function() {
  URL <- paste0(baseURL, 'franchise')
  franchiseRAW <- RCurl::getURL(URL)
  franchiseDF <- fromJSON(franchiseRAW, flatten=TRUE)
  franchiseDF <- as_tibble(franchiseDF$data)
  franchiseDF <- franchiseDF %>% select(id, firstSeasonId, lastSeasonId, fullName)
  return(franchiseDF)
}
```

### Function returning total stats for every franchise (ex: roadTies, roadWins, etc.)

``` r
franchiseTeamTotals <- function() {
  URL <- paste0(baseURL, 'franchise-team-totals')
  franchiseTeamTotalsRAW <- RCurl::getURL(URL)
  franchiseTeamTotalsDF <- fromJSON(franchiseTeamTotalsRAW, flatten=TRUE)
  franchiseTeamTotalsDF <- as_tibble(franchiseTeamTotalsDF$data)
  return(franchiseTeamTotalsDF)
}
```

### Function returning season records for a specific franchise

``` r
seasonRecords <- function(franchise, method = c('id', 'name')) {
  if (method == 'name') {
    franchise = select(filter(franchise(), fullName == franchise), id)$id
  }
  URL <- paste0(baseURL,
                'franchise-season-records?cayenneExp=franchiseId=',
                franchise)
  seasonRecordsRAW <- RCurl::getURL(URL)
  seasonRecordsDF <- fromJSON(seasonRecordsRAW, flatten=TRUE)
  seasonRecordsDF <- as_tibble(seasonRecordsDF$data)
  return(seasonRecordsDF)
}
```

### Function returning season records for a specific franchise

``` r
goalieRecords <- function(franchise, method = c('id', 'name')) {
  if (method == 'name') {
    franchise = select(filter(franchise(), fullName == franchise), id)$id
  }
  URL <- paste0(baseURL,
                'franchise-goalie-records?cayenneExp=franchiseId=',
                franchise)
  goalieRecordsRAW <- RCurl::getURL(URL)
  goalieRecordsDF <- fromJSON(goalieRecordsRAW, flatten=TRUE)
  goalieRecordsDF <- as_tibble(goalieRecordsDF$data)
  return(goalieRecordsDF)
}
```

### Function returning skater records for a specific franchise

``` r
skaterRecords <- function(franchise, method = c('id', 'name')) {
  if (method == 'name') {
    franchise = select(filter(franchise(), fullName == franchise), id)$id
  }
  URL <- paste0(baseURL,
                'franchise-skater-records?cayenneExp=franchiseId=',
                franchise)
  skaterRecordsRAW <- RCurl::getURL(URL)
  skaterRecordsDF <- fromJSON(skaterRecordsRAW, flatten=TRUE)
  skaterRecordsDF <- as_tibble(skaterRecordsDF$data)
  return(skaterRecordsDF)
}
```

– /site/api/franchise-detail?cayenneExp=mostRecentTeamId=ID (Admin
history and retired num- bers)
