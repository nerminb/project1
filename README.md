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
        -   [Function returning admininstration history and retired
            numbers](#function-returning-admininstration-history-and-retired-numbers)
    -   [Function to contact the NHL stats API for the
        ?expand=team.stats
        modifier.](#function-to-contact-the-nhl-stats-api-for-the-expandteamstats-modifier)
    -   [Wrapper function](#wrapper-function)

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

Our base URLs is: <https://records.nhl.com/site/api>.

``` r
baseURLRecords <- "https://records.nhl.com/site/api/"
baseURLStats <- "https://statsapi.web.nhl.com/api/v1/teams"
```

### Function returning id, firstSeasonId, lastSeasonId, and name of every team in the history of the NHL.

``` r
franchises <- function() {
  URL <- paste0(baseURLRecords, 'franchise')
  franchiseRAW <- RCurl::getURL(URL)
  franchiseDF <- fromJSON(franchiseRAW, flatten=TRUE)
  franchiseDF <- as_tibble(franchiseDF$data)
  franchiseDF <- franchiseDF %>% select(id,
                                        mostRecentTeamId,
                                        fullName,
                                        firstSeasonId,
                                        lastSeasonId)
  return(franchiseDF)
}
```

### Function returning total stats for every franchise (ex: roadTies, roadWins, etc.)

``` r
franchiseTeamTotals <- function() {
  URL <- paste0(baseURLRecords, 'franchise-team-totals')
  franchiseTeamTotalsRAW <- RCurl::getURL(URL)
  franchiseTeamTotalsDF <- fromJSON(franchiseTeamTotalsRAW, flatten=TRUE)
  franchiseTeamTotalsDF <- as_tibble(franchiseTeamTotalsDF$data)
  return(franchiseTeamTotalsDF)
}
```

### Function returning season records for a specific franchise

``` r
seasonRecords <- function(team, method = c('franchise_id', 'team_id', 'team_name')) {
  if(missing(team)) {
    stop('Please specify team')
  }
  if(missing(method)) {
    stop('Please specify method. Acceptable methods:\n\nfranchise_id\nteam_id\nteam_name')
  }
  if (method == 'team_id') {
    team = select(filter(franchises(), mostRecentTeamId == team), id)$id
  } else if (method == 'team_name') {
    team = select(filter(franchises(), fullName == team), id)$id
  }
  URL <- paste0(baseURLRecords, 'franchise-season-records?cayenneExp=franchiseId=', team)
  seasonRecordsRAW <- RCurl::getURL(URL)
  seasonRecordsDF <- fromJSON(seasonRecordsRAW, flatten=TRUE)
  seasonRecordsDF <- as_tibble(seasonRecordsDF$data)
  return(seasonRecordsDF)
}
```

### Function returning season records for a specific franchise

``` r
goalieRecords <- function(team, method = c('franchise_id', 'team_id', 'team_name')) {
  if(missing(team)) {
    stop('Please specify team')
  }
  if(missing(method)) {
    stop('Please specify method. Acceptable methods:\n\nfranchise_id\nteam_id\nteam_name')
  }
  if (method == 'team_id') {
    team = select(filter(franchises(), mostRecentTeamId == team), id)$id
  } else if (method == 'team_name') {
    team = select(filter(franchises(), fullName == team), id)$id
  }
  URL <- paste0(baseURLRecords, 'franchise-goalie-records?cayenneExp=franchiseId=', team)
  goalieRecordsRAW <- RCurl::getURL(URL)
  goalieRecordsDF <- fromJSON(goalieRecordsRAW, flatten=TRUE)
  goalieRecordsDF <- as_tibble(goalieRecordsDF$data)
  return(goalieRecordsDF)
}
```

### Function returning skater records for a specific franchise

``` r
skaterRecords <- function(team, method = c('franchise_id', 'team_id', 'team_name')) {
  if(missing(team)) {
    stop('Please specify team')
  }
  if(missing(method)) {
    stop('Please specify method. Acceptable methods:\n\nfranchise_id\nteam_id\nteam_name')
  }
  if (method == 'team_id') {
    team = select(filter(franchises(), mostRecentTeamId == team), id)$id
  } else if (method == 'team_name') {
    team = select(filter(franchises(), fullName == team), id)$id
  }
  URL <- paste0(baseURLRecords, 'franchise-skater-records?cayenneExp=franchiseId=', team)
  skaterRecordsRAW <- RCurl::getURL(URL)
  skaterRecordsDF <- fromJSON(skaterRecordsRAW, flatten=TRUE)
  skaterRecordsDF <- as_tibble(skaterRecordsDF$data)
  return(skaterRecordsDF)
}
```

### Function returning admininstration history and retired numbers

``` r
adminHistory <- function(team, method = c('franchise_id', 'team_id', 'team_name')) {
  if(missing(team)) {
    stop('Please specify team')
  }
  if(missing(method)) {
    stop('Please specify method. Acceptable methods:\n\nfranchise_id\nteam_id\nteam_name')
  }
  if (method == 'franchise_id') {
    team = select(filter(franchises(), id == team), mostRecentTeamId)$mostRecentTeamId
  } else if (method == 'team_name') {
    team = select(filter(franchises(), fullName == team), mostRecentTeamId)$mostRecentTeamId
  }
  URL <- paste0(baseURLRecords, 'franchise-detail?cayenneExp=mostRecentTeamId=', team)
  adminHistoryRAW <- RCurl::getURL(URL)
  adminHistoryDF <- fromJSON(adminHistoryRAW, flatten=TRUE)
  adminHistoryDF <- as_tibble(adminHistoryDF$data)
  return(adminHistoryDF)
}
```

## Function to contact the NHL stats API for the ?expand=team.stats modifier.

This function returns team stats. If no team is specified (by running
the function as “teamStats()”), this function returns stats for all
teams. If a team is specified, either by francise ID, team ID, or team
name, this function returns a single row of stats data. The stats data
contain both raw values for each data point as well as ranks for the
data points.

``` r
teamStats <- function(team, method = c('franchise_id', 'team_id', 'team_name')) {
  
  if (missing(team)) {
    
    URL <- paste0(baseURLStats, '?expand=team.stats')
    teamStatsRAW <- RCurl::getURL(URL)
    teamStatsDF <- fromJSON(teamStatsRAW, flatten=TRUE)
    teamStatsList <- teamStatsDF$teams$teamStats
    # Remove NULL objects
    teamStatsList <- teamStatsList[-which(sapply(teamStatsList, is.null))]
    # Initialize empty list, to be filled with single-row tibbles
    allTeamsStats <- list()
    for (i in teamStatsList) {
      teamStatsSplits <- as_tibble(as.data.frame(i$splits))
      # Split raw value and rank data
      teamStatsValues <- teamStatsSplits[1, ]
      teamStatsRanks <- select(teamStatsSplits[2, ], -c('team.id', 'team.name', 'team.link'))
      # Clean up column names; remove prefix
      colnames(teamStatsValues) <- gsub("stat.", "", colnames(teamStatsValues))
      colnames(teamStatsRanks) <- gsub("Rank", "", colnames(teamStatsRanks))
      colnames(teamStatsRanks) <- gsub("stat.", "", colnames(teamStatsRanks))
      # Distinguish rank data
      colnames(teamStatsRanks) <- paste0(colnames(teamStatsRanks), 'Rank')
      # Drop NA columns
      teamStatsValues <- teamStatsValues[, colSums(is.na(teamStatsValues)) != nrow(teamStatsValues)]
      teamStatsRanks <- teamStatsRanks[, colSums(is.na(teamStatsRanks)) != nrow(teamStatsRanks)]
      # Combine values and ranks into one row
      teamStatsBind <- as_tibble(cbind(teamStatsValues, teamStatsRanks))
      allTeamsStats[[length(allTeamsStats) + 1]] <- teamStatsBind
    }
    # combine all tibbles in resulting list into one tibble
    allTeamsStats <- bind_rows(allTeamsStats)
    return(allTeamsStats)

  } else {
    
    if(missing(team)) {
      stop('Please specify team')
    }
    if(missing(method)) {
      stop('Please specify method. Acceptable methods:\n\nfranchise_id\nteam_id\nteam_name')
    }
    if (method == 'franchise_id') {
      team = select(filter(franchises(), id == team), mostRecentTeamId)$mostRecentTeamId
    } else if (method == 'team_name') {
      team = select(filter(franchises(), fullName == team), mostRecentTeamId)$mostRecentTeamId
    }
    URL <- paste0(baseURLStats, '/', team, '?expand=team.stats')
    teamStatsRAW <- RCurl::getURL(URL)
    teamStatsDF <- fromJSON(teamStatsRAW, flatten=TRUE)
    # Choose 'teamStats' object in JSON
    teamStatsDF <- as_tibble(teamStatsDF$teams$teamStats[[1]], .name_repair = 'minimal')
    teamStatsSplits <- as_tibble(as.data.frame(teamStatsDF$splits))
    # Split raw value and rank data
    teamStatsValues <- teamStatsSplits[1, ]
    teamStatsRanks <- select(teamStatsSplits[2, ], -c('team.id', 'team.name', 'team.link'))
    # Clean up column names; remove prefix
    colnames(teamStatsValues) <- gsub("stat.", "", colnames(teamStatsValues))
    colnames(teamStatsRanks) <- gsub("Rank", "", colnames(teamStatsRanks))
    colnames(teamStatsRanks) <- gsub("stat.", "", colnames(teamStatsRanks))
    # Distinguish rank data
    colnames(teamStatsRanks) <- paste0(colnames(teamStatsRanks), 'Rank')
    # Drop NA columns
    teamStatsValues <- teamStatsValues[, colSums(is.na(teamStatsValues)) != nrow(teamStatsValues)]
    teamStatsRanks <- teamStatsRanks[, colSums(is.na(teamStatsRanks)) != nrow(teamStatsRanks)]
    # Combine values and ranks into one row
    teamStatsBind <- as_tibble(cbind(teamStatsValues, teamStatsRanks))
    return(teamStatsBind)
    
  }
  
}
```

## Wrapper function

This wrapper function is a one-stop-shop for the user to access any of
the API endpoints above. This function simply calls the appropriate
endpoint as per the users request, including any modifiers, team IDs,
etc.

``` r
NHLData <- function(target_data = c('franchises', 'team_totals', 'season_records',
                                    'goalie_records', 'skater_records',
                                    'admin_history', 'team_stats'),...) {
  switch(target_data,
           franchises = franchises(),
           team_totals = franchiseTeamTotals(),
           season_records = seasonRecords(...),
           goalie_records = goalieRecords(...),
           skater_records = skaterRecords(...),
           admin_history = adminHistory(...),
           team_stats = teamStats(...),
           stop("Invalid argument. Acceptable target_data:\n\nfranchises\nteam_totals\nseason_records\ngoalie_records\nskater_records\nadmin_history\nteam_stats")
  )
}
```
