# R script that munges shapefiles and some census data to produce a county-level dataset for the counties in the totality
# path of the Aug 21 eclipse

# Census shapefiles downloaded from: https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html
# Eclipse shapefiles downloaded from NASA at: https://svs.gsfc.nasa.gov/4518
# 2016 presidential election data downloaded from Data for Democracy Election Transparency dataset on data.world:
#   https://data.world/data4democracy/election-transparency

library(rgeos)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(tidycensus)
library(rvest)
library(stringr)
library(lubridate)

countiesShp <- readOGR('/opt/data/Shapefiles/cb_2015_us_county_500k/', 'cb_2015_us_county_500k')
countiesDf <- countiesShp@data
countiesDf$ID <- rownames(countiesDf)

statesShp <- readOGR('/opt/data/Shapefiles/cb_2013_us_state_500k/', 'cb_2013_us_state_500k') %>%
  subset(!(GEOID %in% c('02', '15', '60', '66', '69', '72', '78')))
statesDf <- statesShp@data %>%
  select(StateFIPS=STATEFP, StateAbbr=STUSPS, StateName=NAME) %>%
  mutate_all(as.character) %>%
  as_tibble()

eclipseUmbraShp <- readOGR('/opt/data/Shapefiles/eclipse2017/', 'upath17')
eclipseUmbraShp <- spTransform(eclipseUmbraShp, proj4string(countiesShp))
eclipseUmbraShp <- gIntersection(eclipseUmbraShp, statesShp)

eclipseCountiesShp <- gIntersection(eclipseUmbraShp, countiesShp, byid=c(FALSE, TRUE))

# note: convert area from sq meters to sq miles
eclipseCountiesDf <- map_df(eclipseCountiesShp@polygons, function(polygon) {
  tibble(ID=polygon@ID)
}) %>% inner_join(countiesDf, by='ID') %>%
  select(StateFIPS=STATEFP, CountyFIPS=GEOID, CountyName=NAME, ShapefileID=ID, ALAND, AWATER) %>%
  mutate(Area=(as.numeric(as.character(ALAND)) + as.numeric(as.character(AWATER)))*3.86102e-7) %>%
  select(-ALAND, -AWATER) %>%
  mutate_at(vars(-Area), as.character) %>%
  inner_join(statesDf, by='StateFIPS')

eclipseCountiesShp <- subset(countiesShp, GEOID %in% eclipseCountiesDf$CountyFIPS)

eclipseCountiesSdf <- fortify(eclipseCountiesShp)
statesSdf <- fortify(statesShp)
eclipseUmbraSdf <- fortify(eclipseUmbraShp)

census_api_key(Sys.getenv('CENSUS_API_KEY'))
#acsVariables <- load_variables(2015, 'acs5', cache=TRUE)

acsData <- get_acs(geography='county', variables=c('B01001_001E'), output='wide') %>%
  select(CountyFIPS=GEOID,
         TotalPopulation=B01001_001E)

electionData2016 <- read_csv('https://query.data.world/s/KZ3ZPaNQIiw85_R4ZSXw9--gNxAfoR') %>% select(-StateName, -CountyName, -StateAbbr)

eclipseCountiesFullDf <- inner_join(eclipseCountiesDf, acsData, by='CountyFIPS') %>%
  inner_join(electionData2016, by=c('CountyFIPS'='County'))

eclipseStates <- eclipseCountiesFullDf %>% select(StateName, StateFIPS) %>% distinct()

countySeats <- map2_df(eclipseStates$StateName, eclipseStates$StateFIPS, function(stateName, stateFIPS) {
  Sys.sleep(2)
  read_html(paste0('https://en.wikipedia.org/wiki/Counties_in_', str_replace(stateName, ' ', '_'))) %>% html_node('table.wikitable') %>%
    html_table(header=FALSE) %>% tail(-1) %>% select(CountyFIPS=X2, CountySeat=X3) %>%
    mutate(CountyFIPS=paste0(stateFIPS, CountyFIPS))
})

eclipseCountiesFullDf <- eclipseCountiesFullDf %>% inner_join(countySeats, by='CountyFIPS')

umbraWithTime <- readOGR('/opt/data/Shapefiles/eclipse2017/', 'umbra17_1s')
umbraWithTime <- spTransform(umbraWithTime, proj4string(countiesShp))
umbraWithTimeData <- umbraWithTime@data
umbraWithTimeData$id <- rownames(umbraWithTimeData)

umbraCountyContact <- map_df(eclipseCountiesFullDf$CountyFIPS, function(CountyFIPS) {
  ret <- tibble(CountyFIPS, FirstUmbraContactTime=as.character(NA), LastUmbraContactTime=as.character(NA))
  countyShp <- subset(eclipseCountiesShp, GEOID==CountyFIPS)
  countyUmbraIntersection <- gIntersection(countyShp, umbraWithTime, byid=c(FALSE, TRUE))
  if (!is.null(countyUmbraIntersection)) {
    umbraShapeIDs <- map_chr(countyUmbraIntersection@polygons, function(umbraPolygon) {
      umbraPolygon@ID
    })
    tdf <- umbraWithTimeData %>% filter(id %in% umbraShapeIDs) %>% arrange(UTCTime)
    ret <- bind_cols(
      tdf %>% head(1) %>% rename(FirstUmbraContactTime=UTCTime) %>% select(FirstUmbraContactTime) %>% mutate_all(as.character),
      tdf %>% tail(1) %>% rename(LastUmbraContactTime=UTCTime) %>% select(LastUmbraContactTime) %>% mutate_all(as.character),
      tibble(CountyFIPS)
    )
  } else {
    writeLines(paste0('County ', CountyFIPS, ' had no umbra intersection'))
  }
  ret
}) %>%
  mutate(FirstUmbraContactTime=as_datetime(ifelse(is.na(FirstUmbraContactTime), NA, paste0('2017-08-21 ', FirstUmbraContactTime)), tz='UTC'),
         LastUmbraContactTime=as_datetime(ifelse(is.na(LastUmbraContactTime), NA, paste0('2017-08-21 ', LastUmbraContactTime)), tz='UTC'))

eclipseCountiesFullDf <- eclipseCountiesFullDf %>% inner_join(umbraCountyContact, by='CountyFIPS') %>%
  mutate(UmbraDuration=(LastUmbraContactTime - FirstUmbraContactTime) / dseconds()) %>%
  select(
    CountyName,
    StateName,
    StateAbbr,
    CountyFIPS,
    StateFIPS,
    CountySeat,
    FirstUmbraContactTime,
    LastUmbraContactTime,
    UmbraDuration,
    TotalPopulation,
    Area,
    Trump=trump,
    Clinton=clinton
  )

write_csv(eclipseCountiesFullDf, 'eclipse-counties.csv')

ggplot(mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon(data=statesSdf, fill='grey80') +
  geom_path(data=statesSdf) +
  geom_polygon(data=eclipseCountiesSdf, fill='white') +
  geom_path(data=eclipseCountiesSdf) +
  geom_path(data=eclipseUmbraSdf, color='pink') +
  coord_map() +
  theme_void()

