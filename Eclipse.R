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
eclipseCountiesShpData <- map_df(eclipseCountiesShp@polygons, function(polygon) {
  data.frame(ID=polygon@ID) %>% mutate_all(as.character)
}) %>% inner_join(countiesDf, by='ID')
rownames(eclipseCountiesShpData) <- eclipseCountiesShpData$ID
eclipseCountiesShp <- SpatialPolygonsDataFrame(eclipseCountiesShp, eclipseCountiesShpData)

# note: convert area from sq meters to sq miles
eclipseCountiesDf <- map_df(eclipseCountiesShp@data$GEOID, function(CountyFIPS) {
  ss <- subset(eclipseCountiesShp, GEOID==CountyFIPS)
  p4s <- paste0('+proj=aea +lat_1=', ss@bbox['y','min'], ' +lat_2=', ss@bbox['y','max'],
                    ' +lon_1=', ss@bbox['x','min'], ' +lon_2=', ss@bbox['x','max'], ' +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  crs <- CRS(p4s)
  ss <- spTransform(ss, crs)
  polygon <- ss@polygons[[1]]
  tibble(ID=polygon@ID, AreaInUmbra=gArea(ss))
}) %>% inner_join(countiesDf, by='ID') %>%
  select(StateFIPS=STATEFP, CountyFIPS=GEOID, CountyName=NAME, ShapefileID=ID, ALAND, AWATER, AreaInUmbra) %>%
  mutate(Area=(as.numeric(as.character(ALAND)) + as.numeric(as.character(AWATER)))*3.86102e-7,
         AreaInUmbra=AreaInUmbra*3.86102e-7) %>%
  select(-ALAND, -AWATER) %>%
  mutate_at(vars(-Area, -AreaInUmbra), as.character) %>%
  inner_join(statesDf, by='StateFIPS')

eclipseCountiesShp <- subset(countiesShp, GEOID %in% eclipseCountiesDf$CountyFIPS)

eclipseCountiesSdf <- fortify(eclipseCountiesShp)
statesSdf <- fortify(statesShp)
eclipseUmbraSdf <- fortify(eclipseUmbraShp)

census_api_key(Sys.getenv('CENSUS_API_KEY'))
acsVariables <- load_variables(2015, 'acs5', cache=TRUE)

acsData <- get_acs(geography='county',
                   variables=c('B01001_001E',
                               'B19019_001E',
                               'B01002_001E',
                               'B15003_002E','B15003_003E','B15003_004E','B15003_005E','B15003_006E','B15003_007E','B15003_008E','B15003_009E','B15003_010E','B15003_011E','B15003_012E','B15003_013E','B15003_014E','B15003_015E','B15003_016E',
                               'B15003_017E','B15003_018E','B15003_019E','B15003_020E','B15003_021E',
                               'B15003_022E','B15003_023E','B15003_024E','B15003_025E',
                               'B19066_001E',
                               'B19313_001E',
                               'B25079_001E',
                               'B25001_001E',
                               'B23025_003E',
                               'B23025_004E'), output='wide') %>%
  mutate(LessThanHighSchoolGrad=B15003_002E+B15003_003E+B15003_004E+B15003_005E+B15003_006E+B15003_007E+B15003_008E+B15003_009E+B15003_010E+B15003_011E+B15003_012E+B15003_013E+B15003_014E+B15003_015E+B15003_016E,
         HighSchoolGrad=B15003_017E+B15003_018E+B15003_019E+B15003_020E+B15003_021E,
         BachelorAndAbove=B15003_022E+B15003_023E+B15003_024E+B15003_025E) %>%
  select(CountyFIPS=GEOID,
         TotalPopulation=B01001_001E,
         MedianHouseholdIncome=B19019_001E,
         MedianAge=B01002_001E,
         LessThanHighSchoolGrad, HighSchoolGrad, BachelorAndAbove,
         AggregateSSI=B19066_001E,
         AggregateIncome=B19313_001E,
         AggregateOOHousingValue=B25079_001E,
         HousingUnits=B25001_001E,
         CivilianLaborForce=B23025_003E,
         CivilianEmployed=B23025_004E)

electionData2016 <- read_csv('https://query.data.world/s/KZ3ZPaNQIiw85_R4ZSXw9--gNxAfoR') %>%
  select(County, trump, clinton, totalvotes)
registrationData2016 <- read_csv('https://query.data.world/s/wPKv3e5isFUb0wxKyk8jdMowYtgvCQ') %>%
  filter(Year==2016 & Month==11) %>%
  select(County, RegisteredVoters=Total)

eclipseCountiesFullDf <- inner_join(eclipseCountiesDf, acsData, by='CountyFIPS') %>%
  inner_join(electionData2016, by=c('CountyFIPS'='County')) %>%
  inner_join(registrationData2016, by=c('CountyFIPS'='County'))

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
    AreaInUmbra,
    Trump=trump,
    Clinton=clinton,
    Total2016Votes=totalvotes,
    RegisteredVoters,
    MedianHouseholdIncome,
    MedianAge,
    LessThanHighSchoolGrad, HighSchoolGrad, BachelorAndAbove,
    AggregateSSI,
    AggregateIncome,
    AggregateOOHousingValue,
    HousingUnits,
    CivilianLaborForce,
    CivilianEmployed
  )

cbp <- read_csv('/opt/data/Census/cbp15co.txt')
cbp <- cbp %>%
  mutate(CountyFIPS=paste0(FIPSTATE, FIPSCTY)) %>%
  select(CountyFIPS, NAICS, MidMarchEmployees=EMP, Establishments=EST) %>%
  filter(NAICS %in% c(
    '------',
    '721110',
    '721120',
    '712130',
    '712190',
    '72251/'
  )) %>%
  mutate(Industry=case_when(
    .$NAICS=='------' ~ 'Total',
    .$NAICS=='721110' | .$NAICS=='721120' ~ 'Hotel',
    .$NAICS=='712130' ~ 'ZooBotanical',
    .$NAICS=='712190' ~ 'NaturePark',
    .$NAICS=='72251/' ~ 'Restaurant'
  ))

cbp <- inner_join(
  cbp %>%
    select(-NAICS, -MidMarchEmployees) %>%
    group_by(CountyFIPS, Industry) %>%
    summarize(Establishments=sum(Establishments)) %>%
    spread(Industry, Establishments) %>% ungroup() %>%
    rename_at(vars(-CountyFIPS), funs(sprintf("Establishments%s", .))),
  cbp %>%
    select(-NAICS, -Establishments) %>%
    group_by(CountyFIPS, Industry) %>%
    summarize(MidMarchEmployees=sum(MidMarchEmployees)) %>%
    spread(Industry, MidMarchEmployees) %>% ungroup() %>%
    rename_at(vars(-CountyFIPS), funs(sprintf("MidMarchEmployees%s", .))),
  by='CountyFIPS'
)

eclipseCountiesFullDf <- eclipseCountiesFullDf %>% left_join(cbp, by='CountyFIPS')

write_csv(eclipseCountiesFullDf, 'eclipse-counties.csv')

ggplot(mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon(data=statesSdf, fill='grey80') +
  geom_path(data=statesSdf) +
  geom_polygon(data=eclipseCountiesSdf, fill='white') +
  geom_path(data=eclipseCountiesSdf) +
  geom_path(data=eclipseUmbraSdf, color='pink') +
  coord_map() +
  theme_void()

