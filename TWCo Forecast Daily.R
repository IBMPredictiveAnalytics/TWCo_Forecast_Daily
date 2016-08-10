#inputdate
#fake inputdata
latitudeData <- c("48.139311","48.832953")
longitudeData <- c("11.580559","2.740986")
startDateData <- c("20150615","20150615")
endDateData <- c("20150915","20150915")

modelerData <- data.frame(lat = latitudeData,
                          long = longitudeData,
                          startDate = startDateData,
                          endDate = endDateData)

packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(httr)
packages(plyr)

#input
input_apikey <- "c47ca39d21f8348bd586d0248fecaab3"
#requestType has two value c("is_geocode", "is_postalCode")
input_radio_location <- "is_geocode"
input_col_lat <- "lat"
input_col_long <- "long"
input_col_zipcode <- "zipcode"
input_units <- "e"
# e = English units
# m = Metric units
# h = Hybrid units (UK)
# s  = Metric SI units (not available for all APIs)
#We don't want language but use EN_us only.


#constant
g_url_basic <- "https://api.weather.com/v1/"
g_url_function <-"forecast/daily/10day.json?language=en-US"

funGenerateURL <- function(url_Location){
  sUrlResult <- paste(
    g_url_basic, url_Location, g_url_function,
    "&apiKey=", input_apikey,
    "&units=", input_units,
    sep=""
  )
  return(sUrlResult)
}

funGenerateLocationURL <- function(locationtype = "is_geocode", zipcode = "", lat = "", long = ""){
  #sUrlResult = ""
  if(locationtype == "is_postalCode"){
    sUrlResult <- paste(
      "location/", zipcode, "/", sep=""
    )
  }else{
    sUrlResult <- paste(
      "geocode", "/",
      lat, "/",
      long, "/",
      sep=""
    )
  }
  return(sUrlResult)
}


retriveDataFromTWC <- function(modelerDataIter){
  print(modelerDataIter)
  sRequestURL <- funGenerateURL(
    funGenerateLocationURL(input_radio_location, 
                           modelerDataIter[input_col_zipcode],
                           modelerDataIter[input_col_lat],
                           modelerDataIter[input_col_long])
  )
  print(sRequestURL)
  resultContext <- content(GET(sRequestURL))
  processOneRecord <- function(observations){
    if(is.null(observations$day)){
      #observations$max_temp <- NA
      observations$day <- lapply(observations$night,function(x){return(NA)})
    }
    result <- unlist(lapply(observations,processEachCell))
    #print(length(result))
    #print(names(result))
    return(result)
  }
  
  processEachCell <- function(cell){
    if(is.null(cell)) return(NA)
    if(is.list(cell)) return(lapply(cell,processEachCell))
    else return(cell)
  }
  
  resultData <- ldply (lapply(resultContext$forecasts,processOneRecord))
  return(data.frame(latitude=resultContext$metadata$latitude, 
                    longitude=resultContext$metadata$longitude,
                    resultData))
}

modelerData <- ldply(apply(modelerData, 1, FUN = retriveDataFromTWC),data.frame)

valLatitude<-c(fieldName="latitude",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valLongitude<-c(fieldName="longitude",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valClass<-c(fieldName="class",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valExpire_time_gmt<-c(fieldName="expire_time_gmt",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valFcst_valid<-c(fieldName="fcst_valid",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valFcst_valid_local<-c(fieldName="fcst_valid_local",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNum<-c(fieldName="num",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valMax_temp<-c(fieldName="max_temp",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valMin_temp<-c(fieldName="min_temp",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valTorcon<-c(fieldName="torcon",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valStormcon<-c(fieldName="stormcon",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valBlurb<-c(fieldName="blurb",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valBlurb_author<-c(fieldName="blurb_author",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valLunar_phase_day<-c(fieldName="lunar_phase_day",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDow<-c(fieldName="dow",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valLunar_phase<-c(fieldName="lunar_phase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valLunar_phase_code<-c(fieldName="lunar_phase_code",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valSunrise<-c(fieldName="sunrise",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valSunset<-c(fieldName="sunset",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valMoonrise<-c(fieldName="moonrise",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valMoonset<-c(fieldName="moonset",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valQualifier_code<-c(fieldName="qualifier_code",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valQualifier<-c(fieldName="qualifier",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNarrative<-c(fieldName="narrative",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valQpf<-c(fieldName="qpf",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valSnow_qpf<-c(fieldName="snow_qpf",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valSnow_range<-c(fieldName="snow_range",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valSnow_phrase<-c(fieldName="snow_phrase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valSnow_code<-c(fieldName="snow_code",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.fcst_valid<-c(fieldName="night.fcst_valid",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.fcst_valid_local<-c(fieldName="night.fcst_valid_local",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.day_ind<-c(fieldName="night.day_ind",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.thunder_enum<-c(fieldName="night.thunder_enum",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.daypart_name<-c(fieldName="night.daypart_name",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.long_daypart_name<-c(fieldName="night.long_daypart_name",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.alt_daypart_name<-c(fieldName="night.alt_daypart_name",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.thunder_enum_phrase<-c(fieldName="night.thunder_enum_phrase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.num<-c(fieldName="night.num",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.temp<-c(fieldName="night.temp",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.hi<-c(fieldName="night.hi",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.wc<-c(fieldName="night.wc",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.pop<-c(fieldName="night.pop",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.icon_extd<-c(fieldName="night.icon_extd",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.icon_code<-c(fieldName="night.icon_code",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.wxman<-c(fieldName="night.wxman",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.phrase_12char<-c(fieldName="night.phrase_12char",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.phrase_22char<-c(fieldName="night.phrase_22char",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.phrase_32char<-c(fieldName="night.phrase_32char",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.subphrase_pt1<-c(fieldName="night.subphrase_pt1",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.subphrase_pt2<-c(fieldName="night.subphrase_pt2",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.subphrase_pt3<-c(fieldName="night.subphrase_pt3",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.precip_type<-c(fieldName="night.precip_type",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.rh<-c(fieldName="night.rh",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.wspd<-c(fieldName="night.wspd",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.wdir<-c(fieldName="night.wdir",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.wdir_cardinal<-c(fieldName="night.wdir_cardinal",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.clds<-c(fieldName="night.clds",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.pop_phrase<-c(fieldName="night.pop_phrase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.temp_phrase<-c(fieldName="night.temp_phrase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.accumulation_phrase<-c(fieldName="night.accumulation_phrase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.wind_phrase<-c(fieldName="night.wind_phrase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.shortcast<-c(fieldName="night.shortcast",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.narrative<-c(fieldName="night.narrative",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.qpf<-c(fieldName="night.qpf",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.snow_qpf<-c(fieldName="night.snow_qpf",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.snow_range<-c(fieldName="night.snow_range",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.snow_phrase<-c(fieldName="night.snow_phrase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.snow_code<-c(fieldName="night.snow_code",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.vocal_key<-c(fieldName="night.vocal_key",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.qualifier_code<-c(fieldName="night.qualifier_code",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.qualifier<-c(fieldName="night.qualifier",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.uv_index_raw<-c(fieldName="night.uv_index_raw",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.uv_index<-c(fieldName="night.uv_index",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.uv_warning<-c(fieldName="night.uv_warning",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.uv_desc<-c(fieldName="night.uv_desc",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.golf_index<-c(fieldName="night.golf_index",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valNight.golf_category<-c(fieldName="night.golf_category",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.fcst_valid<-c(fieldName="day.fcst_valid",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.fcst_valid_local<-c(fieldName="day.fcst_valid_local",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.day_ind<-c(fieldName="day.day_ind",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.thunder_enum<-c(fieldName="day.thunder_enum",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.daypart_name<-c(fieldName="day.daypart_name",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.long_daypart_name<-c(fieldName="day.long_daypart_name",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.alt_daypart_name<-c(fieldName="day.alt_daypart_name",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.thunder_enum_phrase<-c(fieldName="day.thunder_enum_phrase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.num<-c(fieldName="day.num",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.temp<-c(fieldName="day.temp",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.hi<-c(fieldName="day.hi",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.wc<-c(fieldName="day.wc",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.pop<-c(fieldName="day.pop",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.icon_extd<-c(fieldName="day.icon_extd",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.icon_code<-c(fieldName="day.icon_code",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.wxman<-c(fieldName="day.wxman",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.phrase_12char<-c(fieldName="day.phrase_12char",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.phrase_22char<-c(fieldName="day.phrase_22char",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.phrase_32char<-c(fieldName="day.phrase_32char",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.subphrase_pt1<-c(fieldName="day.subphrase_pt1",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.subphrase_pt2<-c(fieldName="day.subphrase_pt2",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.subphrase_pt3<-c(fieldName="day.subphrase_pt3",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.precip_type<-c(fieldName="day.precip_type",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.rh<-c(fieldName="day.rh",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.wspd<-c(fieldName="day.wspd",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.wdir<-c(fieldName="day.wdir",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.wdir_cardinal<-c(fieldName="day.wdir_cardinal",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.clds<-c(fieldName="day.clds",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.pop_phrase<-c(fieldName="day.pop_phrase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.temp_phrase<-c(fieldName="day.temp_phrase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.accumulation_phrase<-c(fieldName="day.accumulation_phrase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.wind_phrase<-c(fieldName="day.wind_phrase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.shortcast<-c(fieldName="day.shortcast",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.narrative<-c(fieldName="day.narrative",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.qpf<-c(fieldName="day.qpf",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.snow_qpf<-c(fieldName="day.snow_qpf",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.snow_range<-c(fieldName="day.snow_range",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.snow_phrase<-c(fieldName="day.snow_phrase",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.snow_code<-c(fieldName="day.snow_code",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.vocal_key<-c(fieldName="day.vocal_key",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.qualifier_code<-c(fieldName="day.qualifier_code",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.qualifier<-c(fieldName="day.qualifier",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.uv_index_raw<-c(fieldName="day.uv_index_raw",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.uv_index<-c(fieldName="day.uv_index",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.uv_warning<-c(fieldName="day.uv_warning",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.uv_desc<-c(fieldName="day.uv_desc",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.golf_index<-c(fieldName="day.golf_index",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")
valDay.golf_category<-c(fieldName="day.golf_category",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",fieldRole="")

modelerDataModel <- data.frame(valLatitude,
                               valLongitude,
                               valClass,
                               valExpire_time_gmt,
                               valFcst_valid,
                               valFcst_valid_local,
                               valNum,
                               valMax_temp,
                               valMin_temp,
                               valTorcon,
                               valStormcon,
                               valBlurb,
                               valBlurb_author,
                               valLunar_phase_day,
                               valDow,
                               valLunar_phase,
                               valLunar_phase_code,
                               valSunrise,
                               valSunset,
                               valMoonrise,
                               valMoonset,
                               valQualifier_code,
                               valQualifier,
                               valNarrative,
                               valQpf,
                               valSnow_qpf,
                               valSnow_range,
                               valSnow_phrase,
                               valSnow_code,
                               valNight.fcst_valid,
                               valNight.fcst_valid_local,
                               valNight.day_ind,
                               valNight.thunder_enum,
                               valNight.daypart_name,
                               valNight.long_daypart_name,
                               valNight.alt_daypart_name,
                               valNight.thunder_enum_phrase,
                               valNight.num,
                               valNight.temp,
                               valNight.hi,
                               valNight.wc,
                               valNight.pop,
                               valNight.icon_extd,
                               valNight.icon_code,
                               valNight.wxman,
                               valNight.phrase_12char,
                               valNight.phrase_22char,
                               valNight.phrase_32char,
                               valNight.subphrase_pt1,
                               valNight.subphrase_pt2,
                               valNight.subphrase_pt3,
                               valNight.precip_type,
                               valNight.rh,
                               valNight.wspd,
                               valNight.wdir,
                               valNight.wdir_cardinal,
                               valNight.clds,
                               valNight.pop_phrase,
                               valNight.temp_phrase,
                               valNight.accumulation_phrase,
                               valNight.wind_phrase,
                               valNight.shortcast,
                               valNight.narrative,
                               valNight.qpf,
                               valNight.snow_qpf,
                               valNight.snow_range,
                               valNight.snow_phrase,
                               valNight.snow_code,
                               valNight.vocal_key,
                               valNight.qualifier_code,
                               valNight.qualifier,
                               valNight.uv_index_raw,
                               valNight.uv_index,
                               valNight.uv_warning,
                               valNight.uv_desc,
                               valNight.golf_index,
                               valNight.golf_category,
                               valDay.fcst_valid,
                               valDay.fcst_valid_local,
                               valDay.day_ind,
                               valDay.thunder_enum,
                               valDay.daypart_name,
                               valDay.long_daypart_name,
                               valDay.alt_daypart_name,
                               valDay.thunder_enum_phrase,
                               valDay.num,
                               valDay.temp,
                               valDay.hi,
                               valDay.wc,
                               valDay.pop,
                               valDay.icon_extd,
                               valDay.icon_code,
                               valDay.wxman,
                               valDay.phrase_12char,
                               valDay.phrase_22char,
                               valDay.phrase_32char,
                               valDay.subphrase_pt1,
                               valDay.subphrase_pt2,
                               valDay.subphrase_pt3,
                               valDay.precip_type,
                               valDay.rh,
                               valDay.wspd,
                               valDay.wdir,
                               valDay.wdir_cardinal,
                               valDay.clds,
                               valDay.pop_phrase,
                               valDay.temp_phrase,
                               valDay.accumulation_phrase,
                               valDay.wind_phrase,
                               valDay.shortcast,
                               valDay.narrative,
                               valDay.qpf,
                               valDay.snow_qpf,
                               valDay.snow_range,
                               valDay.snow_phrase,
                               valDay.snow_code,
                               valDay.vocal_key,
                               valDay.qualifier_code,
                               valDay.qualifier,
                               valDay.uv_index_raw,
                               valDay.uv_index,
                               valDay.uv_warning,
                               valDay.uv_desc,
                               valDay.golf_index,
                               valDay.golf_category)