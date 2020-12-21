#BEGINNING OF FILE
library("dplyr")
library(stringr)
library(RCurl)
library(httr)

#I added this
library(covidcastR)
library(jsonlite)
# Import
source('/Users/rishibasu/Downloads/ASEP COVID/PM_COVID-master- current/delphi_epidata.R')
# Fetch data
res <- Epidata$fluview(list('nat'), list(201440, Epidata$range(201501, 201510)))
cat(paste(res$result, res$message, length(res$epidata), "\n"))

#date_of_study = "05-27-2020"
#date_of_study = "07-16-2020"
date_of_study = "08-18-2020"

# Historical data
covid_hist = read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv"))
covid_us_hist = subset(covid_hist, Country_Region == "US" & is.na(FIPS)==F)

# Import outcome data from JHU CSSE
#covid = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_of_study,".csv"))) 
covid = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/08-18-2020.csv")))
covid_us = subset(covid,Country_Region == "US")
#get rid of last two columns
covid_us = subset(covid_us, select = -c(Incidence_Rate,Case.Fatality_Ratio) )
covid_us = rbind(covid_us,subset(covid_us_hist, (!(FIPS %in% covid_us$FIPS))  & Confirmed == 0 & Deaths == 0 & is.na(FIPS)==F))
covid_us$FIPS = str_pad(covid_us$FIPS, 5, pad = "0")

# Import exposure PM2.5 data
county_pm = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_pm25.csv"))

county_temp = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/temp_seasonal_county.csv"))
# Import census, brfss, testing, mortality, hosptial beds data as potential confounders
county_census = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/census_county_interpolated.csv"))
county_brfss<-read.csv(text=getURL("https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020.csv"),skip = 1)
county_brfss<-county_brfss[,c('fipscode','v011_rawvalue','v009_rawvalue')]
names(county_brfss)<-c('fips','obese','smoke')
county_brfss$fips = str_pad(county_brfss$fips, 5, pad = "0")

#state_test = read.csv(text=getURL("https://covidtracking.com/api/v1/states/daily.csv"))
#ERROR
#state_test = read.csv("/Macintosh HD/Users/rishibasu/Downloads/daily1.csv", header=TRUE)

#MANUALLY IMPORT DAILY1.CSV !!!

#state_test = subset(state_test, date ==paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))[,-20]
state_test = subset(daily1, date ==paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))[,-20]
statecode = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/statecode.csv"))

hospitals = read.csv(text=getURL("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"))
hospitals$BEDS[hospitals$BEDS < 0] = NA

#unsure the function of these lines and use of these datasets, will keep an eye out for them
county_base_mortality = read.table(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_base_mortality.txt"), sep = "",header = T)
county_old_mortality = read.table(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_old_mortality.txt"), sep = "",header = T)
county_014_mortality = read.table("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_014_mortality.txt", sep = "",header = T)
county_1544_mortality = read.table("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_1544_mortality.txt", sep = "",header = T)
county_4564_mortality = read.table("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_4564_mortality.txt", sep = "",header = T)

colnames(county_old_mortality)[4] = c("older_Population")
colnames(county_014_mortality)[4] = c("014_Population")
colnames(county_1544_mortality)[4] = c("1544_Population")
colnames(county_4564_mortality)[4] = c("4564_Population")

county_base_mortality = merge(county_base_mortality,county_old_mortality[,c(2,4)] ,by = "County.Code",all.x = T)
county_base_mortality = merge(county_base_mortality,county_014_mortality[,c(2,4)] ,by = "County.Code",all.x = T)
county_base_mortality = merge(county_base_mortality,county_1544_mortality[,c(2,4)] ,by = "County.Code",all.x = T)
county_base_mortality = merge(county_base_mortality,county_4564_mortality[,c(2,4)] ,by = "County.Code",all.x = T)

#error (fixed- added .x to column names) #changed back to original
county_base_mortality$older_pecent = county_base_mortality$older_Population/county_base_mortality$Population
county_base_mortality$"young_pecent" = county_base_mortality$"014_Population"/county_base_mortality$Population
county_base_mortality$"prime_pecent" = county_base_mortality$"1544_Population"/county_base_mortality$Population
county_base_mortality$"mid_pecent" = county_base_mortality$"4564_Population"/county_base_mortality$Population
county_base_mortality$"older_pecent"[is.na(county_base_mortality$"older_pecent")] = 0
county_base_mortality$"prime_pecent"[is.na(county_base_mortality$"prime_pecent")] = 0
county_base_mortality$"mid_pecent"[is.na(county_base_mortality$"mid_pecent")] = 0
county_base_mortality$"young_pecent"[is.na(county_base_mortality$"young_pecent")] = 0

# Import NCHS Urban-Rural Classification Scheme for Counties
NCHSURCodes2013 = read.csv("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/NCHSURCodes2013.csv")
NCHSURCodes2013$FIPS = str_pad(NCHSURCodes2013$FIPS, 5, pad = "0")

# URL not found, not sure what application this has anyway, will ignore it for now
# Import FB survey on covid-like sympton data
#script <- getURL("https://raw.githubusercontent.com/cmu-delphi/delphi-epidata/master/src/client/delphi_epidata.R", ssl.verifypeer = FALSE)

#res = GET(https://delphi.cmu.edu/epidata/api.php)

#script <- covidcast_signal("fb-survey", "smoothed_cli", start_day = "20200501",
                          # end_day = "20200716")

script <- covidcast_signal("fb-survey", "smoothed_cli", start_day = "20200501",
                           end_day = "20200818")
#not sure what this line does
eval(parse(text = script))

# Import social distancing measure data
#This may be outdated, but I don't know where to find new stuff (for example, does not include any reopenings)
state_policy = read.csv("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/state_policy0410.csv")
colnames(state_policy)[6] = "stay_at_home"

# merging data
state_test = merge(state_test,statecode,by.x = "state" ,by.y = "Code" )
#Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column 
state_test = merge(state_test,state_policy[,c(1,6)],by = "State")
#Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
#state_test = merge(state_test,state_policy[,c(1,6)], by.x = "State.x", by.y="State")
#Error in `$<-.data.frame`(`*tmp*`, date_since_social, value = numeric(0)) : 
state_test$date_since_social = as.numeric(as.Date(Sys.Date()) - as.Date((strptime(state_test$stay_at_home, "%m/%d/%Y"))))
#Error in `$<-.data.frame`(`*tmp*`, date_since_social, value = 0) : 
state_test[is.na(state_test$date_since_social)==T,]$date_since_social = 0

# pm2.5 average over 17 years
county_pm_aggregated = county_pm %>% 
  group_by(fips) %>% 
  summarise(mean_pm25 = mean(pm25))

# temperature and relative humidity average over 17 years
county_temp_aggregated = county_temp %>% 
  group_by(fips) %>% 
  summarise(mean_winter_temp= mean(winter_tmmx),
            mean_summer_temp= mean(summer_tmmx),
            mean_winter_rm= mean(winter_rmax),
            mean_summer_rm= mean(summer_rmax))

county_pm_aggregated = merge(county_pm_aggregated,county_temp_aggregated,by="fips",all.x = T)

county_hospitals_aggregated = hospitals %>%
  group_by(COUNTYFIPS) %>%
  summarise(beds = sum(BEDS, na.rm=TRUE))
county_hospitals_aggregated$COUNTYFIPS = str_pad(county_hospitals_aggregated$COUNTYFIPS, 5, pad = "0")

county_census_aggregated2 = subset(county_census, year==2016)

county_census_aggregated2$q_popdensity = 1
quantile_popdensity = quantile(county_census_aggregated2$popdensity,c(0.2,0.4,0.6,0.8))
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity<=quantile_popdensity[1]] = 1
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[1] &
                                         county_census_aggregated2$popdensity<=quantile_popdensity[2]] = 2
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[2] &
                                         county_census_aggregated2$popdensity<=quantile_popdensity[3]] = 3
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[3] &
                                         county_census_aggregated2$popdensity<=quantile_popdensity[4]] = 4
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[4]] = 5

county_census_aggregated2$fips = str_pad(county_census_aggregated2$fips, 5, pad = "0")
county_census_aggregated2 = merge(county_census_aggregated2,county_brfss,
                                  by="fips",all.x=T)

county_pm_aggregated$fips = str_pad(county_pm_aggregated$fips, 5, pad = "0")
aggregate_pm = merge(county_pm_aggregated,covid_us,by.x="fips",by.y = "FIPS")

aggregate_pm_census = merge(aggregate_pm,county_census_aggregated2,by.x="fips",by.y = "fips")

county_base_mortality$County.Code = str_pad(county_base_mortality$County.Code, 5, pad = "0")
aggregate_pm_census_cdc = merge(aggregate_pm_census,county_base_mortality[,c(1,4,12:15)],by.x = "fips",by.y = "County.Code",all.x = T)

aggregate_pm_census_cdc = aggregate_pm_census_cdc[is.na(aggregate_pm_census_cdc$fips) ==F,]

aggregate_pm_census_cdc_test = merge(aggregate_pm_census_cdc,state_test[,-22],by.x="Province_State",by.y = "State")

#Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
#aggregate_pm_census_cdc_test_beds = merge(aggregate_pm_census_cdc_test,county_hospitals_aggregated,by.x = "fips",by.y = "COUNTYFIPS",all.x = T)
colnames(county_hospitals_aggregated)[1] <- "fips.x"
aggregate_pm_census_cdc_test_beds = merge(aggregate_pm_census_cdc_test,county_hospitals_aggregated,by =c("fips.x"),all.x = T)
aggregate_pm_census_cdc_test_beds$beds[is.na(aggregate_pm_census_cdc_test_beds$beds)] = 0
colnames(county_hospitals_aggregated)[1] <- "COUNTYFIPS"
colnames(aggregate_pm_census_cdc_test)[2] <- "fips"
colnames(aggregate_pm_census_cdc_test_beds)[1] <- "fips"

# Import outcome data from JHU CSSE, calculate the timing of the 1st confirmed case for each county
date_of_all = format(seq(as.Date("2020-03-22"), as.Date(strptime(date_of_study,"%m-%d-%Y")), by = "days"),"%m-%d-%Y")

#THIS IS MY ERROR
#I FIXED IT
covid_us_daily_confirmed = lapply(date_of_all,
                                  function(date_of_all){
                                    #covid_daily = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_of_all,".csv")))
                                    #covid_daily = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",date_of_all,".csv")))
                                    #covid_daily = read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
                                    covid_daily = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", date_of_all,".csv")))
                                    #make this next line piece every day together
                                    #covid_daily = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/06-06-2020.csv",date_of_all,".csv")))
                                    #covid_daily = read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/06-06-2020.csv"))
                                    covid_daily = covid_daily[!duplicated(covid_daily$FIPS),]
                                    return(subset(covid_daily,Country_Region == "US" & is.na(FIPS)!=T & Confirmed >0 ))
                                  }
)

covid_us_new_confirmed = list()
covid_us_new_confirmed[1] = covid_us_daily_confirmed[1]
#not adding anything to the list
date_since=150
covid_us_new_confirmed[[1]]$date_since = length(covid_us_daily_confirmed) 
#all of this is good

#this is where I fill in the rest of covid_us_new_confirmed
#Error in `$<-.data.frame`(`*tmp*`, "date_since", value = 67) : replacement has 1 row, data has 0 
# I don't understand how to read this function
#covid_us_new_confirmed[2:length(date_of_all)] =  lapply(2:(length(covid_us_daily_confirmed)),
                                                        #function(i){
                                                        #  covid_us_new_confirmed =subset(covid_us_daily_confirmed[[i]],!(FIPS %in% unlist(sapply(1:(i-1),function(k)covid_us_daily_confirmed[[k]]$FIPS))))
                                                        #  covid_us_new_confirmed$date_since = length(covid_us_daily_confirmed) - i + 1
                                                        #  return(covid_us_new_confirmed)
                                                       # })

#dropped in from updated version
covid_us_new_confirmed[2:length(date_of_all)] =  lapply(2:(length(covid_us_daily_confirmed)),
                                                        function(i){
                                                          covid_us_new_confirmed =subset(covid_us_daily_confirmed[[i]],!(FIPS %in% unlist(sapply(1:(i-1),function(k)covid_us_daily_confirmed[[k]]$FIPS))))
                                                          if (nrow(covid_us_new_confirmed)>0){
                                                            covid_us_new_confirmed$date_since = length(covid_us_daily_confirmed) - i + 1
                                                            return(covid_us_new_confirmed)
                                                          } else{return(NA)}
                                                        })

#Error in rbind(deparse.level, ...) : numbers of columns of arguments do not match
#covid_us_new_confirmed.df <- do.call("rbind", covid_us_new_confirmed)[,c("FIPS","date_since")]
#covid_us_new_confirmed.df <- do.call("rbind", covid_us_new_confirmed)[,c("FIPS","date_since")]

#covid_us_new_confirmed_start = covid_us_new_confirmed[1:68]
#covid_us_new_confirmed_end = covid_us_new_confirmed[69:150]

covid_us_new_confirmed_start <- do.call("rbind", covid_us_new_confirmed[1:68])[,c("FIPS","date_since")]
covid_us_new_confirmed_end <- do.call("rbind", covid_us_new_confirmed[69:150])[,c("FIPS","date_since")]
covid_us_new_confirmed.df <- rbind (covid_us_new_confirmed_start, covid_us_new_confirmed_end)



#covid_us_new_confirmed_first.df = do.call("rbind", covid_us_new_confirmed_start)[,c("FIPS","date_since")]
#covid_us_new_confirmed_extra.df = do.call("rbind", covid_us_new_confirmed_end)[,c("FIPS","date_since")]
#covid_us_new_confirmed.df = rbind( covid_us_new_confirmed.df, covid_us_new_confirmed_extra.df)

covid_us_new_confirmed.df$FIPS = str_pad(covid_us_new_confirmed.df$FIPS, 5, pad = "0")
aggregate_pm_census_cdc_test_beds = merge(aggregate_pm_census_cdc_test_beds,covid_us_new_confirmed.df,
                                          by.x = "fips",by.y = "FIPS", all.x = T)

#Error in `$<-.data.frame`(`*tmp*`, date_since, value = numeric(0)) : replacement has 0 rows, data has 3093
aggregate_pm_census_cdc_test_beds$date_since[is.na(aggregate_pm_census_cdc_test_beds$date_since)] = 0

aggregate_pm_census_cdc_test_beds = merge(aggregate_pm_census_cdc_test_beds,NCHSURCodes2013[,c(1,7)],
                                          by.x = "fips",by.y="FIPS", all.x = T)


#ALL GOOD UP UNTIL HERE!!!



# Combine five boroughs of NYCnr
aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2=="New York City",]$population =
  subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Bronx"& Province_State=="New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Kings"& Province_State=="New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Queens"& Province_State=="New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Richmond"& Province_State=="New York")$population
aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2=="New York City",]$beds =
  subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Bronx"& Province_State=="New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Kings"& Province_State=="New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Queens"& Province_State=="New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Richmond"& Province_State=="New York")$beds

vars = c("mean_pm25","poverty","medianhousevalue","medhouseholdincome","pct_owner_occ",
         "education","pct_blk","hispanic","older_pecent","prime_pecent","mid_pecent","obese","smoke",
         "mean_summer_temp","mean_summer_rm","mean_winter_temp","mean_winter_rm")
aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2=="New York City",][,vars] = 
  sapply(vars,function(var){
    (subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2=="Bronx"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="Bronx"& Province_State=="New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2=="Kings"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="Kings"& Province_State=="New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2=="Queens"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="Queens"& Province_State=="New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2=="Richmond"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="Richmond"& Province_State=="New York")$population)/(
         subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")$population+subset(aggregate_pm_census_cdc_test_beds,Admin2=="Bronx"& Province_State=="New York")$population+
           subset(aggregate_pm_census_cdc_test_beds, Admin2=="Kings"& Province_State=="New York")$population+ subset(aggregate_pm_census_cdc_test_beds,Admin2=="Queens"& Province_State=="New York")$population +
           subset(aggregate_pm_census_cdc_test_beds,Admin2=="Richmond"& Province_State=="New York")$population)
  })
aggregate_pm_census_cdc_test_beds = subset(aggregate_pm_census_cdc_test_beds,
                                           !(Admin2=="Bronx"& Province_State=="New York")&
                                             !(Admin2=="Kings"& Province_State=="New York")&
                                             !(Admin2=="Queens"& Province_State=="New York")&
                                             !(Admin2=="Richmond"& Province_State=="New York"))
# Error in `[.data.frame`(subset(aggregate_pm_census_cdc_test_beds, Admin2 ==  : undefined columns selected 
#SOMEHOW THE ERROR IS GONE!


# Request FB survey data from CMU COVIDcast Delphi Research Group

#getting rid of data with no results (non-responses)
#edits: put a '' around fips (2x)
#Error: unexpected symbol in "sapply(Epidata$covidcast('fb-survey', 'smoothed_cli', 'day', 'county', list(Epidata$range(20200401, paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_st"
#WHERE IS THIS CRAZY COMMA????
aggregate_pm_census_cdc_test_beds$cli  = 
  sapply(aggregate_pm_census_cdc_test_beds$fips, 
         function(fips){
           if (Epidata$covidcast('fb-survey', 'smoothed_cli', 'day', 'county', list(Epidata$range(20200401, paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))),'fips')[[2]]!="no results"){
             return(mean(sapply(Epidata$covidcast('fb-survey', 'smoothed_cli', 'day', 'county', list(Epidata$range(20200401, paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))),'fips')[[2]],function(i){i$value}),na.rm=T))
           }else {return(NA)}})

#Error in FUN(X[[i]], ...) : object 'Epidata' not found


#new try
#getting rid of data with no results (non-responses)
#aggregate_pm_census_cdc_test_beds$cli  = 
 # sapply(aggregate_pm_census_cdc_test_beds$fips, 
    #     function(fips){
    #       if (covidcast('fb-survey', 'smoothed_cli', 'day', 'county', list(range(20200401, paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))),fips)[[2]]!="no results"){
    #         return(mean(sapply(covidcast('fb-survey', 'smoothed_cli', 'day', 'county', list(range(20200401, paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))),fips)[[2]],function(i){i$value}),na.rm=T))
     #      }else {return(NA)}})

#Error in FUN(X[[i]], ...) : object 'Epidata' not found
