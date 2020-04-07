library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)


# input: none
# output: a data.frame with most recent case counts data from JHU in raw format
update_cases_df<-function(){
  df<-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
               header=TRUE, sep=',')
  return(df)
}

# input: none
# output: a data.frame with most recent death counts data from JHU in raw format
update_deaths_df<-function(){
  df<-read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv',
               header=TRUE, sep=',')
  return(df)
}


# input: the data.frame outputted by update_cases_df() OR update_deaths_df() AND 
#        a country name OR vector of country names 
# output: a neatly formatted data.frame including all data of the country or countries
#         in the input. 
#         *the granularity of the data at the province/state specific data is maintained.*
get_counts_by_provinces<-function(df, country_name){
  
  country = df %>% filter(Country.Region %in% country_name)
  country=data.frame(t(country), stringsAsFactors = FALSE)
  
  
  # col_names <- t(country["Province.State",])
  col_names = paste(as.character(country["Country.Region",]),as.character(country["Province.State",]),sep='_')
  
  country=data.frame(country[-1:-4,])
  
  
  if (length(col_names)>1){
    colnames(country)<-col_names

  }
  
  if (length(col_names)==1){
    colnames(country)<-country_name
  }
  
  for (i in 1:dim(country)[2])
  {
    country[,i]<-as.numeric(as.character(country[,i]))
  }

  dates=rownames(country)
  dates=str_replace(dates, "X", "")
  
  rownames(country)=mdy(dates)
  country$date=mdy(dates)
  
  return(country)
}


# input: the data.frame outputted by update_cases_df() OR update_deaths_df() AND 
#        a country name OR vector of country names 
# output: a neatly formatted data.frame including all data of the country or countries
#         in the input. 
#         *the granularity of the data at the province/state specific data is NOT maintained.*
#         *instead all inter-country counts are summed and the outputted data.frame includes*
#         *only one column per country.*
get_counts_by_country<-function(df, country_names){
  dates=colnames(df)[-1:-4]
  dates=str_replace(dates, "X", "")
  
  
  
  output=data.frame(date=mdy(dates))
  
  for (country in country_names)
  {
    country_sum = colSums( filter(df, Country.Region %in% country)[,-1:-4])
    output[,country]=country_sum
    
  }
  
  return(output)
}

# input: the data.frame outputted by get_counts_by_provinces() OR get_counts_by_country() AND
#        a vector of provinces OR "all" IF input data.frame is from get_counts_by_provinces() 
#        (if the input data.frame is from get_counts_by_country() then leave provinces="all" default input)  
#        AND plot label in string format
# output: logarithmic chart of case/death counts
plot_log<-function(df, provinces="all",lab="case/death?"){
  if (sum(provinces=="all")!=0)
  {
    meltdf <- melt(df, id="date")
  }
  else
  {
    meltdf <- melt(df[,c(provinces, "date")], id="date")
  }
  
  ggplot(meltdf,aes(x=date,y=log(value),colour=variable,group=variable)) + geom_line() + ggtitle(paste(lab,": logarithmic chart"))
  
}

# input: the data.frame outputted by get_counts_by_provinces() OR get_counts_by_country() AND
#        a vector of provinces OR "all" IF input data.frame is from get_counts_by_provinces() 
#        (if the input data.frame is from get_counts_by_country() then leave provinces="all" default input)  
#        AND plot label in string format
# output: linear chart of case/death counts
plot_linear<-function(df, provinces="all",lab="case/death?"){
  if (sum(provinces=="all")!=0)
  {
    meltdf <- melt(df, id="date")
  }
  else
  {
    meltdf <- melt(df[,c(provinces, "date")], id="date")
  }
  
  ggplot(meltdf,aes(x=date,y=value,colour=variable,group=variable)) + geom_line() + ggtitle(paste(lab,": linear chart"))
  
}




                                      #################
                                      # EXAMPLE USAGE #
                                      #################

# cases_df=update_cases_df()
# 
# deaths_df=update_deaths_df()
# 
# 
# 
# country_names=c("Canada", "Australia", "Italy", "France", "Vietnam")
# 
# 
# countries_cases=get_counts_by_country(cases_df,country_names)
# countries_deaths_df=get_counts_by_country(deaths_df,country_names)
# 
# 
# # Cases by countries
# plot_log(countries_cases,lab="Cases")
# plot_linear(df=countries_cases,lab="Cases")
# 
# # Deaths by countries
# plot_log(countries_deaths_df,lab="Deaths")
# plot_linear(df=countries_deaths_df,lab="Deaths")
# 
# 
# 
# 
# 
# 
# country_name="Canada"
# Canada_cases=get_counts_by_country(cases_df,country_name)
# 
# Canada_deaths=get_counts_by_country(deaths_df,country_name)
# 
# 
# 
# plot_log(Canada_cases,lab="Cases")
# plot_linear(Canada_cases, lab="Cases")
# 
# plot_log(Canada_deaths,lab="Deaths")
# plot_linear(Canada_deaths, lab="Deaths")
# 
# 
# provinces_cases=get_counts_by_provinces(cases_df,country_name)
# 
# provinces_deaths=get_counts_by_provinces(deaths_df,country_name)
# 
# 
# # plot Canadian provinces: cases
# plot_log(provinces_cases,provinces=c("Canada_Ontario", "Canada_Quebec","Canada_British Columbia"), lab="Cases")
# plot_linear(provinces_cases,provinces=c("Canada_Ontario", "Canada_Quebec","Canada_British Columbia"), lab="Cases")
# 
# # plot Canadian provinces: deaths
# plot_log(provinces_deaths,provinces=c("Canada_Ontario", "Canada_Quebec","Canada_British Columbia"), lab="Deaths")
# plot_linear(provinces_deaths,provinces=c("Canada_Ontario", "Canada_Quebec","Canada_British Columbia"), lab="Deaths")
