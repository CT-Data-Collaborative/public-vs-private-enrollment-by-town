library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
library(tidyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Public vs Private Enrollment by Town
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

#Get state data
geography=geo.make(state=09)
yearlist=c(2010:2018)
span = 5
col.names="pretty" 
key="ed0e58d2538fb239f51e01643745e83f380582d7"
options(scipen=999)

state_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  variable =list()      
  for (k in seq_along(1:49)) {
   number = number=paste0("B14002", "_", sprintf("%03d",k))
   variable = c(variable, number)
   k=k+1
  }  
  variable <- as.character(variable)    
  data <- try(acs.fetch(geography=geography, endyear=endyear, span=span, 
                    variable = variable, key=key))
  print(data)
  year <- data@endyear
  print(paste("Processing: ", year))
  year <- paste(year-4, year, sep="-")
  geo <- data@geography
  geo$NAME <- NULL
  ## NUMBER MEASURES ##
    # Totals by grade
    prek.total <- acsSum(data, c(4,28), "Pre-K;Total")
    k.total  <- acsSum(data, c(7,31), "K;Total")
    grade1to4.total  <- acsSum(data, c(10,34), "Grades 1 to 4;Total")
    grade5to8.total  <- acsSum(data, c(13,37), "Grades 5 to 8;Total")
    grade9to12.total     <- acsSum(data, c(16,40), "Grades 9 to 12;Total")

    # public by grade
    prek.public <- acsSum(data, c(5,29), "Pre-K;Public")
    k.public <- acsSum(data, c(8,32), "K;Public")
    grade1to4.public <- acsSum(data, c(11,35), "Grades 1 to 4;Public")
    grade5to8.public <- acsSum(data, c(14,38), "Grades 5 to 8;Public")
    grade9to12.public <- acsSum(data, c(17,41), "Grades 9 to 12;Public")
    total.public <- acsSum(data, c(5,8,11,14,17,29,32,35,38,41), "Total;Public")

    # private by grade
    prek.private <- acsSum(data, c(6,30), "Pre-K;Private")
    k.private <- acsSum(data, c(9,33), "K;Private")
    grade1to4.private <- acsSum(data, c(12,36), "Grades 1 to 4;Private")
    grade5to8.private <- acsSum(data, c(15,39), "Grades 5 to 8;Private")
    grade9to12.private <- acsSum(data, c(18,42), "Grades 9 to 12;Private")
    total.private <- acsSum(data, c(6,9,12,15,18,30,33,36,39,42), "Total;Private")

    # Universe - people over the age of 3 enrolled in school between prek and grade 12
    total <- acsSum(data, c(4, 7, 10, 13, 16, 28, 31, 34, 37, 40), "Total;Total")
    
    numberEstimates <- data.table(
            geo,
            estimate(total),
            estimate(prek.total),
            estimate(k.total),
            estimate(grade1to4.total),
            estimate(grade5to8.total),
            estimate(grade9to12.total),
            estimate(prek.public),
            estimate(k.public),
            estimate(grade1to4.public),
            estimate(grade5to8.public),
            estimate(grade9to12.public),
            estimate(total.public),
            estimate(prek.private),
            estimate(k.private),
            estimate(grade1to4.private),
            estimate(grade5to8.private),
            estimate(grade9to12.private),
            estimate(total.private),
            `Year` = year,
            `Measure Type` = "Number",
            `Variable` = "Enrolled Students"
        )
    numberMOES <- data.table(
            geo,
            standard.error(total) * 1.645,
            standard.error(prek.total) * 1.645,
            standard.error(k.total) * 1.645,
            standard.error(grade1to4.total) * 1.645,
            standard.error(grade5to8.total) * 1.645,
            standard.error(grade9to12.total) * 1.645,
            standard.error(prek.public) * 1.645,
            standard.error(k.public) * 1.645,
            standard.error(grade1to4.public) * 1.645,
            standard.error(grade5to8.public) * 1.645,
            standard.error(grade9to12.public) * 1.645,
            standard.error(total.public) * 1.645,
            standard.error(prek.private) * 1.645,
            standard.error(k.private) * 1.645,
            standard.error(grade1to4.private) * 1.645,
            standard.error(grade5to8.private) * 1.645,
            standard.error(grade9to12.private) * 1.645,
            standard.error(total.private) * 1.645,
            `Year` = year,
            `Measure Type` = "Number",
            `Variable` = "Margins of Error"
        )

    ## PERCENT MEASURES ##
    #total universe
    total.percent <- divide.acs(total, total, method = "proportion")
    acs.colnames(total.percent) <- "Total;Total"

    # public by grade - divided by total for given grades
    prek.public <- divide.acs(prek.public, prek.total, method = "proportion")
    acs.colnames(prek.public) <- "Pre-K;Public"
    k.public <- divide.acs(k.public, k.total, method = "proportion")
    acs.colnames(k.public) <- "K;Public"
    grade1to4.public <- divide.acs(grade1to4.public, grade1to4.total, method = "proportion")
    acs.colnames(grade1to4.public) <- "Grades 1 to 4;Public"
    grade5to8.public <- divide.acs(grade5to8.public, grade5to8.total, method = "proportion")
    acs.colnames(grade5to8.public) <- "Grades 5 to 8;Public"
    grade9to12.public <- divide.acs(grade9to12.public, grade9to12.total, method = "proportion")
    acs.colnames(grade9to12.public) <- "Grades 9 to 12;Public"

    # grade = Total, Type = public - divide by total enrollment
    total.public <- divide.acs(total.public, total, method = "proportion")
    acs.colnames(total.public) <- "Total;Public"

    # private by grade - divided by total for given grades
    prek.private <- divide.acs(prek.private, prek.total, method = "proportion")
    acs.colnames(prek.private) <- "Pre-K;Private"
    k.private <- divide.acs(k.private, k.total, method = "proportion")
    acs.colnames(k.private) <- "K;Private"
    grade1to4.private <- divide.acs(grade1to4.private, grade1to4.total, method = "proportion")
    acs.colnames(grade1to4.private) <- "Grades 1 to 4;Private"
    grade5to8.private <- divide.acs(grade5to8.private, grade5to8.total, method = "proportion")
    acs.colnames(grade5to8.private) <- "Grades 5 to 8;Private"
    grade9to12.private <- divide.acs(grade9to12.private, grade9to12.total, method = "proportion")
    acs.colnames(grade9to12.private) <- "Grades 9 to 12;Private"

    # grade = Total, Type = private - divide by total enrollment
    total.private <- divide.acs(total.private, total, method = "proportion")
    acs.colnames(total.private) <- "Total;Private"

    # Totals by grade - Divided by total enrollment
    prek.total <- divide.acs(prek.total, total, method = "proportion")
    acs.colnames(prek.total) <- "Pre-K;Total"
    k.total <- divide.acs(k.total, total, method = "proportion")
    acs.colnames(k.total) <- "K;Total"
    grade1to4.total <- divide.acs(grade1to4.total, total, method = "proportion")
    acs.colnames(grade1to4.total) <- "Grades 1 to 4;Total"
    grade5to8.total <- divide.acs(grade5to8.total, total, method = "proportion")
    acs.colnames(grade5to8.total) <- "Grades 5 to 8;Total"
    grade9to12.total <- divide.acs(grade9to12.total, total, method = "proportion")
    acs.colnames(grade9to12.total) <- "Grades 9 to 12;Total"

    percentEstimates <- data.table(
            geo,
            estimate(total.percent),
            estimate(prek.total),
            estimate(k.total),
            estimate(grade1to4.total),
            estimate(grade5to8.total),
            estimate(grade9to12.total),
            estimate(prek.public),
            estimate(k.public),
            estimate(grade1to4.public),
            estimate(grade5to8.public),
            estimate(grade9to12.public),
            estimate(total.public),
            estimate(prek.private),
            estimate(k.private),
            estimate(grade1to4.private),
            estimate(grade5to8.private),
            estimate(grade9to12.private),
            estimate(total.private),
            `Year` = year,
            `Measure Type` = "Percent",
            `Variable` = "Enrolled Students"
        )
    percentMOES <- data.table(
                geo,
                standard.error(total.percent)  *1.645,
                standard.error(prek.total)  *1.645,
                standard.error(k.total)  *1.645,
                standard.error(grade1to4.total)  *1.645,
                standard.error(grade5to8.total)  *1.645,
                standard.error(grade9to12.total)  *1.645,
                standard.error(prek.public)  *1.645,
                standard.error(k.public)  *1.645,
                standard.error(grade1to4.public)  *1.645,
                standard.error(grade5to8.public)  *1.645,
                standard.error(grade9to12.public)  *1.645,
                standard.error(total.public)  *1.645,
                standard.error(prek.private)  *1.645,
                standard.error(k.private)  *1.645,
                standard.error(grade1to4.private)  *1.645,
                standard.error(grade5to8.private)  *1.645,
                standard.error(grade9to12.private)  *1.645,
                standard.error(total.private)  *1.645,
                `Year` = year,
                `Measure Type` = "Percent",
                `Variable` = "Margins of Error"
            )
    
    data <- rbind(
        numberEstimates,
        numberMOES,
        percentEstimates,
        percentMOES
    )

    data <- melt(
            data,
            id.vars=c("state", "Year", "Measure Type", "Variable"),
            variable.name="Grade;Type",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )

    state_data <- rbind(state_data, data)
}

#get county data
geography=geo.make(state=09, county="*", county.subdivision = "*")

town_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  variable =list()      
  for (k in seq_along(1:49)) {
   number = number=paste0("B14002", "_", sprintf("%03d",k))
   variable = c(variable, number)
   k=k+1
  }  
  variable <- as.character(variable)    
  data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                    variable = variable, key=key)
  year <- data@endyear
  print(paste("Processing: ", year))
  year <- paste(year-4, year, sep="-")
  geo <- data@geography
  geo$county <- sprintf("%02d", geo$county)
  geo$county <- gsub("^", "090", geo$county)
  geo$FIPS <- paste0(geo$county, geo$countysubdivision)
  geo$state <- NULL
  geo$NAME <- NULL
  geo$countysubdivision <- NULL
  geo$county <- NULL 

  ## NUMBER MEASURES ##
    # Totals by grade
    prek.total <- acsSum(data, c(4,28), "Pre-K;Total")
    k.total  <- acsSum(data, c(7,31), "K;Total")
    grade1to4.total  <- acsSum(data, c(10,34), "Grades 1 to 4;Total")
    grade5to8.total  <- acsSum(data, c(13,37), "Grades 5 to 8;Total")
    grade9to12.total     <- acsSum(data, c(16,40), "Grades 9 to 12;Total")

    # public by grade
    prek.public <- acsSum(data, c(5,29), "Pre-K;Public")
    k.public <- acsSum(data, c(8,32), "K;Public")
    grade1to4.public <- acsSum(data, c(11,35), "Grades 1 to 4;Public")
    grade5to8.public <- acsSum(data, c(14,38), "Grades 5 to 8;Public")
    grade9to12.public <- acsSum(data, c(17,41), "Grades 9 to 12;Public")
    total.public <- acsSum(data, c(5,8,11,14,17,29,32,35,38,41), "Total;Public")

    # private by grade
    prek.private <- acsSum(data, c(6,30), "Pre-K;Private")
    k.private <- acsSum(data, c(9,33), "K;Private")
    grade1to4.private <- acsSum(data, c(12,36), "Grades 1 to 4;Private")
    grade5to8.private <- acsSum(data, c(15,39), "Grades 5 to 8;Private")
    grade9to12.private <- acsSum(data, c(18,42), "Grades 9 to 12;Private")
    total.private <- acsSum(data, c(6,9,12,15,18,30,33,36,39,42), "Total;Private")

    # Universe - people over the age of 3 enrolled in school between prek and grade 12
    total <- acsSum(data, c(4, 7, 10, 13, 16, 28, 31, 34, 37, 40), "Total;Total")
    
    numberEstimates <- data.table(
            geo,
            estimate(total),
            estimate(prek.total),
            estimate(k.total),
            estimate(grade1to4.total),
            estimate(grade5to8.total),
            estimate(grade9to12.total),
            estimate(prek.public),
            estimate(k.public),
            estimate(grade1to4.public),
            estimate(grade5to8.public),
            estimate(grade9to12.public),
            estimate(total.public),
            estimate(prek.private),
            estimate(k.private),
            estimate(grade1to4.private),
            estimate(grade5to8.private),
            estimate(grade9to12.private),
            estimate(total.private),
            `Year` = year,
            `Measure Type` = "Number",
            `Variable` = "Enrolled Students"
        )
    numberMOES <- data.table(
            geo,
            standard.error(total) * 1.645,
            standard.error(prek.total) * 1.645,
            standard.error(k.total) * 1.645,
            standard.error(grade1to4.total) * 1.645,
            standard.error(grade5to8.total) * 1.645,
            standard.error(grade9to12.total) * 1.645,
            standard.error(prek.public) * 1.645,
            standard.error(k.public) * 1.645,
            standard.error(grade1to4.public) * 1.645,
            standard.error(grade5to8.public) * 1.645,
            standard.error(grade9to12.public) * 1.645,
            standard.error(total.public) * 1.645,
            standard.error(prek.private) * 1.645,
            standard.error(k.private) * 1.645,
            standard.error(grade1to4.private) * 1.645,
            standard.error(grade5to8.private) * 1.645,
            standard.error(grade9to12.private) * 1.645,
            standard.error(total.private) * 1.645,
            `Year` = year,
            `Measure Type` = "Number",
            `Variable` = "Margins of Error"
        )

    ## PERCENT MEASURES ##
    #total universe
    total.percent <- divide.acs(total, total, method = "proportion")
    acs.colnames(total.percent) <- "Total;Total"

    # public by grade - divided by total for given grades
    prek.public <- divide.acs(prek.public, prek.total, method = "proportion")
    acs.colnames(prek.public) <- "Pre-K;Public"
    k.public <- divide.acs(k.public, k.total, method = "proportion")
    acs.colnames(k.public) <- "K;Public"
    grade1to4.public <- divide.acs(grade1to4.public, grade1to4.total, method = "proportion")
    acs.colnames(grade1to4.public) <- "Grades 1 to 4;Public"
    grade5to8.public <- divide.acs(grade5to8.public, grade5to8.total, method = "proportion")
    acs.colnames(grade5to8.public) <- "Grades 5 to 8;Public"
    grade9to12.public <- divide.acs(grade9to12.public, grade9to12.total, method = "proportion")
    acs.colnames(grade9to12.public) <- "Grades 9 to 12;Public"

    # grade = Total, Type = public - divide by total enrollment
    total.public <- divide.acs(total.public, total, method = "proportion")
    acs.colnames(total.public) <- "Total;Public"

    # private by grade - divided by total for given grades
    prek.private <- divide.acs(prek.private, prek.total, method = "proportion")
    acs.colnames(prek.private) <- "Pre-K;Private"
    k.private <- divide.acs(k.private, k.total, method = "proportion")
    acs.colnames(k.private) <- "K;Private"
    grade1to4.private <- divide.acs(grade1to4.private, grade1to4.total, method = "proportion")
    acs.colnames(grade1to4.private) <- "Grades 1 to 4;Private"
    grade5to8.private <- divide.acs(grade5to8.private, grade5to8.total, method = "proportion")
    acs.colnames(grade5to8.private) <- "Grades 5 to 8;Private"
    grade9to12.private <- divide.acs(grade9to12.private, grade9to12.total, method = "proportion")
    acs.colnames(grade9to12.private) <- "Grades 9 to 12;Private"

    # grade = Total, Type = private - divide by total enrollment
    total.private <- divide.acs(total.private, total, method = "proportion")
    acs.colnames(total.private) <- "Total;Private"

    # Totals by grade - Divided by total enrollment
    prek.total <- divide.acs(prek.total, total, method = "proportion")
    acs.colnames(prek.total) <- "Pre-K;Total"
    k.total <- divide.acs(k.total, total, method = "proportion")
    acs.colnames(k.total) <- "K;Total"
    grade1to4.total <- divide.acs(grade1to4.total, total, method = "proportion")
    acs.colnames(grade1to4.total) <- "Grades 1 to 4;Total"
    grade5to8.total <- divide.acs(grade5to8.total, total, method = "proportion")
    acs.colnames(grade5to8.total) <- "Grades 5 to 8;Total"
    grade9to12.total <- divide.acs(grade9to12.total, total, method = "proportion")
    acs.colnames(grade9to12.total) <- "Grades 9 to 12;Total"

    percentEstimates <- data.table(
            geo,
            estimate(total.percent),
            estimate(prek.total),
            estimate(k.total),
            estimate(grade1to4.total),
            estimate(grade5to8.total),
            estimate(grade9to12.total),
            estimate(prek.public),
            estimate(k.public),
            estimate(grade1to4.public),
            estimate(grade5to8.public),
            estimate(grade9to12.public),
            estimate(total.public),
            estimate(prek.private),
            estimate(k.private),
            estimate(grade1to4.private),
            estimate(grade5to8.private),
            estimate(grade9to12.private),
            estimate(total.private),
            `Year` = year,
            `Measure Type` = "Percent",
            `Variable` = "Enrolled Students"
        )
    percentMOES <- data.table(
                geo,
                standard.error(total.percent)  *1.645,
                standard.error(prek.total)  *1.645,
                standard.error(k.total)  *1.645,
                standard.error(grade1to4.total)  *1.645,
                standard.error(grade5to8.total)  *1.645,
                standard.error(grade9to12.total)  *1.645,
                standard.error(prek.public)  *1.645,
                standard.error(k.public)  *1.645,
                standard.error(grade1to4.public)  *1.645,
                standard.error(grade5to8.public)  *1.645,
                standard.error(grade9to12.public)  *1.645,
                standard.error(total.public)  *1.645,
                standard.error(prek.private)  *1.645,
                standard.error(k.private)  *1.645,
                standard.error(grade1to4.private)  *1.645,
                standard.error(grade5to8.private)  *1.645,
                standard.error(grade9to12.private)  *1.645,
                standard.error(total.private)  *1.645,
                `Year` = year,
                `Measure Type` = "Percent",
                `Variable` = "Margins of Error"
            )
    
    data <- rbind(
        numberEstimates,
        numberMOES,
        percentEstimates,
        percentMOES
    )

    data <- melt(
            data,
            id.vars=c("FIPS", "Year", "Measure Type", "Variable"),
            variable.name="Grade;Type",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )

    town_data <- rbind(town_data, data)
}

names(state_data)[1] <- "FIPS"

dataset <- rbind(state_data, town_data)

#Final Additions, processing
# Split Grade and School Type out of appropriate column, then drop that column
dataset[,c("Grade", "School Type"):=do.call(Map, c(f=c, strsplit(`Grade;Type`, ";", fixed=T)))]
dataset[,`Grade;Type` := NULL]

# Round Values according to type/variable
dataset[`Measure Type` == "Number", Value := round(Value, 0)]
dataset[`Measure Type` != "Number", Value := round(Value*100, 1)]
dataset[Variable == "Margins of Error", Value := round(Value, 0)]

#Set NaNs to NAs
dataset$Value[dataset$Value == "NaN"] <- NA

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

dataset <- merge(dataset, towns, by = "FIPS", all.y=T)

dataset$Grade <- factor(dataset$Grade, levels = c("Total", "Pre-K", 
                                                  "K", "Grades 1 to 4", 
                                                  "Grades 5 to 8", "Grades 9 to 12"))

dataset$`School Type` <- factor(dataset$`School Type`, levels = c("Total", "Public", "Private"))

dataset <- dataset %>% 
  select(Town, FIPS, Year, Grade, `School Type`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, Grade, `School Type`, `Measure Type`, Variable)

write.table(
    dataset,
    file.path("data", "public-vs-private-enrollment-town-2018.csv"),
    sep = ",",
    row.names = F,
    na = "-9999"
)

