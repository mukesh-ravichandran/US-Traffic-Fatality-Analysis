library(dplyr)
library(chron)
library(lubridate)
library(datetime)
library(usmap)
library(tidycensus)
library(gtools)
setwd("/Users/mukeshravichandran/OneDrive - North Carolina State University/MSA-Mukesh’s MacBook Pro/Side Project/Traffic/Visualization Project/CSV/Accidents")

temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

temp[i]

dfs = sapply(.GlobalEnv, is.data.frame)

combined= do.call(smartbind, mget(names(dfs)[dfs]))




colnames(combined)

# [1] "STATE"      "ST_CASE"    "VE_TOTAL"   "VE_FORMS"   "PVH_INVL"   "PEDS"       "PERNOTMVIT" "PERMVIT"    "PERSONS"    "COUNTY"     "CITY"       "DAY"        "MONTH"
# [14] "YEAR"       "DAY_WEEK"   "HOUR"       "MINUTE"     "NHS"        "RUR_URB"    "FUNC_SYS"   "RD_OWNER"   "ROUTE"      "TWAY_ID"    "TWAY_ID2"   "MILEPT"     "LATITUDE"
# [27] "LONGITUD"   "SP_JUR"     "HARM_EV"    "MAN_COLL"   "RELJCT1"    "RELJCT2"    "TYP_INT"    "WRK_ZONE"   "REL_ROAD"   "LGT_COND"   "WEATHER1"   "WEATHER2"   "WEATHER"
# [40] "SCH_BUS"    "RAIL"       "NOT_HOUR"   "NOT_MIN"    "ARR_HOUR"   "ARR_MIN"    "HOSP_HR"    "HOSP_MN"    "CF1"        "CF2"        "CF3"        "FATALS"     "DRUNK_DR"

#Data Dictionary

"""VE_TOTAL --> Number of Vehicles in Crash
PVH_INVL --> Number of Parked/Working Vehicles in the Crash
PEDS --> Number of Persons Not in Motor Vehicles
PERNOTMVIT --> Number of Persons Not in Motor Vehicles in Transport
PERMIVIT --> Number of Persons in Motor Vehicles In-Transport
PERSONS --> Number of Person Forms
FUNC_SYS --> functional classification of the segment of the trafficway
RD_OWNER --> legal ownership of the segment of the trafficway
ROUTE ->? route signing of the trafficway on which the crash
TWAY --> element records the trafficway on which the crash occurred
MILEPT --> milepoint nearest to the location where the crash occurred
SP_JUR --> if the location on the trafficway where the crash occurred qualifies as a Special Jurisdiction
HARM_EV --> first injury or damage producing event of the crash
MAN_COLL --> orientation of two motor vehicles in-transport when they are involved in the “First Harmful Event” of a collision crash
RELJCT1 --> presence in an interchange area
RELJCT2 --> crash's location with respect to presence in or proximity to components typically in junction or interchange areas
TYP_INT --> identifies and allows separation of various intersection types
WRK_ZONE --> within the boundaries of a work zone or on an approach to or exit from a work zone
REL_ROAD --> location of the crash as it relates to its position within or outside the trafficway based
SCH_BUS -->if a school bus, or motor vehicle functioning as a school bus, is related to the crash
RAIL --> if the crash occurred in or near a rail grade crossing
NOT_HOUR --> hour that emergency medical service was notified
NOT_MIN --> minutes after the hour that emergency medical service was notified
ARR_HOUR --> hour that emergency medical service arrived on the crash scene
ARR_MIN --> the minutes after the hour that emergency medical service arrived on the crash scene
HOSP_HR --> hour that emergency medical service arrived at the treatment facility
HOSP_MN --> minutes after the hour that emergency medical service arrived at the treatment facility
CF --> factors related to the crash expressed by the investigating officer
FATALS --> number of fatally injured persons
DRUNK_DR --> Number of Drinking Drivers Involved in the Fatal Crash"""

#univariate analysis

plot(sort(table(combined$STATE),TRUE)[1:10])
plot(sort(table(combined$STATE),FALSE)[1:10])
table(combined$VE_TOTAL)
plot(table(combined$DAY))
plot(table(combined$HOUR))

combined$date = as.Date(with(combined, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%B-%d")
combined$HOUR= sub("-.*", "",combined$HOUR)
combined$HOUR= parse_date_time(combined$HOUR, '%H:%M%p')
combined$HOUR= format(combined$HOUR, '%H')
combined$time = paste(combined$HOUR,combined$MINUTE, sep = ":")
combined$time= as.time(combined$time)

barplot(table(month(combined$date)))

barplot(table(combined$HOUR))

barplot(table(combined$DRUNK_DR))
TEMP1= fips(combined$STATE)

TEMP2= sprintf("%03d",combined$COUNTY)
temp3= paste(TEMP1,TEMP2,sep= "")
combined$CODE= temp3

county_death= aggregate(FATALS ~ CODE, data =combined, FUN = sum )
colnames(county_death)= c("fips", "death")


usmap::plot_usmap(regions = "states", data = county_death, values = "death",include= "CA")+ scale_fill_continuous(name = "Death", label = scales::comma) +
  theme(legend.position = "right")

write_csv(county_death, "/Users/mukeshravichandran/OneDrive - North Carolina State University/MSA-Mukesh’s MacBook Pro/Side Project/Traffic/Visualization Project/Creations/deathsbycounty.csv")

write_csv(combined, "/Users/mukeshravichandran/OneDrive - North Carolina State University/MSA-Mukesh’s MacBook Pro/Side Project/Traffic/Visualization Project/Creations/combined.csv")



