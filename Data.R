library(readxl)
DataBike <- read_excel("Data/Original/Amostra_BikeRio.xlsx", 
                       col_types = c("numeric", "text", "numeric", 
                                     "numeric", "date", "text", "date", 
                                     "numeric", "text", "numeric", "text", 
                                     "text", "text", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "text", "text"))
View(DataBike)


# Data Manipulation


  # dataframe of stations

station_name <- unique(DataBike$`Nome da estação de início`)

library(stringr)

idstation <- str_extract(station_name, '\\d+') 
namestation <- gsub('\\d+ - ', '', station_name)

df.station <- data.frame(idstation, namestation)


# dataframe of bikes

df.bikes <- data.frame(DataBike[,c(12, 14, 15)])
  

  # dataframe of dates

library(lubridate)
lubridate::hm
format(DataBike$`Data de início`, '%Y-%m-%d')

df.time <- data.frame(ID=DataBike$ID, 
                      StartDate=format(DataBike$`Data de início`, '%Y-%m-%d'), 
                      StartTime=format(DataBike$`Data de início`, '%H:%M:%S'),
                      EndDate=format(DataBike$`Data final`, '%Y-%m-%d'),
                      EndTime=format(DataBike$`Data final`, '%H:%M:%S'),
                      Duraion=DataBike$Duração)


  # dataframe of products

df.product <- data.frame(unique(DataBike[,c(20,22)]))



  ## main dataframe ##

library(dplyr)
df.main <- DataBike %>% select(ID, `ID da estação de início`,
                               `ID da estação final`, `MSN-bc da bicicleta`, 
                               `ID do produto`)

