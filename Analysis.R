
# Analysis



  # Most frequent trips

library(dplyr)

trip.od <- table(df.main[,2:3]) %>% 
           as.data.frame() 
names(trip.od) <- c('Start', 'End', 'Freq')

trip.od <- trip.od %>% filter(Freq>0) %>% arrange(desc(Freq))


trip.od$End <- full_join(trip.od, df.station, by=c('End'='idstation'))[,4]
trip.od$Start <- full_join(trip.od, df.station, by=c('Start'='idstation'))[,4]



library(ggplot2)

ggplot(arrange(trip.od, desc(Freq)) %>% top_n(20))+
  theme(axis.text.x = element_text(angle = 45,  hjust=1, size = 6))+
  geom_histogram(aes(forcats::fct_inorder(paste(Start, End, sep=' - ')), Freq), 
                 fill='orangered', stat='identity', width=0.25)+
  labs(x='Origem - Destino')
  


  # Frequency per time

    # Start
    
freqtime <- data.frame(table(cut(df.time$StartDatetime, '15 mins')))

l.freqtime <- split(DataBike, freqtime$Var1)

l.freqtime <- lapply(l.freqtime, function(x) x %>% 
                       dplyr::count(`ID da estação de início`) )



l.freqtime <- as.data.frame(do.call(rbind, l.freqtime))
l.freqtime$time <- str_sub(rownames(l.freqtime), 1, 19)
rownames(l.freqtime) <- 1:nrow(l.freqtime)
colnames(l.freqtime) <- c('Id', 'n', 'time')
l.freqtime <- l.freqtime[,c(3,1,2)]
l.freqtime$Id <- as.factor(l.freqtime$Id)
l.freqtime$Id <- full_join(l.freqtime, df.station, by=c('Id'='idstation'))[,4]
l.freqtime$Id <- as.factor(l.freqtime$Id)
l.freqtime$time <- as.POSIXct(l.freqtime$time, tz='UTC')


    # End

end_freqtime <- data.frame(table(cut(df.time$EndDatetime, '15 mins')))

l.end_freqtime <- split(DataBike, end_freqtime$Var1)

l.end_freqtime <- lapply(l.end_freqtime, function(x) x %>% 
                           dplyr::count(`ID da estação final`) )



l.end_freqtime <- as.data.frame(do.call(rbind, l.end_freqtime))
l.end_freqtime$time <- str_sub(rownames(l.end_freqtime), 1, 19)
rownames(l.end_freqtime) <- 1:nrow(l.end_freqtime)
colnames(l.end_freqtime) <- c('Id', 'n', 'time')
l.end_freqtime <- l.end_freqtime[,c(3,1,2)]
l.end_freqtime$Id <- as.factor(l.end_freqtime$Id)
l.end_freqtime$Id <- full_join(l.end_freqtime, df.station, by=c('Id'='idstation'))[,4]
l.end_freqtime$Id <- as.factor(l.end_freqtime$Id)
l.end_freqtime$time <- as.POSIXct(l.end_freqtime$time, tz='UTC')



    # matrix of demand's distribution


      # Frequency

        #Start

m.freqstation <- data.frame(matrix(0, nrow=nrow(freqtime), ncol=nrow(df.station)))

rownames(m.freqstation) <- freqtime$Var1
colnames(m.freqstation) <- df.station$namestation

for(i in 1:nrow(freqtime)){ # rows
  for(j in 1:nrow(df.station)){ # columns
    common <- ((as.factor(l.freqtime$time) %in% rownames(m.freqstation)[i]) & 
                 (l.freqtime$Id %in% colnames(m.freqstation)[j]))
    if(any(common)==FALSE) {
      m.freqstation[i,j] <- 0
    } else {
      m.freqstation[i,j] <- l.freqtime$n[common]
    }
  }
}


        #End

m.end_freqstation <- data.frame(matrix(0, nrow=nrow(end_freqtime), 
                                       ncol=nrow(df.station)))

rownames(m.end_freqstation) <- end_freqtime$Var1
colnames(m.end_freqstation) <- df.station$namestation

for(i in 1:nrow(end_freqtime)){ # rows
  for(j in 1:nrow(df.station)){ # columns
    common <- ((as.factor(l.end_freqtime$time) %in% rownames(m.end_freqstation)[i]) & 
                 (l.end_freqtime$Id %in% colnames(m.end_freqstation)[j]))
    if(any(common)==FALSE) {
      m.end_freqstation[i,j] <- 0
    } else {
      m.end_freqstation[i,j] <- l.end_freqtime$n[common]
    }
  }
}



  # General statistic information
    
    # Start

mean.arriv <- c()
var.arriv <- c()
sd.arriv <- c()
prop.InOut <- c()

for(i in 1:ncol(m.freqstation)){
  mean.arriv[i] <- mean(m.freqstation[,i])
  var.arriv[i] <- var(m.freqstation[,i])
  sd.arriv[i] <- sd(m.freqstation[,i])
  prop.InOut[i] <- sum(m.end_freqstation[,i])/sum(m.freqstation[,i]) # sum(end)/sum(start)
}

info.arrival <- data.frame(mean.arriv, var.arriv, sd.arriv, prop.InOut)
rownames(info.arrival) <- colnames(m.freqstation)



# End

mean.arriv <- c()
var.arriv <- c()
sd.arriv <- c()
prop.InOut <- c()

for(i in 1:ncol(m.freqstation)){
  mean.arriv[i] <- mean(m.end_freqstation[,i])
  var.arriv[i] <- var(m.end_freqstation[,i])
  sd.arriv[i] <- sd(m.end_freqstation[,i])
  prop.InOut[i] <- sum(m.freqstation[,i])/sum(m.end_freqstation[,i]) # sum(start)/sum(end)
}

info.departure <- data.frame(mean.arriv, var.arriv, sd.arriv, prop.InOut)
rownames(info.departure) <- colnames(m.freqstation)
