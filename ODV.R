

# Load libraries
library(metR)
library(tidyverse)
library(lubridate)
library(reshape2)
library(MBA)
library(mgcv)
library(chron)
library(RPMG)


ctd <- read.csv("Multiparametrica Calpe/Datos/Datos_def_cada_0.2metro_corrected_0001.csv", sep = ";", dec = ".")

ctd <- mutate(ctd, Depth  = -Depth) #Correct for plotting


########################################################################### 
########################################################################### 
############################### Datos Calpe ################################
############################### Temperatura ################################ 
########################################################################### 

#En el tercer dato, a??ade despues del ctd$ la columna que quieras representar.
ctd2 <- data.frame(ctd$Date, ctd$Depth, ctd$Temp)
colnames(ctd2) <- c("Date", "Depth", "Data")
ctd2$Date<-as.POSIXct(ctd2$Date, format = "%Y-%m-%d",tz = "GMT")
#ctd2 <- subset(ctd2, ctd2$Date>="2017-04-25" & ctd2$Date<="2018-10-16") #Seleccionar rango de fechas

# No hace falta que cambies el nombre de las columnas, no afecta en nada.
colnames(ctd2) <- c("Date", "Depth", "Data")

# Manually extracted hexidecimal ODV colour palette
ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")
# pastel, rojo, naranja, verde, azul claro, azul oscuro, morado


datosrt <- as.character(ctd2$Date)
ctd2$Date <- as.POSIXct(datosrt, format = "%Y-%m-%d",tz = "GMT")

ctd2$Date <- decimal_date(ctd2$Date)


ctd2 <- na.omit(ctd2)
# Interpolar los datos
ctd_mba <- mba.surf(ctd2, no.X = 300, no.Y = 300, extend = T)
dimnames(ctd_mba$xyz.est$z) <- list(ctd_mba$xyz.est$x, ctd_mba$xyz.est$y)
ctd_mba <- melt(ctd_mba$xyz.est$z, varnames = c('Date', 'Depth'), value.name = 'Data') %>% 
    filter(Depth < 0) %>% 
    mutate(Data = round(Data, 1))


#Volvemos a poner la fecha en formato datetime POSIXct para poder dibujarlo
ctd_mba$Date<-date_decimal(ctd_mba$Date, tz = "GMT")
ctd2$Date<-date_decimal(ctd2$Date, tz = "GMT")

jpng(paste("Multiparametrica Calpe/Figuras/", "Temp", sep = ""), P=c(18,9.8) ) 

#quartz()
ggplot(data = ctd_mba, aes(x = Date, y = Depth)) +
    
    geom_raster(aes(fill = Data)) +
    scale_fill_gradientn(colours = rev(ODV_colours))+
    
    #geom_contour(aes(z = Data), breaks = c(16, 20, 24, 28), colour = "black", alpha = 0.5) +
    geom_contour(aes(z = Data), binwidth = 1, colour = "black", alpha = 0.3) +
    #geom_contour(aes(z = Data), breaks = 16, colour = "black") +
    #geom_contour(aes(z = Data), breaks = 20, colour = "black") +
    #geom_contour(aes(z = Data), breaks = 24, colour = "black") +
    #geom_contour(aes(z = Data), breaks = 28, colour = "black") +
    #geom_label_contour(aes(z = Data), data = , check_overlap = TRUE)

    labs(y = "Profundidad (m)", x = "Mes", fill = expression(paste("Temperature (",degree,"C)"))) +
    coord_cartesian(expand = 0) +
    scale_x_datetime(date_breaks= "1 month", date_label = "%m/%y") +
    theme(axis.text = element_text(face="bold", size=10),
          axis.title = element_text(face="bold", size=16),
          legend.text = element_text(face="bold", size=16),
          legend.title = element_text(face="bold", size=16, angle = 90)) +
    theme(legend.key.size = unit(1.5, "cm")) + # Tama??o leyenda
    
    guides(colour = guide_legend(title.position = "left"))




{
    dev.off()
   }







########################################################################### 
########################################################################### 
############################### Datos Calpe ################################
################################### Chl #################################### 
########################################################################### 

#En el tercer dato, a??ade despues del ctd$ la columna que quieras representar.
ctd2 <- data.frame(ctd$Date, ctd$Depth, ctd$Chla*10)
colnames(ctd2) <- c("Date", "Depth", "Data")
ctd2$Date<-as.POSIXct(ctd2$Date, format = "%Y-%m-%d",tz = "GMT")
#ctd2 <- subset(ctd2, ctd2$Date>="2017-04-25" & ctd2$Date<="2018-10-16") #Seleccionar rango de fechas

# No hace falta que cambies el nombre de las columnas, no afecta en nada.
colnames(ctd2) <- c("Date", "Depth", "Data")

# Manually extracted hexidecimal ODV colour palette
#ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")
# pastel, rojo, naranja, verde, azul claro, azul oscuro, morado
ODV_colours <- c("#27ab19", "#0db5e6", "#7139fe", "#d16cfa")



datosrt <- as.character(ctd2$Date)
ctd2$Date <- as.POSIXct(datosrt, format = "%Y-%m-%d",tz = "GMT")

ctd2$Date <- decimal_date(ctd2$Date)




ctd2 <- na.omit(ctd2)
# Interpolar los datos
ctd_mba <- mba.surf(ctd2, no.X = 300, no.Y = 300, extend = T)
dimnames(ctd_mba$xyz.est$z) <- list(ctd_mba$xyz.est$x, ctd_mba$xyz.est$y)
ctd_mba <- melt(ctd_mba$xyz.est$z, varnames = c('Date', 'Depth'), value.name = 'Data') %>% 
    filter(Depth < 0) %>% 
    mutate(Data = round(Data, 1))


#Volvemos a poner la fecha en formato datetime POSIXct para poder dibujarlo
ctd_mba$Date<-date_decimal(ctd_mba$Date, tz = "GMT")
ctd2$Date<-date_decimal(ctd2$Date, tz = "GMT")

jpng(paste("Multiparametrica Calpe/Figuras/", "Chl", sep = ""), P=c(18,9.8) ) 

#quartz()
ggplot(data = ctd_mba, aes(x = Date, y = Depth)) +
    
    geom_raster(aes(fill = Data)) +
    #scale_fill_gradientn(colours = rev(ODV_colours))+
    scale_fill_gradientn(colours = rev(ODV_colours), breaks = c(0, 10, 20, 30, 40), labels = c("0", "1", "2", "3", "4")) +
    
    geom_contour(aes(z = Data), binwidth = 5, colour = "black", alpha = 0.2) +
    #geom_contour(aes(z = Data), breaks = 15, colour = "black") +
    #geom_contour(aes(z = Data), breaks = 20, colour = "black") +
    #geom_contour(aes(z = Data), breaks = 25, colour = "black") +
    #geom_contour(aes(z = Data), breaks = 30, colour = "black") +
    labs(y = "Profundidad (m)", x = "Mes", fill = expression(paste("Chla (", mu,"g/L)"))) +
    coord_cartesian(expand = 0) +
    scale_x_datetime(date_breaks= "1 month", date_label = "%m/%y") +
    theme(axis.text = element_text(face="bold", size=10),
          axis.title = element_text(face="bold", size=16),
          legend.text = element_text(face="bold", size=16),
          legend.title = element_text(face="bold", size=16, angle = 90)) +
    theme(legend.key.size = unit(1.5, "cm")) + # Tama??o leyenda
    
    guides(colour = guide_legend(title.position = "left"))




{
    dev.off()
   }












########################################################################### 
########################################################################### 
############################### Datos Calpe ################################
################################ Salinidad ################################# 
########################################################################### 

#En el tercer dato, a??ade despues del ctd$ la columna que quieras representar.
ctd2 <- data.frame(ctd$Date, ctd$Depth, ctd$Sal*10)
colnames(ctd2) <- c("Date", "Depth", "Data")
ctd2$Date<-as.POSIXct(ctd2$Date, format = "%Y-%m-%d",tz = "GMT")
#ctd2 <- subset(ctd2, ctd2$Date>="2017-04-25" & ctd2$Date<="2018-10-16") #Seleccionar rango de fechas

# No hace falta que cambies el nombre de las columnas, no afecta en nada.
colnames(ctd2) <- c("Date", "Depth", "Data")

# Manually extracted hexidecimal ODV colour palette
ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")
# pastel, rojo, naranja, verde, azul claro, azul oscuro, morado
ODV_colours <- c("#0db5e6", "#7139fe", "#d16cfa")



datosrt <- as.character(ctd2$Date)
ctd2$Date <- as.POSIXct(datosrt, format = "%Y-%m-%d",tz = "GMT")

ctd2$Date <- decimal_date(ctd2$Date)


ctd2 <- na.omit(ctd2)
# Interpolar los datos
ctd_mba <- mba.surf(ctd2, no.X = 300, no.Y = 300, extend = T)
dimnames(ctd_mba$xyz.est$z) <- list(ctd_mba$xyz.est$x, ctd_mba$xyz.est$y)
ctd_mba <- melt(ctd_mba$xyz.est$z, varnames = c('Date', 'Depth'), value.name = 'Data') %>% 
    filter(Depth < 0) %>% 
    mutate(Data = round(Data, 1))


#Volvemos a poner la fecha en formato datetime POSIXct para poder dibujarlo
ctd_mba$Date<-date_decimal(ctd_mba$Date, tz = "GMT")
ctd2$Date<-date_decimal(ctd2$Date, tz = "GMT")

jpng(paste("Multiparametrica Calpe/Figuras/", "Salinity", sep = ""), P=c(18,9.8) ) 

#quartz()
ggplot(data = ctd_mba, aes(x = Date, y = Depth)) +
    
    geom_raster(aes(fill = Data)) +
    #scale_fill_gradientn(colours = rev(ODV_colours))+
    scale_fill_gradientn(colours = rev(ODV_colours), breaks = c(370, 372.5, 375, 377.5, 380, 382.5), labels = c("37.00", "37.25", "37.50", "37.75", "38.00", "38.25")) +
    
    geom_contour(aes(z = Data), binwidth = 5, colour = "black", alpha = 0.2) +
    #geom_contour(aes(z = Data), breaks = 15, colour = "black") +
    #geom_contour(aes(z = Data), breaks = 20, colour = "black") +
    #geom_contour(aes(z = Data), breaks = 25, colour = "black") +
    #geom_contour(aes(z = Data), breaks = 30, colour = "black") +
    labs(y = "Profundidad (m)", x = "Mes", fill = "Salinity (psu)") +
    coord_cartesian(expand = 0) +
    scale_x_datetime(date_breaks= "1 month", date_label = "%m/%y") +
    theme(axis.text = element_text(face="bold", size=10),
          axis.title = element_text(face="bold", size=16),
          legend.text = element_text(face="bold", size=16),
          legend.title = element_text(face="bold", size=16, angle = 90)) +
    theme(legend.key.size = unit(1.5, "cm")) + # Tama??o leyenda
    
    guides(colour = guide_legend(title.position = "left"))




{
    dev.off()
   }












########################################################################### 
########################################################################### 
############################### Datos Calpe ################################
################################# Oxigeno ################################## 
########################################################################### 


ctd <- read.csv("Multiparametrica Calpe/Datos/Datos_def_cada_0.2metro_corrected_0000.csv", sep = ";", dec = ".")

ctd <- mutate(ctd, Depth  = -Depth) #Correct for plotting


#En el tercer dato, a??ade despues del ctd$ la columna que quieras representar.
ctd2 <- data.frame(ctd$Date, ctd$Depth, ctd$DO*10)
colnames(ctd2) <- c("Date", "Depth", "Data")
ctd2$Date<-as.POSIXct(ctd2$Date, format = "%Y-%m-%d",tz = "GMT")
#ctd2 <- subset(ctd2, ctd2$Date>="2017-04-25" & ctd2$Date<="2018-10-16") #Seleccionar rango de fechas

# No hace falta que cambies el nombre de las columnas, no afecta en nada.
colnames(ctd2) <- c("Date", "Depth", "Data")

# Manually extracted hexidecimal ODV colour palette
ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")
# pastel, rojo, naranja, verde, azul claro, azul oscuro, morado
ODV_colours <- c("#0db5e6", "#7139fe", "#d16cfa")



datosrt <- as.character(ctd2$Date)
ctd2$Date <- as.POSIXct(datosrt, format = "%Y-%m-%d",tz = "GMT")

ctd2$Date <- decimal_date(ctd2$Date)


ctd2 <- na.omit(ctd2)
# Interpolar los datos
ctd_mba <- mba.surf(ctd2, no.X = 300, no.Y = 300, extend = T)
dimnames(ctd_mba$xyz.est$z) <- list(ctd_mba$xyz.est$x, ctd_mba$xyz.est$y)
ctd_mba <- melt(ctd_mba$xyz.est$z, varnames = c('Date', 'Depth'), value.name = 'Data') %>% 
    filter(Depth < 0) %>% 
    mutate(Data = round(Data, 1))


#Volvemos a poner la fecha en formato datetime POSIXct para poder dibujarlo
ctd_mba$Date<-date_decimal(ctd_mba$Date, tz = "GMT")
ctd2$Date<-date_decimal(ctd2$Date, tz = "GMT")

jpng(paste("Multiparametrica Calpe/Figuras/", "OD", sep = ""), P=c(18,9.8) ) 

#quartz()
ggplot(data = ctd_mba, aes(x = Date, y = Depth)) +
    
    geom_raster(aes(fill = Data)) +
    #scale_fill_gradientn(colours = rev(ODV_colours))+
    scale_fill_gradientn(colours = rev(ODV_colours), breaks = seq(60,100,10), labels = c("6", "7", "8", "9", "10")) +
    
    #geom_contour(aes(z = Data), binwidth = 2, colour = "black", alpha = 0.2) +
    geom_contour(aes(z = Data), breaks = 60, colour = "black",alpha = 0.2) +
    geom_contour(aes(z = Data), breaks = 70, colour = "black",alpha = 0.2) +
    geom_contour(aes(z = Data), breaks = 80, colour = "black",alpha = 0.2) +
    geom_contour(aes(z = Data), breaks = 90, colour = "black",alpha = 0.2) +
    geom_contour(aes(z = Data), breaks = 100, colour = "black",alpha = 0.2) +
    #geom_text_contour(aes(z = Data))
    
    labs(y = "Profundidad (m)", x = "Mes", fill = "OD (mg/L)") +
    coord_cartesian(expand = 0) +
    scale_x_datetime(date_breaks= "1 month", date_label = "%m/%y") +
    theme(axis.text = element_text(face="bold", size=10),
          axis.title = element_text(face="bold", size=16),
          legend.text = element_text(face="bold", size=16),
          legend.title = element_text(face="bold", size=16, angle = 90)) +
    theme(legend.key.size = unit(1.5, "cm")) + # Tama??o leyenda
    
    guides(colour = guide_legend(title.position = "left"))




{
    dev.off()
   }


















########################################################################### 
########################################################################### 
############################### Datos Calpe ################################
############################### Turbidez ################################ 
########################################################################### 


    
ctd <- read.csv("Multiparametrica Calpe/Datos/Datos_def_cada_0.2metro_corrected_0000.csv", sep = ";", dec = ".")
    
ctd <- mutate(ctd, Depth  = -Depth) #Correct for plotting
    
    
#En el tercer dato, a??ade despues del ctd$ la columna que quieras representar.
ctd2 <- data.frame(ctd$Date, ctd$Depth, ctd$Turb*10)
colnames(ctd2) <- c("Date", "Depth", "Data")
ctd2$Date<-as.POSIXct(ctd2$Date, format = "%Y-%m-%d",tz = "GMT")
ctd2 <- subset(ctd2, ctd2$Date>="2017-04-25" & ctd2$Date<="2019-3-27") #Seleccionar rango de fechas
    
# No hace falta que cambies el nombre de las columnas, no afecta en nada.
colnames(ctd2) <- c("Date", "Depth", "Data")
    
# Manually extracted hexidecimal ODV colour palette
ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")
# pastel, rojo, naranja, verde, azul claro, azul oscuro, morado
ODV_colours <- c("#0db5e6", "#7139fe", "#d16cfa")
    

datosrt <- as.character(ctd2$Date)
ctd2$Date <- as.POSIXct(datosrt, format = "%Y-%m-%d",tz = "GMT")

ctd2$Date <- decimal_date(ctd2$Date)


ctd2 <- na.omit(ctd2)
# Interpolar los datos
ctd_mba <- mba.surf(ctd2, no.X = 300, no.Y = 300, extend = T)
dimnames(ctd_mba$xyz.est$z) <- list(ctd_mba$xyz.est$x, ctd_mba$xyz.est$y)
ctd_mba <- melt(ctd_mba$xyz.est$z, varnames = c('Date', 'Depth'), value.name = 'Data') %>% 
    filter(Depth < 0) %>% 
    mutate(Data = round(Data, 1))


#Volvemos a poner la fecha en formato datetime POSIXct para poder dibujarlo
ctd_mba$Date<-date_decimal(ctd_mba$Date, tz = "GMT")
ctd2$Date<-date_decimal(ctd2$Date, tz = "GMT")

jpng(paste("Multiparametrica Calpe/Figuras/", "Turb", sep = ""), P=c(18,9.8) ) 

#quartz()
ggplot(data = ctd_mba, aes(x = Date, y = Depth)) +

    
    
    geom_raster(aes(fill = Data)) +
    #scale_fill_gradientn(colours = rev(ODV_colours))+
    scale_fill_gradientn(colours = rev(ODV_colours), breaks = c(0.626, 4, 8, 12, 16), labels = c("0", "0.4", "0.8", "1.2", "1.6")) +
    
    #geom_contour(aes(z = Data), binwidth = 1, colour = "black", alpha = 0.2) +
    geom_contour(aes(z = Data), breaks = 0.626, colour = "black",alpha = 0.2) +
    geom_contour(aes(z = Data), breaks = 4, colour = "black",alpha = 0.2) +
    geom_contour(aes(z = Data), breaks = 8, colour = "black",alpha = 0.2) +
    geom_contour(aes(z = Data), breaks = 12, colour = "black",alpha = 0.2) +
    geom_contour(aes(z = Data), breaks = 16, colour = "black",alpha = 0.2) +
    
    labs(y = "Profundidad (m)", x = "Mes", fill = "Turbidity") +
    coord_cartesian(expand = 0) +
    scale_x_datetime(date_breaks= "1 month", date_label = "%m/%y") +
    theme(axis.text = element_text(face="bold", size=10),
          axis.title = element_text(face="bold", size=16),
          legend.text = element_text(face="bold", size=16),
          legend.title = element_text(face="bold", size=16, angle = 90)) +
    theme(legend.key.size = unit(1.5, "cm")) + # Tama??o leyenda
    
    guides(colour = guide_legend(title.position = "left"))




{
    dev.off()
   }

    
    
########################################################################### 
########################################################################### 
############################### Datos Calpe ################################
################################### ph #################################### 
########################################################################### 
    
    
    
ctd <- read.csv("Multiparametrica Calpe/Datos/Datos_def_cada_0.2metro_corrected_0000.csv", sep = ";", dec = ".")

ctd <- mutate(ctd, Depth  = -Depth) #Correct for plotting
    
    
#En el tercer dato, a??ade despues del ctd$ la columna que quieras representar.
ctd2 <- data.frame(ctd$Date, ctd$Depth, ctd$pH*10)
colnames(ctd2) <- c("Date", "Depth", "Data")
ctd2$Date<-as.POSIXct(ctd2$Date, format = "%Y-%m-%d",tz = "GMT")
#ctd2 <- subset(ctd2, ctd2$Date>="2017-04-25" & ctd2$Date<="2019-3-27") #Seleccionar rango de fechas
    
# No hace falta que cambies el nombre de las columnas, no afecta en nada.
colnames(ctd2) <- c("Date", "Depth", "Data")
    
# Manually extracted hexidecimal ODV colour palette
ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")
# pastel, rojo, naranja, verde, azul claro, azul oscuro, morado
ODV_colours <- c("#27ab19", "#0db5e6", "#7139fe", "#d16cfa")
    
    
    
datosrt <- as.character(ctd2$Date)
ctd2$Date <- as.POSIXct(datosrt, format = "%Y-%m-%d",tz = "GMT")
    
ctd2$Date <- decimal_date(ctd2$Date)
    
    
ctd2 <- na.omit(ctd2)
# Interpolar los datos
ctd_mba <- mba.surf(ctd2, no.X = 300, no.Y = 300, extend = T)
dimnames(ctd_mba$xyz.est$z) <- list(ctd_mba$xyz.est$x, ctd_mba$xyz.est$y)
ctd_mba <- melt(ctd_mba$xyz.est$z, varnames = c('Date', 'Depth'), value.name = 'Data') %>% 
    filter(Depth < 0) %>% 
    mutate(Data = round(Data, 1))
    

#Volvemos a poner la fecha en formato datetime POSIXct para poder dibujarlo
ctd_mba$Date<-date_decimal(ctd_mba$Date, tz = "GMT")
ctd2$Date<-date_decimal(ctd2$Date, tz = "GMT")

jpng(paste("Multiparametrica Calpe/Figuras/", "pH", sep = ""), P=c(18,9.8) ) 

#quartz()
ggplot(data = ctd_mba, aes(x = Date, y = Depth)) +

    
    
    geom_raster(aes(fill = Data)) +
    #scale_fill_gradientn(colours = rev(ODV_colours))+
    scale_fill_gradientn(colours = rev(ODV_colours), breaks = seq(75,85,1), labels = c("7.5", "7.6", "7.7", "7.8", "7.9", "8", "8.1", "8.2", "8.3", "8.4", "8.5")) +
    
    geom_contour(aes(z = Data), binwidth = 1, colour = "black", alpha = 0.2) +
    #geom_contour(aes(z = Data), breaks = 60, colour = "black",alpha = 0.2) +
    #geom_contour(aes(z = Data), breaks = 70, colour = "black",alpha = 0.2) +
    #geom_contour(aes(z = Data), breaks = 80, colour = "black",alpha = 0.2) +
    #geom_contour(aes(z = Data), breaks = 90, colour = "black",alpha = 0.2) +
    #geom_contour(aes(z = Data), breaks = 100, colour = "black",alpha = 0.2) +
    
    labs(y = "Profundidad (m)", x = "Mes", fill = "pH") +
    coord_cartesian(expand = 0) +
    scale_x_datetime(date_breaks= "1 month", date_label = "%m/%y") +
    theme(axis.text = element_text(face="bold", size=10),
          axis.title = element_text(face="bold", size=16),
          legend.text = element_text(face="bold", size=16),
          legend.title = element_text(face="bold", size=16, angle = 90)) +
    theme(legend.key.size = unit(1.5, "cm")) + # Tama??o leyenda
    
    guides(colour = guide_legend(title.position = "left"))



{
    dev.off()
   }
    
