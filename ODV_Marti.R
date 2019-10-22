

# Load libraries
library(tidyverse)
library(lubridate)
library(reshape2)
library(MBA)
library(mgcv)
library(chron)
library(RPMG)
########################################################################### 
########################################################################### 
############################### Datos Calpe ################################
########################################################################### 
########################################################################### 

ctd <- read.csv("Multiparametrica Calpe/Datos/Datos_def_cada_metro_corrected_0000.csv", sep = ";", dec = ".")

ctd <- mutate(ctd, Depth  = -Depth) #Correct for plotting


#En el tercer dato, a??ade despues del ctd$ la columna que quieras representar.
ctd2 <- data.frame(ctd$Date, ctd$Depth, ctd$Temp)
colnames(ctd2) <- c("Date", "Depth", "Data")
ctd2$Date<-as.POSIXct(ctd2$Date, format = "%Y-%m-%d",tz = "GMT")
#ctd2 <- subset(ctd2, ctd2$Date>="2017-04-25" & ctd2$Date<="2018-10-16") #Seleccionar rango de fechas

# No hace falta que cambies el nombre de las columnas, no afecta en nada.
colnames(ctd2) <- c("Date", "Depth", "Data")

# Manually extracted hexidecimal ODV colour palette
#ODV_colours <- c("#27ab19", "#0db5e6", "#7139fe", "#d16cfa")
ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")



datosrt <- as.character(ctd2$Date)
ctd2$Date <- as.POSIXct(datosrt, format = "%Y-%m-%d",tz = "GMT")

ctd2$Date <- decimal_date(ctd2$Date)



#originday <-as.POSIXct("1970-1-1", format = "%Y-%m-%d",tz = "GMT")
#ctd2$Date <- as.numeric(julian(ctd2$Date, origin = originday))


########################################################################### 
########################################################################### 
########################################################################### 
########################################################################### 

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

#jpng(paste("Multiparametrica Calpe/", "Conf_sal", sep = ""), P=c(18,9.8) ) 

quartz()
ggplot(data = ctd_mba, aes(x = Date, y = Depth)) +
    geom_raster(aes(fill = Data)) +
    scale_fill_gradientn(colours = rev(ODV_colours), labels = c("20", "25", "30", "35", "40")) +
    #scale_colour_gradient(limits = c(37, 38.2), 
    #                     breaks = c(37, 37.25, 37.50, 37.75, 38, 38.2),
    #                    labels = c(37, 37.25, 37.50, 37.75, 38, 38.2)) +
    geom_contour(aes(z = Data), binwidth = 1, colour = "black", alpha = 0.2) +
    #geom_contour(aes(z = Data), breaks = 15, colour = "black") +
    #geom_contour(aes(z = Data), breaks = 20, colour = "black") +
    #geom_contour(aes(z = Data), breaks = 25, colour = "black") +
    #geom_contour(aes(z = Data), breaks = 30, colour = "black") +
    labs(y = "Profundidad (m)", x = "Mes", fill = "Temperatura (??C)") +
    coord_cartesian(expand = 0) +
    #scale_y_continuous(breaks = c(0.5,5,10,15,20,25,29.5), labels = c(0,5,10,15,20,25,30))
    scale_x_datetime(date_breaks= "1 month", date_label = "%m/%y") +
    theme(axis.text = element_text(face="bold", size=6),
          axis.title = element_text(face="bold", size=16),
          legend.text = element_text(face="bold", size=16),
          legend.title = element_text(face="bold", size=16, angle = 90)) +
    theme(legend.key.size = unit(1.5, "cm")) + # Tama??o leyenda

    guides(colour = guide_legend(title.position = "left"))




#{
#    dev.off()
 #   }


