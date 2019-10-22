

library(data.table)
library(RPMG)




# rbind data from a range of depths

files <- list.files("Multiparametrica Calpe/Datos/Emo/", full.names = TRUE)
filesnames <- list.files("Multiparametrica Calpe/Datos/Emo/", full.names = FALSE)


files <- list.files("Multiparametrica Calpe/Datos/Conmo/", full.names = TRUE)
filesnames <- list.files("Multiparametrica Calpe/Datos/Conmo/", full.names = FALSE)


depth <- seq(from = 0.5, to =30, by = 0.2)

range <- 0.2


alldata <- data.frame(Date = character(), Depth = numeric(), Temp = numeric(), Sal = numeric(), Chla = numeric(), Turb = numeric(), pH = numeric(), DO = numeric())

datadate <- data.frame(Date = character(), Depth = numeric(), Temp = numeric(), Sal = numeric(), Chla = numeric(), Turb = numeric(), pH = numeric(), DO = numeric())




for (i in 1:length(files)){
    
    
    data <- fread(files[i], 
                  sep = ",", 
                  dec = ".",
                  skip = 64,
                  data.table = FALSE,
                  colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
    
    colnames(data) <- c("Date", "Depth", "Temp", "Sal", "Cond", "EC25", "Density", "SigmaT", "Chl-flu", "Chla", "Turb", "pH", "DO (%)", "DO")
    data[,15] <- NULL; data$Cond <- NULL; data$EC25 <- NULL; data$Density <- NULL;
    data$SigmaT <- NULL; data$`Chl-flu` <- NULL; data$`DO (%)` <- NULL
    
    datost <- data[1,1]
    datost2 <- strsplit(x = datost, split = " ")
    
    
    for (i in 1:length(depth)){
        
        depthselec <- c(depth[i]-range, depth[i]+range)
        data1 <- data$Depth > depthselec[1] & data$Depth < depthselec[2]
        data2 <- data[data1,]
        data3 <- c(datost2[[1]][1], colMeans(data2[,-1], na.rm = TRUE))
        data3[2] <- depth[i]
        data4 <- rbind.data.frame(data3)
        colnames(data4) <- c("Date", "Depth", "Temp", "Sal", "Chla", "Turb", "pH", "DO")
        
        datadate <- merge.data.frame(x = datadate, y = data4, all.x = TRUE, all.y = TRUE)
        
    }
    
    #alldata <- rbind(alldata, datadate)
    alldata <- merge.data.frame(x = alldata, y = datadate, all.x = TRUE, all.y = TRUE)
    
}

for (i in 2:length(alldata)) {
    alldata[,i] <- as.numeric(as.character(alldata[,i]))
    
}



datosrt <- as.character(alldata$Date)

alldata$Date <- as.POSIXct(datosrt, format = "%d/%m/%Y",tz = "GMT")



finaldata <- alldata[with(alldata, order(Date)),]



write.table(x = finaldata, file = local.file(paste("Multiparametrica Calpe/Datos/", "Datos_def_cada_0.2metro_corrected", sep = ""),"csv"), sep=";", row.names = FALSE)











# Extrac the data for Anomia
aa <- which(finaldata$Depth == 5 | finaldata$Depth == 15 | finaldata$Depth == 20)

finaldata <- finaldata[aa,]

finaldata2 <- finaldata
aa <- which(finaldata$Date > labels[1] & finaldata$Date < labels[9])
write.table(x = finaldata2, file = local.file(paste("Datos/Perfiles_calpe_multipa/Extraido/", "Oce_anomia", sep = ""),"txt"), sep=";", row.names = FALSE)




#Only temp
alldata$Sal <- NULL; alldata$Chla <- NULL; alldata$Turb <- NULL; alldata$pH <- NULL;
alldata$DO <- NULL;
finaldata$Sal <- NULL; finaldata$Chla <- NULL; finaldata$Turb <- NULL; finaldata$pH <- NULL; 
finaldata$DO <- NULL;

# Para poner en columnas los valores a cada depth

for (i in 3:length(alldata)) {
    
    assign(paste(colnames(alldata[i])), data.frame(alldata[,1], alldata[,2], alldata[,i]))
}

finaldata <- as.data.frame(matrix(nrow = length(alldata$Date)/length(depth), ncol = 1))



for (z in 3:length(alldata)) {
    
    
    for (i in 1:length(depth)){
        
        dat <- eval(parse(text = colnames(alldata[z])))
        data1 <- dat[,2] >= depth[i] & dat[,2] < depth[i]+range
        data2 <- dat[data1,]
        datfinal <- data.frame(data2[,1], data2[,3])
        finaldata[paste(colnames(alldata[z]), depth[i], sep = "")] <- datfinal[,2]
        
    }
    
}

finaldata[,1] <- datfinal[,1]

datosrt <- as.character(finaldata$V1)

#finaldata$V1 <- as.POSIXct(datosrt, format = "%d-%m-%Y",tz = "GMT")

colnames(finaldata)[1] <- "Date"


finaldata <- finaldata[with(finaldata, order(Date)),]

write.table(x = finaldata, file = local.file(paste("Multiparametrica Calpe/", "Datos_rafa_Temp_10_15_20", sep = ""),"csv"), sep=";", row.names = FALSE)
















# Extract de data from a range of depths


files <- list.files("Multiparametrica Calpe/Datos/Emo/", full.names = TRUE)
filesnames <- list.files("Multiparametrica Calpe/Datos/Emo/", full.names = FALSE)
depth <- c(1, 5, 9, 14)


files <- list.files("Multiparametrica Calpe/Datos/Conmo/", full.names = TRUE)
filesnames <- list.files("Multiparametrica Calpe/Datos/Conmo/", full.names = FALSE)
depth <- c(5, 10, 15, 20,25,29.5)

range <- 1


depth <- seq(from = 0.5, to = 29.5, by = 0.5)
depth <- seq(from = 0.5, to = 14.5, by = 0.5)

depth <- seq(from = 1, to = 15, by = 1)

range <- 0.5


alldata <- data.frame(Date = character(), Depth = numeric(), Temp = numeric(), Sal = numeric(), Chla = numeric(), Turb = numeric(), pH = numeric(), DO = numeric())

datadate <- data.frame(Date = character(), Depth = numeric(), Temp = numeric(), Sal = numeric(), Chla = numeric(), Turb = numeric(), pH = numeric(), DO = numeric())



for (i in 1:length(files)){
    
    
    data <- fread(files[i], 
                  sep = ",", 
                  dec = ".",
                  skip = 64,
                  data.table = FALSE,
                  colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
    
    colnames(data) <- c("Date", "Depth", "Temp", "Sal", "Cond", "EC25", "Density", "SigmaT", "Chl-flu", "Chla", "Turb", "pH", "DO (%)", "DO")
    data[,15] <- NULL; data$Cond <- NULL; data$EC25 <- NULL; data$Density <- NULL;
    data$SigmaT <- NULL; data$`Chl-flu` <- NULL; data$`DO (%)` <- NULL
    
    datost <- data[1,1]
    datost2 <- strsplit(x = datost, split = " ")
    

    for (i in 1:length(depth)){
        
        depthselec <- c(depth[i], depth[i]+range)
        data1 <- data$Depth > depthselec[1] & data$Depth < depthselec[2]
        data2 <- data[data1,]
        data3 <- c(datost2[[1]][1], colMeans(data2[,-1], na.rm = TRUE))
        data3[2] <- depth[i]
        data4 <- rbind.data.frame(data3)
        colnames(data4) <- c("Date", "Depth", "Temp", "Sal", "Chla", "Turb", "pH", "DO")
        
        datadate <- merge.data.frame(x = datadate, y = data4, all.x = TRUE, all.y = TRUE)
        
    }

    alldata <- merge.data.frame(x = alldata, y = datadate, all.x = TRUE, all.y = TRUE)
    
}


for (i in 2:length(alldata)) {
    alldata[,i] <- as.numeric(as.character(alldata[,i]))
    
}


for (i in 3:length(alldata)) {
    
    assign(paste(colnames(alldata[i])), data.frame(alldata[,1], alldata[,2], alldata[,i]))
}

alldata$Date <- as.POSIXct(alldata$Date, format = "%d/%m/%Y",tz = "GMT")

alldata2 <- alldata[with(alldata, order(Date)),]

namefile <- paste("multiparm_prueba2",sep = "_")
write.table(x = alldata2, file = local.file(paste("Multiparametrica Calpe/Datos/", namefile, sep = ""),"csv"), sep=";", row.names = FALSE)






#Poner cada profundidad en columna

finaldata <- as.data.frame(matrix(nrow = length(alldata$Date)/length(depth), ncol = 1))

for (z in 3:length(alldata)) {
    
    
    for (i in 1:length(depth)){
        
        dat <- eval(parse(text = colnames(alldata[z])))
        data1 <- dat[,2] >= depth[i] & dat[,2] < depth[i]+range
        data2 <- dat[data1,]
        datfinal <- data.frame(data2[,1], data2[,3])
        finaldata[paste(colnames(alldata[z]), depth[i], sep = "")] <- datfinal[,2]
        
    }
    
}
##

finaldata[,1] <- datfinal[,1]

datosrt <- as.character(finaldata$V1)

finaldata$V1 <- as.POSIXct(datosrt, format = "%d/%m/%Y",tz = "GMT")

colnames(finaldata)[1] <- "Date"


finaldata <- finaldata[with(finaldata, order(Date)),]


##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################

####
#### Extraer datos de unos dias (archivos) concretos.
####
data[,15] <- NULL; data$Cond <- NULL; data$EC25 <- NULL; data$Density <- NULL;
data$SigmaT <- NULL; data$`Chl-flu` <- NULL; data$`DO (%)` <- NULL; data$Sal <- NULL;
data$Turb <- NULL; data$pH <- NULL; data$Temp <- NULL



files <- list.files("Multiparametrica Calpe/Datos/Emo/", full.names = TRUE)
filesnames <- list.files("Multiparametrica Calpe/Datos/Emo/", full.names = FALSE)


files <- list.files("Multiparametrica Calpe/Datos/Conmo/", full.names = TRUE)
filesnames <- list.files("Multiparametrica Calpe/Datos/Conmo/", full.names = FALSE)


alldata <- data.frame(Date = character(), Depth = numeric(), Temp = numeric(), Sal = numeric(), Chla = numeric(), Turb = numeric(), pH = numeric(), DO = numeric())

datadate <- data.frame(Date = character(), Depth = numeric(), Temp = numeric(), Sal = numeric(), Chla = numeric(), Turb = numeric(), pH = numeric(), DO = numeric())



whatdatatemp <- c(1, 11, 18, 26)

for (i in 1:length(whatdatatemp)){
    
    
    
    data <- fread(files[whatdatatemp][i], 
                  sep = ",", 
                  dec = ".",
                  skip = 64,
                  data.table = FALSE,
                  stringsAsFactors = FALSE,
                  colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
    
    colnames(data) <- c("Date", "Depth", "Temp", "Sal", "Cond", "EC25", "Density", "SigmaT", "Chlflu", "Chla", "Turb", "pH", "DO(%)", "DO")
    data[,15] <- NULL 
    
    
    #data$Cond <- NULL 
    data$EC25 <- NULL
    #data$Density <- NULL
    data$SigmaT <- NULL 
    data$Chlflu <- NULL
    #data$Chla <- NULL
    data$`DO(%)` <- NULL
    #data$Sal <- NULL
    #data$Turb <- NULL
    #data$pH <- NULL
    #data$Temp <- NULL
    
    
    
    datost2 <- strsplit(x = data$Date[1], split = " ")
    datedata <- datost2[[1]][1]
    datedata <-gsub("/", "_", datedata)
    namefile <- paste("multiparm_CONMO_alldata", datedata,sep = "_")
    
    write.table(x = data, file = local.file(paste("Multiparametrica Calpe/Datos/", namefile, sep = ""),"csv"), sep=";", row.names = FALSE)
    
}


### Extraer temperatura
library(data.table)
library(RPMG)


files <- list.files("Multiparametrica Calpe/Datos/Emo/", full.names = TRUE)
filesnames <- list.files("Multiparametrica Calpe/Datos/Emo/", full.names = FALSE)


files <- list.files("Multiparametrica Calpe/Datos/Conmo/", full.names = TRUE)
filesnames <- list.files("Multiparametrica Calpe/Datos/Conmo/", full.names = FALSE)


alldata <- data.frame(Date = character(), Depth = numeric(), Temp = numeric(), Sal = numeric(), Chla = numeric(), Turb = numeric(), pH = numeric(), DO = numeric())

datadate <- data.frame(Date = character(), Depth = numeric(), Temp = numeric(), Sal = numeric(), Chla = numeric(), Turb = numeric(), pH = numeric(), DO = numeric())



whatdatatemp <- c(2, 26, 11, 4, 16)

for (i in 1:length(whatdatatemp)){
    
    
    
    data <- fread(files[whatdatatemp][i], 
                  sep = ",", 
                  dec = ".",
                  skip = 64,
                  data.table = FALSE,
                  colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
    
    colnames(data) <- c("Date", "Depth", "Temp", "Sal", "Cond", "EC25", "Density", "SigmaT", "Chl-flu", "Chla", "Turb", "pH", "DO (%)", "DO")
    data[,15] <- NULL; data$Cond <- NULL; data$EC25 <- NULL; data$Density <- NULL;
    data$SigmaT <- NULL; data$`Chl-flu` <- NULL; data$`DO (%)` <- NULL; data$Sal <- NULL;
    data$Chla <- NULL; data$Turb <- NULL; data$pH <- NULL; data$DO <- NULL
    
    
    datost2 <- strsplit(x = data$Date[1], split = " ")
    datedata <- datost2[[1]][1]
    datedata <-gsub("/", "_", datedata)
    namefile <- paste("multiparm_Temp", datedata,sep = "_")
    
    #write.table(x = data, file = local.file(paste("Datos/Perfiles_calpe_multipa/Extraido/", namefile, sep = ""),"csv"), sep=";", row.names = FALSE)

}




### Extraer clorofila



files <- list.files("Multiparametrica Calpe/Datos/Emo/", full.names = TRUE)
filesnames <- list.files("Multiparametrica Calpe/Datos/Emo/", full.names = FALSE)


files <- list.files("Multiparametrica Calpe/Datos/Conmo/", full.names = TRUE)
filesnames <- list.files("Multiparametrica Calpe/Datos/Conmo/", full.names = FALSE)


alldata <- data.frame(Date = character(), Depth = numeric(), Temp = numeric(), Sal = numeric(), Chla = numeric(), Turb = numeric(), pH = numeric(), DO = numeric())

datadate <- data.frame(Date = character(), Depth = numeric(), Temp = numeric(), Sal = numeric(), Chla = numeric(), Turb = numeric(), pH = numeric(), DO = numeric())



whatdatachl <- c(11, 1)

for (i in 1:length(whatdatachl)){
    
    
    
    data <- fread(files[whatdatachl][i], 
                  sep = ",", 
                  dec = ".",
                  skip = 64,
                  data.table = FALSE,
                  colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
    
    colnames(data) <- c("Date", "Depth", "Temp", "Sal", "Cond", "EC25", "Density", "SigmaT", "Chl-flu", "Chla", "Turb", "pH", "DO (%)", "DO")
    data[,15] <- NULL; data$Cond <- NULL; data$EC25 <- NULL; data$Density <- NULL;
    data$SigmaT <- NULL; data$`Chl-flu` <- NULL; data$`DO (%)` <- NULL; data$Sal <- NULL;
    data$Chla <- NULL; data$Turb <- NULL; data$pH <- NULL; data$DO <- NULL
    
    
    datost2 <- strsplit(x = data$Date[1], split = " ")
    datedata <- datost2[[1]][1]
    datedata <-gsub("/", "_", datedata)
    namefile <- paste("multiparm_Chl", datedata,sep = "_")
    
    write.table(x = data, file = local.file(paste("Datos/Perfiles_calpe_multipa/Extraido/", namefile, sep = ""),"csv"), sep=";", row.names = FALSE)
    
}





### Extraer oxigeno



files <- list.files("Multiparametrica Calpe/Datos/Emo/", full.names = TRUE)
filesnames <- list.files("Multiparametrica Calpe/Datos/Emo/", full.names = FALSE)


files <- list.files("Multiparametrica Calpe/Datos/Conmo/", full.names = TRUE)
filesnames <- list.files("Multiparametrica Calpe/Datos/Conmo/", full.names = FALSE)


alldata <- data.frame(Date = character(), Depth = numeric(), Temp = numeric(), Sal = numeric(), Chla = numeric(), Turb = numeric(), pH = numeric(), DO = numeric())

datadate <- data.frame(Date = character(), Depth = numeric(), Temp = numeric(), Sal = numeric(), Chla = numeric(), Turb = numeric(), pH = numeric(), DO = numeric())



whatdataO2 <- c(1, 11, 18, 26)

for (i in 1:length(whatdataO2)){
    
    
    
    data <- fread(files[whatdataO2][i], 
                  sep = ",", 
                  dec = ".",
                  skip = 64,
                  data.table = FALSE,
                  colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
    
    colnames(data) <- c("Date", "Depth", "Temp", "Sal", "Cond", "EC25", "Density", "SigmaT", "Chl-flu", "Chla", "Turb", "pH", "DO (%)", "DO")
    data[,15] <- NULL; data$Cond <- NULL; data$EC25 <- NULL; data$Density <- NULL;
    data$SigmaT <- NULL; data$`Chl-flu` <- NULL; data$`DO (%)` <- NULL; data$Sal <- NULL;
    data$Chla <- NULL; data$Turb <- NULL; data$pH <- NULL; data$DO <- NULL
    
    
    datost2 <- strsplit(x = data$Date[1], split = " ")
    datedata <- datost2[[1]][1]
    datedata <-gsub("/", "_", datedata)
    namefile <- paste("multiparm_O2", datedata,sep = "_")
    
    write.table(x = data, file = local.file(paste("Multiparametrica Calpe/Datos/", namefile, sep = ""),"csv"), sep=";", row.names = FALSE)
    
}



##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################

#Extract the depth of the thermocline



tempdata <- finaldata[1:60]





individuals <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)


validpinnaOne <- function(){
    cat("###########################################\n")
    cat("Choose what variables, separate the numbers by commas without spaces\n")
#    if(datagroup == 1) {
#        numbord <- allindividuals
 #       print(paste(allindividuals, individualsname, sep = "-"))
  #  } else {
   #     numbord <- allsensors
    #    print(paste(allsensors, sensorsname, sep = "."))
    #}
    whatpinnas = readline(prompt = "Variable:")
    #whatpinnas <- as.numeric(unlist(strsplit(x = whatpinnas,split = " ")))
} 
repeat{
    Pinna <- validpinnaOne()
    
    Pinnaselec = as.numeric(unlist(strsplit(split=" ", (Pinna))))
    
   # if(datagroup == 1) {
        numbord <- allindividuals
   # } else {
  #      numbord <- allsensors
  #  }
    
    if(anyNA(Pinnaselec)) {
        cat("###########################################\n")
        print("Error0, try again")
    } else if (length(Pinnaselec) == 0){
        print("Error1, try again") 
    } else if (any(duplicated(Pinnaselec))) {
        print("Error2, try again") 
    } else if (all(Pinnaselec >= min(numbord)) & all(Pinnaselec <= max(numbord)) 
               & length(Pinnaselec) > 0){break
    } else{
        print("That doesn't seems like and individual")
    }
}




##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################






is.leapyear=function(year){
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}









data1_2 <- data$Depth > 1 & data$Depth < 2
data1_2 <- data[data1_2,]
data1_2 <- c(datost2[[1]][1], colMeans(data1_2[,-1], na.rm = TRUE))
#data1_2[data1_2=="NaN"] <- 0
data1_2[2] <- 1.5

data5_6 <- data$Depth > 5 & data$Depth < 6
data5_6 <- data[data5_6,]
data5_6 <- c(datost2[[1]][1], colMeans(data5_6[,-1], na.rm = TRUE))
#data5_6[data5_6=="NaN"] <- 0
data5_6[2] <- 5.5

data9_10 <- data$Depth > 9 & data$Depth < 10
data9_10 <- data[data9_10,]
data9_10 <- c(datost2[[1]][1], colMeans(data9_10[,-1], na.rm = TRUE))
#data9_10[data9_10=="NaN"] <- 0
data9_10[2] <- 9.5

data14_15 <- data$Depth > 14 & data$Depth < 15
data14_15 <- data[data14_15,]
data14_15 <- c(datost2[[1]][1], colMeans(data14_15[,-1], na.rm = TRUE))
#data14_15[data14_15=="NaN"] <- 0
data14_15[2] <- 14.5



data2 <- rbind.data.frame(data1_2, data5_6, data9_10, data14_15)
colnames(data2) <- c("Date", "Depth", "Temp", "Sal", "Chla", "Turb", "pH", "DO")


alldata <- merge.data.frame(x = alldata, y = data2, all.x = TRUE, all.y = TRUE)


