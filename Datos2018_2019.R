#Tnsert Ingreso 2018
ingreso2018 <- read_excel("~/Desktop/Excels/INGRESO 2018-ANON.xlsx")
names(ingreso2018)

#Renombrar Columnas
names(ingreso2018) <- c("pos", "Status", "Name", "Sex", "Career", "Year", "Entry", "Mat", "RMat", "Fis", "RFis", "PromCI", "GrantType", "GrantAsked", "GetsGrant", "PromSec", "GottenGrant", "School")
names(ingreso2018)

#Reordenar Columnas, eliminar pos
ingreso2018 <- ingreso2018[, c(1,6,3,4,2,5,7,8,9,10,11,12,13,14,15,17,18,16)]
ingreso2018 <- ingreso2018[, -1]

ingreso2018[,"Year"] <- 2018

#Emparejar datos
names(ingreso2018)
unique(ingreso2018[,1])
unique(ingreso2018[,"Mat"])

#Organizar Notas
if(!is.element("plyr", installed.packages()[,1])){
  install.packages("plyr")
}
library("plyr")

standardize <- function(str, tokenArray, fixedArray){
  returnStr <- str
  for(i in 1:length(tokenArray)){
    if(grepl(tokenArray[i], str, ignore.case = TRUE)){
      returnStr = fixedArray[i]
      break
    }
  }
  returnStr
}


noInforma <- c("-", "Es pase", "Es pase interno", "x")
ingreso2018$Mat <- mapvalues(ingreso2018$Mat, noInforma, rep(NA, length(noInforma)))
ingreso2018$Fis <- mapvalues(ingreso2018$Fis, noInforma, rep(NA, length(noInforma)))

noAprueba <- c("A", "Desaprobado", "AUS")
ingreso2018$Mat <- mapvalues(ingreso2018$Mat, noAprueba, rep(1, length(noAprueba)))
ingreso2018$Fis <- mapvalues(ingreso2018$Fis, noAprueba, rep(1, length(noAprueba)))
ingreso2018$RMat <- mapvalues(ingreso2018$RMat, noAprueba, rep(1, length(noAprueba)))
ingreso2018$RFis <- mapvalues(ingreso2018$RFis, noAprueba, rep(1, length(noAprueba)))

ingreso2018$Mat <- as.numeric(ingreso2018$Mat)
ingreso2018$RMat <- as.numeric(ingreso2018$RMat)
ingreso2018$Fis <- as.numeric(ingreso2018$Fis)
ingreso2018$RFis <- as.numeric(ingreso2018$RFis)

ingreso2018$PromCI = round(rowMeans(ingreso2018[ , 7:10], na.rm = TRUE), digit = 2)
ingreso2018[which(ingreso2018[, "PromCI"] == "NaN"),"PromCI"] <- NA
unique(ingreso2018[,"PromCI"])

# Grant
unique(ingreso2018[,"GrantAsked"])

sesenta <- c("40/20", "40/ 20", "30/30")
ingreso2018$GrantAsked <- mapvalues(ingreso2018$GrantAsked, sesenta, rep(60, length(sesenta)))
ingreso2018[which(ingreso2018[, "GrantAsked"] == "20/20"),"GrantAsked"] <- 40

ingreso2018$GrantAsked <- as.numeric(ingreso2018$GrantAsked)
unique(ingreso2018[,"GottenGrant"])

unique(ingreso2018[, "GetsGrant"])

falso <- c("NO", "MO")
ingreso2018$GetsGrant <- mapvalues(ingreso2018$GetsGrant, falso, rep(FALSE, length(falso)))
ingreso2018[which(ingreso2018[, "GetsGrant"] == "SI"),"GetsGrant"] <- "TRUE"
ingreso2018[which(ingreso2018[, "GetsGrant"] == "falta doc y CI"), "GetsGrant"] <- NA

ingreso2018$GetsGrant <- as.logical(ingreso2018$GetsGrant)

grantt <- table(ingreso2018[which(ingreso2018[,"GetsGrant"] == TRUE), "GottenGrant"])
ingreso2018[which(ingreso2018[,"GetsGrant"] == TRUE), "GottenGrant"] <- ingreso2018[which(ingreso2018[,"GetsGrant"] == TRUE), "GrantAsked"]

#Arreglo Status
unique(ingreso2018[, "Status"])

baja <- c("BAJA SE PASA A FCE", "BAJA DESAPROBO RECUPERATORIO", "bAJA DESAPROBO RECUPERATORIO", "Baja 2017", "BAJA PACT", "BAJA NO VINO A RECUPERAR")
ingreso2018$Status <- mapvalues(ingreso2018$Status, baja, rep("BAJA", length(baja)))
ingreso2018[which(ingreso2018[, "Status"] != "BAJA"), "Status"] <- "MATRICULADO"


save(file = "ingreso2018.rdata", ingreso2018)

#__________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________-

#Tnsert Ingreso 2019
ingreso2019 <-read_excel("~/Desktop/Excels/INGRESO 2019-ANON.xlsx")

names(ingreso2019)

#Renombrar Columnas
names(ingreso2019) <- c("pos", "Status", "Name", "Sex", "Career", "Year", "Entry", "Mat", "RMat", "Fis", "RFis", "PromCI", "GrantType", "GrantAsked", "GetsGrant", "PromSec", "GottenGrant","State","IngStatus", "School")
names(ingreso2019)
#Reordenar Columnas
ingreso2019 <- ingreso2019[, c(1,6,3,4,2,18,19,5,7,8,9,10,11,12,13,14,15,17,20,16)]
ingreso2019 <- ingreso2019[, -1]

ingreso2019[,"Year"] <- 2019

#Unificamos el Estado
unique(ingreso2019[,"State"])
unique(ingreso2019[,"Status"])
unique(ingreso2019[,"IngStatus"])

unique(ingreso2019[which(is.na(ingreso2019[,"State"])),"Status"])

ingreso2019[which(ingreso2019[, "State"] == "A - MATRICULADO"),"State"] <- "MATRICULADO"
ingreso2019[which(ingreso2019[, "State"] == "A - ASISTIÓ AL CURSO"),"State"] <- ingreso2019[which(ingreso2019[,"State"] == "A - ASISTIÓ AL CURSO"), "IngStatus"]
names(ingreso2019)

ingreso2019 <- ingreso2019[, -6]
ingreso2019 <- ingreso2019[, -4]

names(ingreso2019) <- c("Year", "Name", "Sex", "Status", "Career", "Entry", "Mat", "RMat", "Fis", "RFis", "PromCI", "GrantType", "GrantAsked", "GetsGrant", "GottenGrant", "School", "PromSec")

#Todo ok
unique(ingreso2019[,"Career"])

#Notas a numeric
unique(ingreso2019[, "Mat"])
ingreso2019[which(ingreso2019[, "Mat"] == "AUS"),"Mat"] <- 1
unique(ingreso2019[, "RMat"])

RMatError <- c("AUS", "ausente", "APROBADO", "-")
RMatFixed <- c(1,1,4,NA)
ingreso2019$RMat <- sapply(ingreso2019$RMat, standardize, RMatError, RMatFixed)

unique(ingreso2019[, "Fis"])
ingreso2019[which(ingreso2019[, "Fis"] == "AUS"),"Fis"] <- 1

unique(ingreso2019[, "RFis"])
ingreso2019[which(ingreso2019[, "RFis"] == "AUS"),"RFis"] <- 1

ingreso2019$Mat <- as.numeric(ingreso2019$Mat)
ingreso2019$RMat <- as.numeric(ingreso2019$RMat)
ingreso2019$Fis <- as.numeric(ingreso2019$Fis)
ingreso2019$RFis <- as.numeric(ingreso2019$RFis)

#Promedio CI
ingreso2019$PromCI = round(rowMeans(ingreso2019[ , 7:10], na.rm = TRUE), digit = 2)
ingreso2019[which(ingreso2019[, "PromCI"] == "NaN"),"PromCI"] <- NA
unique(ingreso2019[,"PromCI"])

#Promedio Sec
prom <- table(ingreso2019[, "PromSec"])

ingreso2019[which(ingreso2019[, "PromSec"] == 50),"PromSec"] <- NA
ingreso2019[which(ingreso2019[, "PromSec"] == "7,51"),"PromSec"] <- 7.51
ingreso2019[which(ingreso2019[, "PromSec"] == "9,08"),"PromSec"] <- 9.08

ingreso2019$PromSec <- as.numeric(ingreso2019$PromSec)

#Becas
unique(ingreso2019[,"GrantAsked"])
grants <- table(ingreso2019[,"GrantAsked"])

setenta <- c("40/30", "30/40", "50/20")
ingreso2019$GrantAsked <- mapvalues(ingreso2019$GrantAsked, setenta, rep(70, length(setenta)))
cien <- c("40/? (100 % total)", "40+60", "50/50", "60/40")
ingreso2019$GrantAsked <- mapvalues(ingreso2019$GrantAsked, cien, rep(100, length(cien)))
cuarenta <- c("40/?", "40+?")
ingreso2019$GrantAsked <- mapvalues(ingreso2019$GrantAsked, cuarenta, rep(40, length(cuarenta)))
sesenta <- c("40/20 (TOTAL 100/60%)", "40/20")
ingreso2019$GrantAsked <- mapvalues(ingreso2019$GrantAsked, sesenta, rep(60, length(sesenta)))
ochenta <- c("80 TOTAL", "60/20")
ingreso2019$GrantAsked <- mapvalues(ingreso2019$GrantAsked, ochenta, rep(80, length(ochenta)))

ingreso2019$GrantAsked[!grepl("100", ingreso2019$GrantAsked)] <- substr(ingreso2019$GrantAsked[!grepl("100", ingreso2019$GrantAsked)], 1, 2)

ingreso2019$GrantAsked <- as.numeric(ingreso2019$GrantAsked)

unique(ingreso2019[,"GetsGrant"])

gets <- c("OBTIENE", "Si", "NO OBTIENE pero se le da x ayuda")
ingreso2019$GetsGrant <- mapvalues(ingreso2019$GetsGrant, gets, rep(TRUE, length(gets)))
notGets <- c("NO OBTIENE", "NO", "No", "N/A", "BFI N/A", "BAJA", "?")
ingreso2019$GetsGrant <- mapvalues(ingreso2019$GetsGrant, notGets, rep(FALSE, length(notGets)))

ingreso2019$GetsGrant <- as.logical(ingreso2019$GetsGrant)

granted <- table(ingreso2019[,"GottenGrant"])

library(stringr)

GottenGrantError <- c("70% (30 BMA + 30 PU + 10 BAU)","100 (60 BEA + 40 BAU)","80 (50 bct + 30 bau)","100% (60% BEA+ 15% BAU + 25% PU)")
GottenGrantFixed <- c(70,100,80,100)
ingreso2019$GottenGrant <- sapply(ingreso2019$GottenGrant, standardize, GottenGrantError, GottenGrantFixed)

ingreso2019$GottenGrant[!grepl("100", ingreso2019$GottenGrant)] <- substr(ingreso2019$GottenGrant[!grepl("100", ingreso2019$GottenGrant)], 1, 2)

grantt19 <- data.frame(ingreso2019[which(ingreso2019[,"GetsGrant"] == TRUE), "GottenGrant"], ingreso2019[which(ingreso2019[,"GetsGrant"] == TRUE), "GrantAsked"])
getgrant <- which(ingreso2019[,"GetsGrant"] == TRUE & is.na(ingreso2019[,"GottenGrant"]))
ingreso2019[ getgrant, "GottenGrant"] <- ingreso2019[getgrant, "GrantAsked"]

save(file = "ingreso2019.rdata", ingreso2019)

#------------------------------------------------------------------------------------------------------------------------------------------------------

ingreso <- merge.data.frame(ingreso2018,ingreso2019, all = TRUE)

names(ingreso)

ingreso <- ingreso[, -12]
ingreso <- ingreso[, -12]
ingreso <- ingreso[, -12]

names(ingreso) <- c("Year", "Name", "Sex", "Status", "Career", "Entry", "Mat", "RMat", "Fis", "Rfis", "PromCI", "Grant", "School", "PromSec")

ingreso <- ingreso[,-14]

schools <- table(ingreso[,"School"])

#standardize School names with function
schoolError <- c("d'elía","bede","brick towers","newman","pironio","colegio del pilar","Del Viso Day School","Jesus Maria Sant Gervasi","lincoln","los molinos","los robles","michael ham","northfield","northlands","verbo divino","Nuestra Señora","Oakhill","pilgrims","san felipe","san jos","santa in","Moorlands","st. george","st. john","St. Mary of the Hills","St. Matthew","sworn","wellspring", "Alemán (Santa Cruz -Bolivia)","Alemán Mariscal Braun (La Paz - Bolivia)", "Antonio Berni (Benavidez)", "Angel D'Elia")
schoolFixed <- c("Ángel D'Elía","Bede's Grammar School","Brick Towers","Cardenal Newman","Cardenal Pironio","Colegio del Pilar","Del Viso Day School","Jesus Maria Sant Gervasi","Lincoln","Los Molinos","Los Robles","Michael Ham","Northfield School","Northlands","Verbo Divino","Nuestra Señora de Lujan","Oakhill","Pilgrims","San Felipe","San Jose","Santa Ines","Moorlands","St. George's College","St. John's","St. Mary of the Hills","St. Matthew's North","Sworn","Wellspring", "Aleman", "Alemán Mariscal Braun","Antonio Berni" , "Ángel D'Elía")
ingreso$School <- sapply(ingreso$School, standardize, schoolError, schoolFixed)

save(file = "ingreso.rdata", ingreso)


unique(ingreso[, "Entry"])

directo <- c("Ingreso Directo", "Pase Universitario", "Ingreso Directo (i feb)", "Ingreso Directo (i sep)", "Ingreso Directo (i oct)", "Ingreso Directo (i cuatri)")
ingreso$Entry <- mapvalues(ingreso$Entry, directo, rep("Directo", length(directo)))

entryError <- c("Ingreso Febrero", "Cuatrimestral Part Time", "Ingreso Septiembre", "Ingreso Octubre Pilar", "Ingreso Octubre", "Ingreso Libre Diciembre", "Cuatrimestral Full Time", "Ingreso Agosto","Ingreso Febrero MAT", "Ingreso Libre Febrero", "Curso Febrero", "Curso Octubre Nordelta", "Curso Octubre Pilar", "Curso Febrero MAT", "Curso Septiembre", "Febrero Libre", "Libre Diciembre", "Curso Septiembre (Libre)")
entryFixed <- c("Febrero", "Marzo", "Septiembre", "Octubre", "Octubre", "Diciembre", "Marzo", "Agosto", "Febrero", "Febrero", "Febrero", "Octubre", "Octubre", "Febrero", "Septiembre", "Febrero", "Diciembre", "Septiembre")
ingreso$Entry <- sapply(ingreso$Entry, standardize, entryError, entryFixed)

unique(ingreso[,"Year"])

mean <- mean(ingreso$PromCI, na.rm = TRUE)
sd <- sd(ingreso$PromCI, na.rm = TRUE)

Analisis_IND <- read_excel("~/Desktop/Excels/Analisis IND.xlsx")
Analisis_INF <- read_excel("~/Desktop/Excels/Analisis INF.xlsx")

AnalisisInf <- Analisis_INF[which(Analisis_INF[,"Parcial..1"] != 0),]
AnalisisInd <- Analisis_IND[which(Analisis_IND[,"Parcial..1"] != 0),]

rm(Analisis_IND)
rm(Analisis_INF)

names(AnalisisInf) <- c("pos", "Nombre", "Nota", "Avg", "Date", "Prom")
AnalisisInf <- AnalisisInf[,-c(1,4,5)]

names(AnalisisInd) <- c("pos", "Nombre", "Nota", "Avg", "Date", "Prom")
AnalisisInd <- AnalisisInd[,-c(1,4,5)]


save(file = "analisisInd.rdata", AnalisisInd)
save(file = "analisisInf.rdata", AnalisisInf)








