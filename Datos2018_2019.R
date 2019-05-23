#Tnsert Ingreso 2018
ingreso2018 <- read_excel("~/Desktop/Excels/INGRESO 2018-ANON.xlsx")
names(ingreso2018)

#Renombrar Columnas
names(ingreso2018) <- c("pos", "Status", "Name", "Sex", "Career", "Year", "Entry", "Mat", "RMat", "Fis", "RFis", "PromCI", "GrantType", "GrantAsked", "GetsGrant", "PromSec", "GottenGrant", "School")
names(ingreso2018)

#Reordenar Columnas, eliminar pos
ingreso2018 <- ingreso2018[, c(1,6,3,4,2,5,7,8,9,10,11,12,13,14,15,17,18,16)]
ingreso2018 <- ingreso2018[, -1]

#Emparejar datos
names(ingreso2018)
unique(ingreso2018[,1])
unique(ingreso2018[,"Mat"])

#Organizar Notas
if(!is.element("plyr", installed.packages()[,1])){
  install.packages("plyr")
}
library("plyr")
unique(ingreso2018$Mat)
unique(ingreso2018$Fis)
unique(ingreso2018[,"RMat"])
unique(ingreso2018[,"RFis"])


noInforma <- c("-", "Es pase", "Es pase interno", "x")
ingreso2018$Mat <- mapvalues(ingreso2018$Mat, noInforma, rep(NA, length(noInforma)))
ingreso2018$Fis <- mapvalues(ingreso2018$Fis, noInforma, rep(NA, length(noInforma)))

noAprueba <- c("A", "Desaprobado", "AUS")
ingreso2018$Mat <- mapvalues(ingreso2018$Mat, noAprueba, rep(NA, length(noAprueba)))
ingreso2018$Fis <- mapvalues(ingreso2018$Fis, noAprueba, rep(NA, length(noAprueba)))
ingreso2018$RMat <- mapvalues(ingreso2018$RMat, noAprueba, rep(NA, length(noAprueba)))
ingreso2018$RFis <- mapvalues(ingreso2018$RFis, noAprueba, rep(NA, length(noAprueba)))

ingreso2018$Mat <- as.numeric(ingreso2018$Mat)
ingreso2018$RMat <- as.numeric(ingreso2018$RMat)
ingreso2018$Fis <- as.numeric(ingreso2018$Fis)
ingreso2018$RFis <- as.numeric(ingreso2018$RFis)

ingreso2018$PromCI = round(rowMeans(ingreso2018[ , 7:10], na.rm = TRUE), digit = 2)
ingreso2018[which(ingreso2018[, "PromCI"] == "NaN"),"PromCI"] <- NA
unique(ingreso2018[,"PromCI"])

# Grant
unique(ingreso2018[,"GrantAsked"])
ingreso2018[which(ingreso2018[, "GrantAsked"] == "40/ 20"),"GrantAsked"] <- 60
ingreso2018[which(ingreso2018[, "GrantAsked"] == "40/20"),"GrantAsked"] <- 60
ingreso2018[which(ingreso2018[, "GrantAsked"] == "30/30"),"GrantAsked"] <- 60
ingreso2018[which(ingreso2018[, "GrantAsked"] == "20/20"),"GrantAsked"] <- 40

ingreso2018$GrantAsked <- as.numeric(ingreso2018$GrantAsked)
unique(ingreso2018[,"GottenGrant"])

unique(ingreso2018[, "GetsGrant"])
ingreso2018[which(ingreso2018[, "GetsGrant"] == "SI"),"GetsGrant"] <- "TRUE"
ingreso2018[which(ingreso2018[, "GetsGrant"] == "NO"),"GetsGrant"] <- "FALSE"
ingreso2018[which(ingreso2018[, "GetsGrant"] == "MO"),"GetsGrant"] <- "FALSE"
ingreso2018[which(ingreso2018[, "GetsGrant"] == "falta doc y CI"), "GetsGrant"] <- NA

ingreso2018$GetsGrant <- as.logical(ingreso2018$GetsGrant)

grantt <- table(ingreso2018[which(ingreso2018[,"GetsGrant"] == TRUE), "GottenGrant"])
ingreso2018[which(ingreso2018[,"GetsGrant"] == TRUE), "GottenGrant"] <- ingreso2018[which(ingreso2018[,"GetsGrant"] == TRUE), "GrantAsked"]

#Arreglo Status
unique(ingreso2018[, "Status"])
ingreso2018[which(ingreso2018[, "Status"] == "BAJA SE PASA A FCE"),"Status"] <- "BAJA"
ingreso2018[which(ingreso2018[, "Status"] == "BAJA DESAPROBO RECUPERATORIO"), "Status"] <- "BAJA"
ingreso2018[which(ingreso2018[, "Status"] == "Baja 2017"), "Status"] <- "BAJA"
ingreso2018[which(ingreso2018[, "Status"] == "bAJA DESAPROBO RECUPERATORIO"), "Status"] <- "BAJA"
ingreso2018[which(ingreso2018[, "Status"] == "BAJA PACT"), "Status"] <- "BAJA"
ingreso2018[which(ingreso2018[, "Status"] == "BAJA NO VINO A RECUPERAR"), "Status"] <- "BAJA"
ingreso2018[which(ingreso2018[, "Status"] != "BAJA"), "Status"] <- "MATRICULADO"

#Done
unique(ingreso2018[,"Career"])

#Entry (Ingreso)
unique(ingreso2018[, "Entry"])
ingreso2018[which(ingreso2018[, "Entry"] == "Ingreso Octubre Pilar"),"Entry"] <- "Octubre"
ingreso2018[which(ingreso2018[, "Entry"] == "Ingreso Febrero"),"Entry"] <- "Febrero"
ingreso2018[which(ingreso2018[, "Entry"] == "Ingreso Directo (i feb)"),"Entry"] <- "Directo"
ingreso2018[which(ingreso2018[, "Entry"] == "Ingreso Directo"),"Entry"] <- "Directo"
ingreso2018[which(ingreso2018[, "Entry"] == "Ingreso Libre Diciembre"),"Entry"] <- "Diciembre Libre"
ingreso2018[which(ingreso2018[, "Entry"] == "Ingreso Febrero MAT"),"Entry"] <- "Febrero"
ingreso2018[which(ingreso2018[, "Entry"] == "Ingreso Octubre"),"Entry"] <- "Octubre"
ingreso2018[which(ingreso2018[, "Entry"] == "Pase Universitario"),"Entry"] <- "Directo"
ingreso2018[which(ingreso2018[, "Entry"] == "Ingreso Septiembre"),"Entry"] <- "Septiembre"
ingreso2018[which(ingreso2018[, "Entry"] == "Cuatrimestral Full Time"),"Entry"] <- "Marzo"
ingreso2018[which(ingreso2018[, "Entry"] == "Ingreso Agosto"),"Entry"] <- "Agosto"
ingreso2018[which(ingreso2018[, "Entry"] == "Cuatrimestral Part Time"),"Entry"] <- "Marzo"
ingreso2018[which(ingreso2018[, "Entry"] == "Ingreso Libre Febrero MAT"),"Entry"] <- "Febrero Libre"
ingreso2018[which(ingreso2018[, "Entry"] == "Ingreso Directo (i sep)"),"Entry"] <- "Directo"
ingreso2018[which(ingreso2018[, "Entry"] == "Ingreso Directo (i oct)"),"Entry"] <- "Directo"
ingreso2018[which(ingreso2018[, "Entry"] == "Ingreso Libre Febrero"),"Entry"] <- "Febrero Libre"

# Promedio del Secundario (todo NA)
unique(ingreso2018[,"PromSec"])

#Colegio
ingreso2018 <- ingreso2018[order(ingreso2018$School),]
ingreso2018[1,"School"] <- "Escuela Tecnica Henry Ford"
ingreso2018 <- ingreso2018[order(ingreso2018$School),]

school <- table(ingreso2018[,"School"])

ingreso2018[which(ingreso2018[,"School"] == "E.E.S.T. N° 3"), "School"] <- "E.E.S.T. N°3"
ingreso2018[which(ingreso2018[,"School"] == "E.E.S.T N°3"), "School"] <- "E.E.S.T. N°3"
ingreso2018[which(ingreso2018[,"School"] == "E.E.S.T N°2"), "School"] <- "E.E.S.T. N°2"
ingreso2018[which(ingreso2018[,"School"] == "Escuela de Educacion Secundaria Tecnica N°1"), "School"] <- "E.E.S.T. N°1"
ingreso2018[which(ingreso2018[,"School"] == "Wellspring School (Del Viso)"), "School"] <- "Wellspring"

school <- table(ingreso2018[,"School"])


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

#Curar Ingreso
unique(ingreso2019[,"Entry"])
ingreso2019[which(ingreso2019[, "Entry"] == "Pase Universitario"),"Entry"] <- "Directo"
ingreso2019[which(ingreso2019[, "Entry"] == "Cuatrimestral Part Time"),"Entry"] <- "Marzo"
ingreso2019[which(ingreso2019[, "Entry"] == "Curso Octubre Nordelta"),"Entry"] <- "Octubre"
ingreso2019[which(ingreso2019[, "Entry"] == "Curso Febrero"),"Entry"] <- "Febrero"
ingreso2019[which(ingreso2019[, "Entry"] == "Ingreso Directo"),"Entry"] <- "Directo"
ingreso2019[which(ingreso2019[, "Entry"] == "Cuatrimestral Full Time 19"),"Entry"] <- "Marzo"
ingreso2019[which(ingreso2019[, "Entry"] == "Curso Septiembre"),"Entry"] <- "Septiembre"
ingreso2019[which(ingreso2019[, "Entry"] == "Curso Octubre Pilar"),"Entry"] <- "Octubre"
ingreso2019[which(ingreso2019[, "Entry"] == "Libre Diciembre"),"Entry"] <- "Diciembre Libre"
ingreso2019[which(ingreso2019[, "Entry"] == "Ingreso Directo (i cuatri)"),"Entry"] <- "Directo"
ingreso2019[which(ingreso2019[, "Entry"] == "Cuatrimestral Full Time"),"Entry"] <- "Marzo"
ingreso2019[which(ingreso2019[, "Entry"] == "Curso Febrero MAT"),"Entry"] <- "Febrero"
ingreso2019[which(ingreso2019[, "Entry"] == "Curso Septiembre (Libre)"),"Entry"] <- "Septiembre"

#Todo ok
unique(ingreso2019[,"Career"])

#Notas a numeric
unique(ingreso2019[, "Mat"])
ingreso2019[which(ingreso2019[, "Mat"] == "AUS"),"Mat"] <- 1
unique(ingreso2019[, "RMat"])
ingreso2019[which(ingreso2019[, "RMat"] == "AUS"), "RMat"] <- 1
ingreso2019[which(ingreso2019[, "RMat"] == "ausente"), "RMat"] <- 1
ingreso2019[which(ingreso2019[, "RMat"] == "APROBADO"), "RMat"] <- 4
ingreso2019[which(ingreso2019[, "RMat"] == "-"), "RMat"] <- NA
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
unique(ingreso2019[, "PromSec"])
prom <- table(ingreso2019[, "PromSec"])
ingreso2019[which(ingreso2019[, "PromSec"] == 50),"PromSec"] <- NA
ingreso2019[which(ingreso2019[, "PromSec"] == "7,51"),"PromSec"] <- 7.51
ingreso2019[which(ingreso2019[, "PromSec"] == "9,08"),"PromSec"] <- 9.08
ingreso2019$PromSec <- as.numeric(ingreso2019$PromSec)

#Becas
unique(ingreso2019[,"GrantType"])
unique(ingreso2019[,"GrantAsked"])
grants <- table(ingreso2019[,"GrantAsked"])

ingreso2019[which(ingreso2019[, "GrantAsked"] == "40/?"), "GrantAsked"] <- 40
ingreso2019[which(ingreso2019[, "GrantAsked"] == "40/30"),"GrantAsked"] <- 70
ingreso2019[which(ingreso2019[, "GrantAsked"] == "40/20 (TOTAL 100/60%)"),"GrantAsked"] <- 60
ingreso2019[which(ingreso2019[, "GrantAsked"] == "30/40"),"GrantAsked"] <- 70
ingreso2019[which(ingreso2019[, "GrantAsked"] == "40/? (100 % total)"),"GrantAsked"] <- 100
ingreso2019[which(ingreso2019[, "GrantAsked"] == "40/20"),"GrantAsked"] <- 60
ingreso2019[which(ingreso2019[, "GrantAsked"] == "40+?"), "GrantAsked"] <- 40
ingreso2019[which(ingreso2019[, "GrantAsked"] == "40+60"),"GrantAsked"] <- 100
ingreso2019[which(ingreso2019[, "GrantAsked"] == "50/20"),"GrantAsked"] <- 70
ingreso2019[which(ingreso2019[, "GrantAsked"] == "50/50"),"GrantAsked"] <- 100
ingreso2019[which(ingreso2019[, "GrantAsked"] == "60/20"),"GrantAsked"] <- 80
ingreso2019[which(ingreso2019[, "GrantAsked"] == "60/40"),"GrantAsked"] <- 100
which(ingreso2019[,"GrantAsked"] == "80 %")
ingreso2019[171, 13] <- 80
ingreso2019[which(ingreso2019[, "GrantAsked"] == "80 TOTAL"),"GrantAsked"] <- 80

ingreso2019$GrantAsked <- as.numeric(ingreso2019$GrantAsked)

unique(ingreso2019[,"GetsGrant"])

ingreso2019[which(ingreso2019[, "GetsGrant"] == "OBTIENE"),"GetsGrant"] <- TRUE
ingreso2019[which(ingreso2019[, "GetsGrant"] == "Si"),"GetsGrant"] <- TRUE
ingreso2019[which(ingreso2019[, "GetsGrant"] == "NO OBTIENE pero se le da x ayuda"),"GetsGrant"] <- TRUE
ingreso2019[which(ingreso2019[, "GetsGrant"] == "NO OBTIENE"),"GetsGrant"] <- FALSE
ingreso2019[which(ingreso2019[, "GetsGrant"] == "NO"),"GetsGrant"] <- FALSE
ingreso2019[which(ingreso2019[, "GetsGrant"] == "No"),"GetsGrant"] <- FALSE
ingreso2019[which(ingreso2019[, "GetsGrant"] == "N/A"),"GetsGrant"] <- NA
ingreso2019[which(ingreso2019[, "GetsGrant"] == "BFI N/A"),"GetsGrant"] <- NA
ingreso2019[which(ingreso2019[, "GetsGrant"] == "BAJA"),"GetsGrant"] <- NA
ingreso2019[which(ingreso2019[, "GetsGrant"] == "?"),"GetsGrant"] <- NA

ingreso2019$GetsGrant <- as.logical(ingreso2019$GetsGrant)

granted <- table(ingreso2019[,"GottenGrant"])
granted

library(stringr)

ingreso2019[which(ingreso2019[, "GottenGrant"] == "70% (30 BMA + 30 PU + 10 BAU)"),"GottenGrant"] <- 70
ingreso2019[which(ingreso2019[, "GottenGrant"] == "100 (60 BEA + 40 BAU)"),"GottenGrant"] <- 100
ingreso2019[which(ingreso2019[, "GottenGrant"] == "80 (50 bct + 30 bau)"),"GottenGrant"] <- 80
ingreso2019[which(ingreso2019[, "GottenGrant"] == "100% (60% BEA+ 15% BAU + 25% PU)"),"GottenGrant"] <- 100

ingreso2019$GottenGrant[!grepl("100", ingreso2019$GottenGrant)] <- substr(ingreso2019$GottenGrant[!grepl("100", ingreso2019$GottenGrant)], 1, 2)

grantt19 <- data.frame(ingreso2019[which(ingreso2019[,"GetsGrant"] == TRUE), "GottenGrant"], ingreso2019[which(ingreso2019[,"GetsGrant"] == TRUE), "GrantAsked"])
getgrant <- which(ingreso2019[,"GetsGrant"] == TRUE & is.na(ingreso2019[,"GottenGrant"]))
ingreso2019[ getgrant, "GottenGrant"] <- ingreso2019[getgrant, "GrantAsked"]


#Unificar colegios
school19 <- table(ingreso2019[,"School"])
school19
ingreso2019[which(ingreso2019[, "School"] == "Colegio Del Pilar"),"School"] <- "Colegio del Pilar"
ingreso2019[which(ingreso2019[, "School"] == "E.E.S.N 27"), "School"] <- "E.E.S. N°27"
ingreso2019[which(ingreso2019[, "School"] == "Escuela de Educación Técnica Nº2 Santiago Derqui (Derqui)"), "School"] <- "E.E.S.T Nº2"
ingreso2019[which(ingreso2019[, "School"] == "Oakhill (Pilar)"), "School"] <- "Oakhill"
ingreso2019[which(ingreso2019[, "School"] == "Santa Inés (San Isidro)"), "School"] <- "Santa Ines"
ingreso2019[which(ingreso2019[, "School"] == "Santa Maria (Pilar)"), "School"] <- "Santa Maria"
ingreso2019[which(ingreso2019[, "School"] == "St. Catherine´s Moorlands (Tortuguitas)"), "School"] <- "St. Catherine's Moorlands"
ingreso2019[which(ingreso2019[, "School"] == "St. George´s College North"), "School"] <- "St. George's College North"
ingreso2019[which(ingreso2019[, "School"] == "St. George´s (Quilmes)"), "School"] <- "St. George's College North"
ingreso2019[which(ingreso2019[, "School"] == "St. John´s (Pilar)"), "School"] <- "St. John's"
ingreso2019[which(ingreso2019[, "School"] == "St. Gregory´s (Vicente López)"), "School"] <- "St. Gregory's"
ingreso2019[which(ingreso2019[, "School"] == "St. Mary of the Hills (San Fernando)"), "School"] <- "St. Mary of the Hills"
ingreso2019[which(ingreso2019[, "School"] == "St. Matthew´s North (Pilar)"), "School"] <- "St. Mathew's College North"
ingreso2019[which(ingreso2019[, "School"] == "Sworn College Junior (Pacheco)"), "School"] <- "Sworn Junior College (Pacheco)"
ingreso2019[which(ingreso2019[, "School"] == "Verbo Divino (Pilar)"), "School"] <- "Verbo Divino"
ingreso2019[which(ingreso2019[, "School"] == "Bede´s Grammar School (Tortuguitas)"), "School"] <- "Bede's Grammar School (Tortuguitas)"

save(file = "ingreso2019.rdata", ingreso2019)

#-------------------------------------------------------------------------------------------------------------------------------------------------------

ingreso <- merge.data.frame(ingreso2018,ingreso2019, all = TRUE)

schools <- table(ingreso[,"School"])



