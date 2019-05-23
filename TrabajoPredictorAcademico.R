#Tnsert Ingreso 2018
ingreso2018 <- read_excel("~/AYED/Predictor/Archivos/INGRESO 2018-ANON.xlsx")

names(ingreso2018)
names(ingreso1819)

#Renombrar Columnas
names(ingreso2018) <- c("pos", "Status", "Name", "Sex", "Career", "Year", "Entry", "Mat", "RMat", "Fis", "RFis", "PromCI", "GrantType", "GrantAsked", "GetsGrant", "PromSec", "GottenGrant", "School")
names(ingreso2018)
names(ingreso1819)<-c("EntryYear","Name","Sex","School","Nationality","Province","District","Year","Career","Entry","Fis","Mat","GetsGrant","PromSec","Status")



#Reordenar Columnas, eliminar pos
ingreso2018 <- ingreso2018[, c(1,6,3,4,2,5,7,8,9,10,11,12,13,14,15,17,18,16)]
ingreso2018 <- ingreso2018[, -1]
ingreso1819<-ingreso1819[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]

#Emparejar datos
names(ingreso2018)
unique(ingreso2018[,1])
unique(ingreso2018[,"Mat"])
unique(ingreso1819[,"Entry"])

#Organizar Entry 
vectoresFebrero <- which(ingreso1819[,"Entry"] == "Febrero en Pilar")
ingreso1819[vectoresFebrero,"Entry"] <- "Febrero"
vectoresCuatrimestre <-which(ingreso1819[,"Entry"] == "Cuatrimestral Full Time")
ingreso1819[vectoresCuatrimestre,"Entry"] <- "Cuatrimestral"
vectoresagosto <-which(ingreso1819[,"Entry"] == "Ingreso Agosto")
ingreso1819[vectoresagosto,"Entry"] <- "Agosto"
vectoresFebrero2 <- which(ingreso1819[,"Entry"] == "Febrero (Sede Pilar)")
ingreso1819[vectoresFebrero2, "Entry"] <- "Febrero"
vectoresCuatrimestreP <- which(ingreso1819[,"Entry"] == "Cuatrimestral Part Time")
ingreso1819[vectoresCuatrimestreP,"Entry"] <- "Cuatrimestral"
vectoresOctubre <- which(ingreso1819[,"Entry"] == "Octubre en Pilar")
ingreso1819[vectoresOctubre,"Entry"]<- "Octubre"
vectoresOctubreSI<-which(ingreso1819[,"Entry"] == "Octubre en San Isidro")
ingreso1819[vectoresOctubreSI,"Entry"]<-"Octubre"
vectoresSeptiembre2 <- which(ingreso1819[,"Entry"] == "Septiembre en Pilar")
ingreso1819[vectoresSeptiembre2,"Entry"] <- "Septiembre"


#Organizar Notas
which(ingreso2018[, "Mat"] == "Desaprobado")
ingreso2018[c(63,79,162,175),"Mat"] <- 1
which(ingreso2018[,"Mat"] == "-")
ingreso2018[c(52, 70, 81, 116, 144, 149), "Mat"] <- NA
which(ingreso2018[,"Mat"] == "Es pase")
ingreso2018[c(77, 129,172), "Mat"] <- NA
which(ingreso2018[,"Mat"] == "Es pase interno")
ingreso2018[146, "Mat"] <- NA
which(ingreso2018[,"Mat"] == "A")
ingreso2018[82, "Mat"] <- 0

names(ingreso1819)
which(ingreso1819[,"Entry"] == "Octubre en San Isidro") 



which(ingreso2018[, "Fis"] == "Desaprobado")
ingreso2018[c(162),"Fis"] <- 1
which(ingreso2018[,"Fis"] == "-")
ingreso2018[c(52, 70, 81, 92, 116, 140, 149), "Fis"] <- NA
which(ingreso2018[,"Fis"] == "A")
ingreso2018[43, "Fis"] <- 0

ingreso2018$PromCI = round(rowMeans(ingreso2018[ , 7:10], na.rm = TRUE), digit = 2)

ingreso2018$Mat <- as.numeric(ingreso2018$Mat)
ingreso2018$RMat <- as.numeric(ingreso2018$RMat)
ingreso2018$Fis <- as.numeric(ingreso2018$Fis)
ingreso2018$RFis <- as.numeric(ingreso2018$RFis)


# la barra wtf
unique(ingreso2018[,"GrantAsked"])

#Arreglo Status
unique(ingreso2018[, "Status"])
ingreso2018[which(ingreso2018[, "Status"] == "BAJA SE PASA A FCE"),"Status"] <- "BAJA"
ingreso2018[which(ingreso2018[, "Status"] == "BAJA DESAPROBO RECUPERATORIO"), "Status"] <- "BAJA"
ingreso2018[which(ingreso2018[, "Status"] == "Baja 2017"), "Status"] <- "BAJA"
ingreso2018[which(ingreso2018[, "Status"] == "bAJA DESAPROBO RECUPERATORIO"), "Status"] <- "BAJA"
ingreso2018[which(ingreso2018[, "Status"] == "BAJA PACT"), "Status"] <- "BAJA"
ingreso2018[which(ingreso2018[, "Status"] == "BAJA NO VINO A RECUPERAR"), "Status"] <- "BAJA"
ingreso2018[which(ingreso2018[, "Status"] != "BAJA"), "Status"] <- "MATRICULADO"

#Donee
unique(ingreso2018[,"Career"])

#Ni idea que poner
unique(ingreso2018[, "Entry"])

# todo NA
unique(ingreso2018[,"PromSec"])

#Paso a boolean
unique(ingreso2018[, "GetsGrant"])
ingreso2018[which(ingreso2018[, "GetsGrant"] == "SI"),"GetsGrant"] <- "TRUE"
ingreso2018[which(ingreso2018[, "GetsGrant"] == "NO"),"GetsGrant"] <- "FALSE"
ingreso2018[which(ingreso2018[, "GetsGrant"] == "MO"),"GetsGrant"] <- "FALSE"
ingreso2018[which(ingreso2018[, "GetsGrant"] == "falta doc y CI"), "GetsGrant"] <- NA

ingreso2018$GetsGrant <- as.logical(ingreso2018$GetsGrant)

#Colegio
ingreso2018 <- ingreso2018[order(ingreso2018$School),]
ingreso2018[1,"School"] <- "Escuela Tecnica Henry Ford"
ingreso2018 <- ingreso2018[order(ingreso2018$School),]


#Grant
ingreso2018[which(ingreso2018[,"GetsGrant"] == TRUE), "GottenGrant"] <- ingreso2018[which(ingreso2018[,"GetsGrant"] == TRUE), "GrantAsked"]

# prom <- function(x,y){
#   (x+y)/2
# }
# 
# ingreso2018[which(ingreso2018[,"Mat"] > 3) && which(ingreso2018[,"Fis"] > 3), "PromCI"] <- prom(ingreso2018[which(ingreso2018[,"Mat"] > 3 && ingreso2018[,"Fis"] > 3),"Mat"],ingreso2018[which(ingreso2018[,"Fis"] > 3 && ingreso2018[,"Mat"] > 3), "Fis"])
# 
# unique(ingreso2018[which(ingreso2018[,"Mat"] > 3) && which(ingreso2018[,"Fis"] > 3), "PromCI"])

save(file = "ingreso2018.rdata", ingreso2018)
save(file = "ingreso1819.rdata", ingreso1819)
#__________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________-

#Tnsert Ingreso 2019
ingreso2019 <- read_excel("~/Downloads/Archivo/INGRESO 2019-ANON.xlsx")
save(file = "ingreso2019.rdata", ingreso2019)

names(ingreso2019)

#Renombrar Columnas
names(ingreso2019) <- c("pos", "Status", "Name", "Sex", "Career", "Year", "Entry", "Mat", "RMat", "Fis", "RFis", "PromCI", "GrantType", "GrantAsked", "GetsGrant", "PromSec", "GottenGrant","State","IngStatus", "School")
names(ingreso2019)

ingreso2019 <- ingreso2019[, c(1,6,3,4,2,18,19,5,7,8,9,10,11,12,13,14,15,17,20,16)]
ingreso2019 <- ingreso2019[, -1]


unique(ingreso2019[,"State"])
unique(ingreso2019[,"Status"])
unique(ingreso2019[,"IngStatus"])

unique(ingreso2019[,"Career"])
 
unique(ingreso2019[which(is.na(ingreso2019[,"State"])),"Status"])




