
ingreso1819 <- read_excel("~/Desktop/Excels/INGRESO 2018-2019-ANON.xlsx")

names(ingreso1819)

names(ingreso1819)<-c("pos", "EntryYear","Name","Sex","School","Nationality","Province","District","Year","Career","Entry","Fis","Mat","GetsGrant","PromSec","Status")

ingreso1819<-ingreso1819[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]

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

names(ingreso1819)
which(ingreso1819[,"Entry"] == "Octubre en San Isidro") 


schools1819 <- table(ingreso1819[,"School"])







