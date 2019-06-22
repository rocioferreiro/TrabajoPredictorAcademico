library(readxl)
library(dplyr)
INGRESO2018 <- read_excel("INGRESO 2018-ANON.xlsx")
INGRESO2019 <- read_excel("INGRESO 2019-ANON.xlsx")
INGRESO2018_2019 <- read_excel("INGRESO 2018-2019-ANON.xlsx")
INGRESO2018 <- INGRESO2018 %>% rename(Ingreso=Oportunidad..del.Ingreso)
INGRESO2019$Status <- INGRESO2019$Estado.del..Aspirante
INGRESO2019[,19] <- NULL
INGRESO2019[,18] <- NULL
strErrorFix <- function(str, tokenArray, fixedArray, booleanFixed = FALSE){
  newArray <- sapply(tokenArray, grepl, str, ignore.case = TRUE, fixed = booleanFixed)
  index <- which(newArray == TRUE)[1]
  if(!is.na(index)){
    fixedArray[index]
  }else{
    str
  }
}
INGRESO2018$Cohorte <- "2018"

INGRESO2019$COhorte <- "2019"
INGRESOMERGED <- merge(INGRESO2018,INGRESO2019, all = TRUE)
INGRESOMERGED[,1] <- NULL
trim_name <- function(name){
  return(unlist(strsplit(name, split="[(]"))[1])
}
INGRESOMERGED$Colegio.de..Procedencia <- sapply(INGRESOMERGED$Colegio.de..Procedencia, trim_name)
INGRESOMERGED <- INGRESOMERGED %>% rename(Colegio=Colegio.de..Procedencia)
INGRESOMERGED[INGRESOMERGED == "N/A"] <- NA
INGRESOMERGED[INGRESOMERGED == "-"] <- NA
INGRESO2018_2019 <- INGRESO2018_2019 %>% rename(Cohorte=Ingreso)
INGRESO2018_2019$Nacionalidad=NULL
INGRESO2018_2019$Provincia=NULL
INGRESO2018_2019$Zona=NULL
INGRESO2018_2019$Año.de.graduación=NULL
INGRESO2018_2019 <- INGRESO2018_2019 %>% rename(Colegio=Nombre.de.Cuenta)
wantToChange <- c("Ingeniería Ind", "Ingeniería en Inf","Ingeniería Bi")
toChange <- c("IND","INF","BIO")
INGRESO2018_2019$Carrera.de.grado <- sapply(INGRESO2018_2019$Carrera.de.grado, strErrorFix, wantToChange, toChange)
changing <- c("Ingeniería Ind", "Ingeniería en Inf","Ingeniería Bi")
changed <- c("IND","INF","BIO")
INGRESO2018_2019$Carrera.de.grado <- sapply(INGRESO2018_2019$Carrera.de.grado, strErrorFix, changing, changed)
INGRESO2018_2019 <- INGRESO2018_2019 %>% rename(Carrera=Carrera.de.grado)
INGRESO2018_2019 <- INGRESO2018_2019 %>% rename(Ingreso=Ingeniería)
INGRESO2018_2019 <- INGRESO2018_2019 %>% rename(Cal.FIS=Nota.Física)
INGRESO2018_2019 <- INGRESO2018_2019 %>% rename(Cal.MAT=Nota.Matemática)
INGRESO2018_2019$Resultado.del.curso.de.ingreso=NULL
INGRESO2018_2019 <- INGRESO2018_2019 %>% rename(Porcentaje..Otorgado=Porcentaje.beca.otorgada)
INGRESO2018_2019 <- INGRESO2018_2019 %>% rename(Prom..Sec=Promedio.Escolar)
INGRESOS <- merge(INGRESOMERGED,INGRESO2018_2019,all=TRUE)
INGRESOS <- INGRESOS[-which(duplicated(INGRESOS$Nombres)), ]
INGRESOS$Colegio <- sapply(INGRESOS$Colegio, trim_name)
changing <- c("Angel D'Elia", "Bede", "Cardenal Newman", "Cardenal Pironio", "Inmaculada Concepción", "CIREG", "Colegio Marista", "Colegio Nacional Campo", "Colegio Peruano Britanico", "Colegio San Vicente de Paul", "Colegio Santa Soledad Torres Acosta", "Dante Alighieri", "Escuela de Educacion Tecnica N°1", "Escuela de Educación Técnica N°2", "Escuela del Alba", "Escuela Polimodal República Argentina DIEPREGEP N° 5704", "Escuela Provincial Técnica 760 Guardacostas Rio Iguazu", "Escuela Técnica ORT N° 2", "Gabriele D'annunzio", "Goethe", "Hans Christian Andersen", "Holters Natur", "Instituto Adventista Andres Guacurari", "Instituto Alfredo R. Bufano", "Instituto de Enseñanza Secundaria Martín Miguel de Guemes", "Instituto Español Virgen del Pilar", "Instituto Linea Cuchilla", "Ipesmi", "Jose Manuel Estrada", "Los Molinos", "Los Robles", "Northfield School", "Northlands", "Nuestra Señora de Luján", "Oakhill", "Pilgrims", "San Felipe", "San Jose", "Santa Ines", "Santa Maria", "Santa Teresa", "Santo Tomas", "St. Francis", "St. George's", "St. Mary of the Hills", "St. Matthew's", "Sworn", "Wellspring School", "St. George´s North", "St. George´s College North", "St. John´s", "E.E.S.T. N° 3", "Antonio Berni ", "Brick Towers College ", "Ángel D'Elía ", "Del Viso Day School ", "Educacional Fátima ", "St. Catherine´s Moorlands ",  "St. Catherine's Moorlands" , "Santa María ", "Santa María de Luján ", "St. George´s ", "St. George's College North", "St. George´s College North", "St. George´s North ", "St. John´s ", "St. Matthew´s North ", "Wellspring School")
changed <- c("Ángel D'Elía", "Bede's Grammar School", "Cardenal Newman", "Cardenal Pironio", "Colegio de la Inmaculada Concepción", "CIREG", "Colegio Marista", "Colegio Nacional Campo", "Colegio Peruano Britanico", "Colegio San Vicente de Paul", "Colegio Santa Soledad Torres Acosta", "Dante Alighieri", "Escuela de Educación Técnica N°1", "Escuela de Educación Técnica N°2", "Escuela del Alba", "Escuela Polimodal República Argentina DIEPREGEP N° 5704", "Escuela Provincial Técnica 760 Guardacostas Rio Iguazu", "Escuela Técnica ORT N°2", "Gabriele D'annunzio", "Goethe", "Hans Christian Andersen", "Holters Natur", "Instituto Adventista Andres Guacurari", "Instituto Alfredo R. Bufano", "Instituto de Enseñanza Secundaria Martín Miguel de Guemes", "Instituto Español Virgen del Pilar", "Instituto Linea Cuchilla", "Ipesmi", "Jose Manuel Estrada", "Los Molinos", "Los Robles", "Northfield School", "Northlands", "Nuestra Señora de Luján", "Oakhill", "Pilgrims College", "San Felipe Apostol", "San José", "Santa Inés", "Santa María", "Santa Teresa", "Santo Tomas de Aquino", "St. Francis School", "St. George's College North", "St. Mary of the Hills", "St. Matthew's", "Sworn", "Wellspring School","St. George's College North","St. George's College North", "St. John's", "E.E.S.T. N°3", "Antonio Berni", "Brick Towers", "Ángel D'Elía", "Del Viso Day School", "Educacional Fátima", "Moorlands", "Moorlands", "Santa María", "Santa María", "St. George's", "St. George's", "St. George's", "St. George's", "St. John's", "St. Matthew's", "Wellspring" )
INGRESOS$Colegio <- sapply(INGRESOS$Colegio, strErrorFix, changing, changed)
INGRESOS$Colegio <- sapply(INGRESOS$Colegio, strErrorFix, changing, changed)
changing <-c("7","2","5","6","8","9","A")
changed <- c("7","2","5","6","8","9","1")
INGRESOS$Cal.MAT <- sapply(INGRESOS$Cal.MAT, strErrorFix, changing, changed)
INGRESOS$Cal.FIS <- sapply(INGRESOS$Cal.FIS, strErrorFix, changing, changed)
INGRESOS$Rec.MAT <- sapply(INGRESOS$Rec.MAT, strErrorFix, changing, changed)
INGRESOS$Rec.FIS <- sapply(INGRESOS$Rec.FIS, strErrorFix, changing, changed)
changing <-c("Cuatrimestral Full Time 19","Curso Feb","Curso Oct","Curso Septiembre (Libre)","Curso Septiembre","Febrero Libre","Ingreso Dire","Libre Diciem","Ingreso Octubre","Ingreso Febrero MAT","Febrero ","Octubre en Pil","Examen Lib","Septiembre en P","Octubre en San Is")
changed<-c("Cuatrimestral Full Time","Ingreso Febrero","Ingreso Octubre","Ingreso Libre","Ingreso Septiembre","Ingreso Libre","Ingreso Directo","Ingreso Libre","Ingreso Octubre","Ingreso Febrero","Ingreso Febrero","Ingreso Octubre","Ingreso Libre","Ingreso Septiembre","Ingreso Octubre")
INGRESOS$Ingreso <- sapply(INGRESOS$Ingreso, strErrorFix, changing, changed)
INGRESOS$...1=NULL
changing<-c("OBTIENE","MO","SI","No","Si","BAJA","BFI","?")
changed<-c("SI","NO","SI","NO","SI","NO","SI","NO")
INGRESOS$Obtiene..BECA <-sapply(INGRESOS$Obtiene..BECA,strErrorFix,changing,changed)
INGRESOS$Obtiene..BECA[is.na(INGRESOS$Obtiene..BECA)]<- "NO"
changing<-c("Ingreso Febrero","Ingreso Octubre","Cuatrimestral Part Time","Ingreso Directo","Pase Universitario","Ingreso Septiembre","Ingreso Agosto","Cuatrimestral Full Time","Ingreso Libre")
changed<-c("Febrero","Octubre","PartTime","Directo","Pase","Septiembre","Agosto","FullTime","Libre")
INGRESOS$Ingreso<-sapply(INGRESOS$Ingreso,strErrorFix,changing,changed)
INGRESOS$Obtiene..BECA<- as.factor(INGRESOS$Obtiene..BECA)
INGRESOS$Cal.FIS<- as.numeric(INGRESOS$Cal.FIS)
INGRESOS$Cal.MAT<- as.numeric(INGRESOS$Cal.MAT)
INGRESOS$Rec.FIS<- as.numeric(INGRESOS$Rec.FIS)
INGRESOS$Rec.MAT<- as.numeric(INGRESOS$Rec.MAT)
INGRESOS$PROM.CI<- as.numeric(INGRESOS$PROM.CI)
INGRESOS$Prom..Sec<- as.numeric(INGRESOS$Prom..Sec)
Analisis_IND <- read_excel("Analisis IND.xlsx")
Analisis_INF <- read_excel("Analisis INF.xlsx")
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
ingreso<-INGRESOS
rm(INGRESO2018)
rm(INGRESO2019)
rm(INGRESO2018_2019)
rm(INGRESOMERGED)
names(ingreso) <- c("Sex", "Career", "Year", "Entry","Mat", "Fis", "PromSec", "Grant", "School", "Status", "Name", "RMat", "RFis", "PromCI", "GrantTipe","GrantRequest","GetsGrant","Nombre Completo")
ingreso$Name[7]<-"Aguirres Garcia Pitronaci"
ingreso[,18]=NULL
baja <- c("BAJA SE PASA A FCE", "BAJA DESAPROBO RECUPERATORIO", "bAJA DESAPROBO RECUPERATORIO", "Baja 2017", "BAJA PACT", "BAJA NO VINO A RECUPERAR")
ingreso$Status <- sapply(ingreso$Status, strErrorFix, baja, rep("BAJA", length(baja)))
ingreso[which(ingreso[, "Status"] != "BAJA"), "Status"] <- "MATRICULADO"
save(file = "ingreso.rdata", ingreso)
changing<-c("Feb","Oct","Part","Dir","Pas","Sep","Ago","Full","Lib")
INGRESOS$Ingreso<-sapply(INGRESOS$Ingreso,strErrorFix,changed,changing)
save(file = "INGRESOS.RData", INGRESOS)