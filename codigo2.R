#2.	Automatiza la carga anterior mediante un bucle y genera una función en la que 
#utilizando como parámetro el mes te devuelva lo siguiente: “El país con mayor volumen 
#de importaciones ha sido XXX y en el que Euskadi más ha exportado ha sido YYY” (2)

funcion<-function(mes_){
  library(readxl)
  library(dplyr)
  
  rm(list = ls())
  
  fichero_enero<-read_xlsx('Import_Export_Euskadi.xlsx', sheet = 'Enero')
  fichero_enero<-fichero_enero%>%
    mutate(mes=1)
  fichero_feb<-read_xlsx('Import_Export_Euskadi.xlsx', sheet = 'Febrero')
  fichero_feb<-fichero_feb%>%
    mutate(mes=2)
  fichero_mar<-read_xlsx('Import_Export_Euskadi.xlsx', sheet = 'Marzo')
  fichero_mar<-fichero_mar%>%
    mutate(mes=3)
  fichero_abril<-read_xlsx('Import_Export_Euskadi.xlsx', sheet = 'Abril')
  fichero_abril<-fichero_abril%>%
    mutate(mes=4)
  fichero_may<-read_xlsx('Import_Export_Euskadi.xlsx', sheet = 'Mayo')
  fichero_may<-fichero_may%>%
    mutate(mes=5)
  fichero_jun<-read_xlsx('Import_Export_Euskadi.xlsx', sheet = 'Junio')
  fichero_jun<-fichero_jun%>%
    mutate(mes=6)
  fichero_jul<-read_xlsx('Import_Export_Euskadi.xlsx', sheet = 'Julio')
  fichero_jul<-fichero_jul%>%
    mutate(mes=7)
  fichero_agos<-read_xlsx('Import_Export_Euskadi.xlsx', sheet = 'Agosto')
  fichero_agos<-fichero_agos%>%
    mutate(mes=8)
  fichero_sep<-read_xlsx('Import_Export_Euskadi.xlsx', sheet = 'Septiembre')
  fichero_sep<-fichero_sep%>%
    mutate(mes=9)
  fichero_oct<-read_xlsx('Import_Export_Euskadi.xlsx', sheet = 'Octubre')
  fichero_oct<-fichero_oct%>%
    mutate(mes=10)
  fichero_nov<-read_xlsx('Import_Export_Euskadi.xlsx', sheet = 'Noviembre')
  fichero_nov<-fichero_nov%>%
    mutate(mes=11)
  fichero_dic<-read_xlsx('Import_Export_Euskadi.xlsx', sheet = 'Diciembre')
  fichero_dic<-fichero_dic%>%
    mutate(mes=12)
  
  colnames(fichero_enero)[6]<-'cantidad'
  colnames(fichero_feb)[6]<-'cantidad'
  colnames(fichero_mar)[6]<-'cantidad'
  colnames(fichero_abril)[6]<-'cantidad'
  colnames(fichero_may)[6]<-'cantidad'
  colnames(fichero_jun)[6]<-'cantidad'
  colnames(fichero_jul)[6]<-'cantidad'
  colnames(fichero_agos)[6]<-'cantidad'
  colnames(fichero_sep)[6]<-'cantidad'
  colnames(fichero_oct)[6]<-'cantidad'
  colnames(fichero_nov)[6]<-'cantidad'
  colnames(fichero_dic)[6]<-'cantidad'
  
  colnames(fichero_enero)<-colnames(fichero_feb)
  colnames(fichero_enero)<-colnames(fichero_mar)
  colnames(fichero_enero)<-colnames(fichero_may)
  colnames(fichero_enero)<-colnames(fichero_jun)
  colnames(fichero_enero)<-colnames(fichero_jul)
  colnames(fichero_enero)<-colnames(fichero_agos)
  colnames(fichero_enero)<-colnames(fichero_abril)
  colnames(fichero_enero)<-colnames(fichero_sep)
  colnames(fichero_enero)<-colnames(fichero_oct)
  colnames(fichero_enero)<-colnames(fichero_nov)
  colnames(fichero_enero)<-colnames(fichero_dic)
  
  datos<-rbind(fichero_enero,fichero_feb,fichero_mar,fichero_abril,fichero_may,fichero_jun,fichero_jul, fichero_agos,fichero_sep,fichero_oct,fichero_nov,fichero_dic)
  str(datos)
  datos$cantidad<-as.numeric(datos$cantidad)
  
  importaciones<-datos%>%
    group_by(`territorio histórico`, mes)%>%
    filter(flujo=='Importaciones')%>%
    summarise(imp_total=sum(cantidad, na.rm = T))
  
  exportaciones<-datos%>%
    group_by(`territorio histórico`, mes)%>%
    filter(flujo=='Exportaciones')%>%
    summarise(imp_total=sum(cantidad, na.rm = T))
  
  colnames(importaciones)<-c('territorio.impo','mes.impo','cantidad.impo')
  colnames(exportaciones)<-c('territorio.ex','mes.ex','cantidad.ex')
  
  final<-merge(importaciones,exportaciones,all = TRUE)
  
  final<-final%>%
    mutate(suma=cantidad.impo+cantidad.ex)
  
ejercicio2<-final%>%
  mutate(suma=cantidad.impo+cantidad.ex)

ejercicio2<-ejercicio2%>%
  filter(mes.impo==mes_ & mes.ex==mes_)%>%
  arrange(desc(suma))%>%
  head(1)

print(paste('El país con mayor volumen de importaciones ha sido', ejercicio2$territorio.impo, 'y en el que Euskadi más ha exportado ha sido', ejercicio2$territorio.ex))
}
