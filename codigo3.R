#	Genera otra función que utilizando como parámetro una sección arancelaria y un 
#país te devuelva una gráfica con el volumen de exportaciones que ha realizado 
#Euskadi con dicho país en el tipo de productos indicados por la sesión arancelaria mes a mes. (2)
library(ggplot2)
funcion2<-function(seccion_, pais){
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
  
  datos$mes<-as.factor(datos$mes)
  datos$cantidad<-as.numeric(datos$cantidad)

  exportaciones<-datos%>%
    group_by(mes)%>%
    filter(flujo=='Exportaciones')%>%
    filter(`país`==pais)%>%
    filter(`sección arancelaria`== seccion_)
    summarise(imp_total=sum(cantidad, na.rm = T))

  colnames(exportaciones)<-c('seccion.ex','pais.ex','mes.ex','cantidad.ex')
  
  grafico<-ggplot(exportaciones ,aes(x=mes.ex, y=imp_total))+geom_line()
  return(grafico)
}
funcion2('VII Plásticos y caucho','Arabia Saudí')

