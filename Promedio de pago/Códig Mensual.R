#install.packages("lubridate")
#library(lubridate)
fecha=c("2015-08-31","2015-09-30","2015-10-31","2015-11-30","2015-12-31",
            "2016-01-31","2016-02-29","2016-03-31","2016-04-30","2016-05-31","2016-06-30"
            ,"2016-07-31","2016-08-31","2016-09-30","2016-10-31","2016-11-30","2016-12-31","2017-01-31","2017-02-28","2017-03-31","2017-04-30","2017-05-31",
             "2017-06-30","2017-07-31","2017-08-31","2017-09-30")
objetivo=subset(query1,rc_estado_pago_codigo=="1")#validado con dias_mora
loop=length(fecha)
i=1
j=1
mora=c()
while(i<loop){
  mes=subset(objetivo,fecha_snapshot==as.Date(fecha[i]))
  compara=subset(query1,fecha_snapshot==as.Date(fecha[i+1]))
  final=compara[compara$rc_credito_numero_credito %in% mes$rc_credito_numero_credito,]
  mora[i]=mean(final$dias_mora)
  i=i+1
}
setwd("C:/Users/cgomez/Desktop")
write.table(mora,file="moramensual.csv",sep=";",dec=",",col.names=FALSE,row.names=TRUE)
