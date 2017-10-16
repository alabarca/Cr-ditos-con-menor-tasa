
#install.packages("lubridate")
library(lubridate)
fecha=c("2015-08-31","2015-09-30","2015-10-31","2015-11-30","2015-12-31",
        "2016-01-31","2016-02-29","2016-03-31","2016-04-30","2016-05-31","2016-06-30",
        "2016-07-31","2016-08-31","2016-09-30","2016-10-31","2016-11-30","2016-12-31",
        "2017-01-31","2017-02-28","2017-03-31","2017-04-30","2017-05-31","2017-06-30",
        "2017-07-31","2017-08-31","2017-09-30")

objetivo=subset(query1,dias_mora<="90")#validado con dias_mora
loop=length(fecha)
i=n=1
k=as.Date("2017-08-31")
mora=dmora=c()
while(i<loop){
  mes=subset(objetivo,fecha_snapshot==as.Date("2017-08-31"))
  while(k<as.Date("2017-09-30")){
    print(k)
    k=k+1
    compara=subset(query1,fecha_snapshot==k)
    final=compara[compara$rc_credito_numero_credito %in% mes$rc_credito_numero_credito,]
    dmora[n]=mean(final$dias_mora)
    n=n+1
  }
  n=1
  mora[i]=mean(dmora)
  dmora=c()
  i=i+1
}
setwd("C:/Users/cgomez/Desktop")
write.table(mora,file="mora_diario.csv",sep=";",dec=",",col.names=FALSE,row.names=TRUE)