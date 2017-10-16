#modificar sólo la sintaxis en SQL, añadiendo el mes de la cosecha mas reciente
#install.packa
library(zoo)
setwd("C:/Users/igrau/Desktop")
area=read.delim("C:/Users/igrau/Desktop/Areas cod. productos.txt", header=FALSE)
library(RPostgreSQL)
#install.packages("RPostgreSQL")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "dbbncriskguard"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con1 <- dbConnect(drv, dbname = "riskguard_production",
                  host = "192.168.99.4", port = 5432,
                  user = "consulta", password = pw)
rm(pw) # removes the password

v=c("select rccc.fecha_snapshot, rccc.rc_credito_tc_cliente_codigo, tccl.tc_tipo_persona_codigo, tccl.numero_documento,  tccl.nombre, rccc.rc_credito_numero_credito, rccc.rc_producto_crediticio_codigo, rccc.saldo_capital, rccc.saldo_capital_vencido,rccc.saldo_capital_litigio, rcmc.fecha_liquidacion, rcmc.fecha_vencimiento, rccc.fecha_reprecio, rc_estado_pago_codigo, dias_mora, rcmc.capital_original
    from rc_carteras_creditos rccc inner join tc_clientes tccl on tccl.codigo = rccc.rc_credito_tc_cliente_codigo inner join rc_creditos rcmc on rcmc.numero_credito = rccc.rc_credito_numero_credito
    inner join rc_productos_crediticios pc on pc.codigo = rccc.rc_producto_crediticio_codigo where (rccc.fecha_snapshot = '2013-06-30' OR rccc.fecha_snapshot = '2013-07-31' 
    OR rccc.fecha_snapshot = '2013-08-31' OR rccc.fecha_snapshot = '2013-09-30' OR rccc.fecha_snapshot = '2013-10-31' OR rccc.fecha_snapshot = '2013-11-30' OR rccc.fecha_snapshot = '2013-12-31' OR rccc.fecha_snapshot = '2014-01-31'
    OR rccc.fecha_snapshot = '2014-02-28' OR rccc.fecha_snapshot = '2014-03-31' OR rccc.fecha_snapshot = '2014-04-30' OR rccc.fecha_snapshot = '2014-05-31' 
    OR rccc.fecha_snapshot = '2014-06-30' OR rccc.fecha_snapshot = '2014-07-31' OR rccc.fecha_snapshot = '2014-08-31' OR rccc.fecha_snapshot = '2014-09-30'
    OR rccc.fecha_snapshot = '2014-10-31' OR rccc.fecha_snapshot = '2014-11-30' OR rccc.fecha_snapshot = '2014-12-31' OR rccc.fecha_snapshot = '2015-01-31'       
    OR rccc.fecha_snapshot = '2015-02-28' OR rccc.fecha_snapshot = '2015-03-31' OR rccc.fecha_snapshot = '2015-04-30' OR rccc.fecha_snapshot = '2015-05-31' 
    OR rccc.fecha_snapshot = '2015-06-30' OR rccc.fecha_snapshot = '2015-07-31' OR rccc.fecha_snapshot = '2015-08-31' OR rccc.fecha_snapshot = '2015-09-30'
    OR rccc.fecha_snapshot = '2015-10-31' OR rccc.fecha_snapshot = '2015-11-30' OR rccc.fecha_snapshot = '2015-12-31' OR rccc.fecha_snapshot = '2016-01-31'
    OR rccc.fecha_snapshot = '2016-02-29' OR rccc.fecha_snapshot = '2016-03-31' OR rccc.fecha_snapshot = '2016-04-30' OR rccc.fecha_snapshot = '2016-05-31' 
    OR rccc.fecha_snapshot = '2016-06-30' OR rccc.fecha_snapshot = '2016-07-31' OR rccc.fecha_snapshot = '2016-08-31' OR rccc.fecha_snapshot = '2016-09-30' 
    OR rccc.fecha_snapshot = '2016-10-31' OR rccc.fecha_snapshot = '2016-11-30' OR rccc.fecha_snapshot = '2016-12-31' OR rccc.fecha_snapshot = '2017-01-31'
    OR rccc.fecha_snapshot = '2017-02-28' OR rccc.fecha_snapshot = '2017-03-31' OR rccc.fecha_snapshot = '2017-04-30' OR rccc.fecha_snapshot = '2017-05-31'
    OR rccc.fecha_snapshot = '2017-06-30' OR rccc.fecha_snapshot = '2017-07-31' OR rccc.fecha_snapshot = '2017-08-31')")
query1=dbGetQuery(con1,v)
query1=query1[query1$rc_producto_crediticio_codigo!="HK",]
query1=merge(query1,area,by.x="rc_producto_crediticio_codigo",by.y="V1",all.x=TRUE)
fecha_fin=levels(as.factor(query1$fecha_snapshot))
fecha_inicio=as.Date(as.yearmon(fecha_fin))
fecha_inicio[1]=as.Date("2013-06-30")
names=c("indices.csv","saldos.csv","liquidaciones.csv")
out_file1 <- file(names[1], open="a")
out_file2 <- file(names[2],open="a")
out_file3 <- file(names[3],open="a")
earea=levels(as.factor(area$V2))
loop=length(earea)
indice1=saldo1=liq1=list()
indice=saldo=liq=c()
t=0
l=z=h=a=1
while(t<5){
  TT=length(seq(from=8*t+1*h+4*(1-h)*(t-1),to=length(fecha_fin),by=1))
  mindice1=msaldo1=mliq1=matrix(ncol=loop,nrow=TT-1)
  while(a<loop+1){
  if(t<4){
    cosecha=unique(subset(query1,fecha_liquidacion>=as.Date(fecha_inicio[8*t+1*h+4*(1-h)*(t-1)]) & fecha_liquidacion<=as.Date(fecha_fin[19*t+7*h+7*(1-t)*(1-h)]) & V2==earea[a])[,c(7,11,12)])
  }else{
    cosecha=unique(subset(query1,fecha_liquidacion>=as.Date(fecha_inicio[11*t]) & V2==earea[a])[,c(7,11,12)])
    z=0
  }
  i=1
  dt=(7*h+12*(1-h))*z+((1-z)*TT)
  l=z
  j=k=dt+(length(levels(as.factor(query1$fecha_snapshot)))*(1-z))
  while(i<TT){
    muestra=cosecha
    if(i>TT-dt){
      muestra=subset(cosecha,fecha_liquidacion>=as.Date(fecha_inicio[8*t+1*h+4*(1-h)*(t-1)]) & fecha_liquidacion<=as.Date(fecha_fin[length(fecha_fin)-i]))
      j=-i+k
      k=k-1
    }
    muestra=subset(muestra,fecha_vencimiento>=as.Date(fecha_inicio[i+j+(1*h)+((1-h)*(8*t+1*h+4*(1-h)*(t-1))-1)*l-((1-z)*dt)]))
    if(sum(dim(muestra)[1])!=0){
      muestra=subset(muestra,fecha_vencimiento>=as.Date(fecha_inicio[i+j+(1*h)+((1-h)*(8*t+1*h+4*(1-h)*(t-1))-1)*l-((1-z)*dt)]))
      muestra=data.frame(fecha_maduracion=fecha_fin[i+j+(1*h)+((1-h)*(8*t+1*h+4*(1-h)*(t-1))-1)*l-((1-z)*dt)],muestra)
      muestra$fecha_maduracion=as.Date(muestra$fecha_maduracion)
      BD_maduracion=query1[query1$fecha_snapshot %in% muestra$fecha_maduracion,]
      BD_maduracion=BD_maduracion[BD_maduracion$rc_credito_numero_credito %in% muestra$rc_credito_numero_credito,]
      indice[i+j]=sum(BD_maduracion$saldo_capital_vencido,BD_maduracion$saldo_capital_litigio)/sum(BD_maduracion$saldo_capital)
      saldo[i+j]=sum(BD_maduracion$saldo_capital)
      liq[i+j]=sum(BD_maduracion$capital_original)
    }else{
      indice[i+j]=0
      saldo[i+j]=0
      liq[i+j]=0
    }
    i=i+1
  }
  if(TT!=1){
    if(t!=4){
      indice=indice[2:length(indice)]
      saldo=saldo[2:length(saldo)]
      liq=liq[2:length(liq)]
    }else{
      indice=indice[48:length(indice)]
      saldo=saldo[48:length(saldo)]
      liq=liq[49:length(liq)]
    }
    mindice1[,a]=indice  
    msaldo1[,a]=saldo
    mliq1[,a]=liq
    indice=c()
    saldo=c()
    liq=c()
  }
  print(a)
  a=a+1
  }
  if(t==0){
    names(mindice1)=names(msaldo1)=names(mliq1)=earea
  }
  write.table(mindice1,out_file1,sep=";",dec=",",row.names=TRUE,col.names=TRUE)
  write.table(msaldo1,out_file2,sep=";",dec=",",row.names=TRUE,col.names=TRUE)
  write.table(mliq1,out_file3,sep=";",dec=",",row.names=TRUE,col.names=TRUE)
  indice1[[t+1]]=mindice1[t+1,]
  saldo1[[t+1]]=msaldo1[t+1,]
  liq1[[t+1]]=mliq1[t+1,]
  h=0
  a=1
  print(t)
  t=t+1
}
close(out_file1)
close(out_file2)
close(out_file3)
