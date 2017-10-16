#"C:/Users/alabarca/Desktop"
#"Areas cod. productos.txt"
#"lineas.csv"
#v=c("select * from rc_lineas_creditos")
#lineas=dbGetQuery(con1,v)
#EAD=function(gamma,ruta1,ruta2,consulta,ruta3){
# EAD por cliente
setwd("C:/Users/igrau/Desktop")
g=0.6250#gamm
#areas<- read.delim("Areas cod. productos.txt", header=FALSE, stringsAsFactors=FALSE)
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
v=paste("select * from rc_carteras_creditos
    where fecha_snapshot = ","'","2017-08-31","'",sep=" ")
comercial=dbGetQuery(con1,v)
v=paste("select * from rc_tarjetas_creditos
    where fecha_snapshot = ","'","2017-08-31","'",sep=" ")
tdc=dbGetQuery(con1,v)
v=paste("select * from rc_creditos")
mc=dbGetQuery(con1,v)
mc_creditos=mc[mc$numero_credito %in% comercial$rc_credito_numero_credito & mc$tc_cliente_codigo %in% comercial$rc_credito_tc_cliente_codigo,]  
A=data.frame(table(mc_creditos$numero_credito))
duplicated=as.character(as.vector((A[A$Freq==2,1])))
credit_temp=comercial[comercial$rc_credito_numero_credito %in% duplicated,3]
loop=length(credit_temp)
i=1
while(i<loop+1){
  mc_creditos=mc_creditos[-which(mc_creditos$numero_credito==duplicated[i] & mc_creditos$tc_cliente_codigo!=credit_temp[i]),]
  i=i+1
}
comercial=comercial[order(comercial$rc_credito_numero_credito),]
mc_creditos=mc_creditos[order(mc_creditos$numero_credito),]
comercial["codigo_linea"]=mc_creditos[mc_creditos$numero_credito %in% comercial$rc_credito_numero_credito,19]
lineas<- read.csv("lineas ago 2017.csv", sep=";", colClasses="character")
lineas$monto=as.numeric(lineas$monto)
lineas=subset(lineas,fecha_vencimiento>="2017-08-31" & rc_estado_credito_codigo=="1")
comercial[!comercial$codigo_linea %in% lineas$codigo,35]="0"
code1=levels(as.factor(comercial$rc_credito_tc_cliente_codigo))
code2=levels(as.factor(tdc$rc_tarjeta_tc_cliente_codigo))
code=c(code1,code2)
code=levels(as.factor(code))
code=data.frame(codigo=code,stringsAsFactors=FALSE)
loop=dim(code)[1]
i=1
bloque=as.data.frame(matrix(ncol=8,nrow=loop))
#ll=levels(as.factor(areas$V2))
#lll=levels(as.factor(areas$V3))
names(bloque)=c("codigo","saldo_tdc","limite_tdc","saldo_cartera_comercial","linea_cupo_comercial","saldo_total","linea_cupo_total","EAD")#,ll,lll)
while(i<loop+1){
  suma1=subset(tdc,rc_tarjeta_tc_cliente_codigo=="001000421")[,c(19,25)]
  suma2=subset(comercial,rc_credito_tc_cliente_codigo=="001000421")[,c(5,31,35)]
  suma3=subset(lineas,tc_cliente_codigo=="001000421")[,4]
  x=c(sum(suma1$saldo_capital),sum(suma1$limite_credito))
  y=sum(suma2$saldo_capital)
  z=sum(suma3)
  h=sum(suma2[suma2$codigo_linea!="0",1])
  bloque[i,c(1:7)]=c(code[i,1],x[1],x[2],y[1],z[1],x[1]+y[1],x[2]+z[1])
  if(y[1]!=0 & h[1]==0 & x[1]==0){
    bloque[i,8]=y[1]
  }else{
    if(y[1]!=0 & h[1]==0 & x[1]!=0){
      bloque[i,8]=x[1]+y[1]+(g*max(x[2]-x[1],0))
    }else{
      if(y[1]!=0 & h[1]!=0 & x[1]==0){
        if(h[1]<=z[1]){
        bloque[i,8]=y[1]+(g*(z[1]-h[1]))
        }else{
          bloque[i,8]="validar_linea_disponible"
        }
      }else{
        if(y[1]!=0 & h[1]!=0 & x[1]!=0){
          if(h[1]<=z[1]){
          bloque[i,8]=y[1]+x[1]+(g*(z[1]-h[1]+max(x[2]-x[1],0)))
          }else{
            bloque[i,8]="validar_linea_disponible"
          }
        }else{
          if(y[1]==0 & h[1]!=0 & x[1]!=0){
            if(h[1]<=z[1]){
            bloque[i,8]=x[1]+(g*(z[1]-h[1]+max(x[2]-x[1],0)))
            }else{
              bloque[i,8]="validar_linea_disponible"
            }
          }else{
            bloque[i,8]=x[1]+(g*max(x[2]-x[1],0))
          }
        }
      }
    }
  }
  #if(dim(suma2)[1]!=0){
   # temp=merge(suma2,areas,by.x="rc_producto_crediticio_codigo",by.y="V1",all.x=TRUE)[,-2]
    #temp1=table(temp$V2)
    #temp2=table(temp$V3)
    #l=length(temp1)
    #L=length(temp2)
    #j=1
    #while(j<l+1){
     #bloque[i,which(names(temp1)[j]==names(bloque))]=temp1[j]*bloque[i,8]/sum(temp1)
     #j=j+1
    #}
    #j=1
    #while(j<L+1){
     # bloque[i,which(names(temp2)[j]==names(bloque))]=temp2[j]*bloque[i,8]/sum(temp1)
      #j=j+1
    #}
  #}
  print(i)
  i=i+1
}

#bloque=as.data.frame(bloque)
#bloque[is.na(bloque)] <- 0
#bloque[,2]=as.numeric(as.vector(bloque[,2]))
#bloque[,3]=as.numeric(as.vector(bloque[,3]))
#bloque[,4]=as.numeric(as.vector(bloque[,4]))
#bloque[,5]=as.numeric(as.vector(bloque[,5]))
#bloque[,6]=as.numeric(as.vector(bloque[,6]))
#bloque[,7]=as.numeric(as.vector(bloque[,7]))
#bloque[,8]=as.numeric(as.vector(bloque[,8]))
#l=levels(as.factor(areas$V2))
#loop=length(l)
#i=1
#c_area=matrix(ncol=2,nrow=loop)
#c_area[,1]=l
#while(i<loop+1){
 # c_area[i,2]=sum(subset(bloque,names(bloque)[which(names(bloque)==l[i])]!=0)[,which(names(bloque)==l[i])])
  #i=i+1
#}
#l=levels(as.factor(areas$V3))
#loop=length(l)
#i=1
#c_basilea=matrix(ncol=2,nrow=loop)
#c_basilea[,1]=l
#while(i<loop+1){
 # c_basilea[i,2]=sum(subset(bloque,names(bloque)[which(names(bloque)==l[i])]!=0)[,which(names(bloque)==l[i])])
#  i=i+1
#}

write.table(bloque,file="EAD_cliente.csv",sep=";",row.names=FALSE)
#write.table(c_area,file="EAD_area.csv",sep=";",dec=".",row.names=FALSE)
#write.table(c_basilea,file="EAD_basilea.csv",sep=";",dec=".",row.names=FALSE)
#EAD(0.75,"C:/Users/jmarquina/Desktop","Areas cod. productos.txt","2017-03-31","lineas 1.csv")
