funcion2=function(ruta1,ruta2,salida){
  setwd(salida)
  CC <- read.delim(ruta1, sep=";",quote="\"",row.names=NULL,na="++")
  CC[,7]=as.numeric(as.vector(CC[,7]))
  CC[,4]=as.character(CC[,4])
  CC=data.frame(CC[order(CC[,3]),],row.names=NULL)
  CCC=data.frame(subset(CC,rc_producto_crediticio_codigo=="CA" | rc_producto_crediticio_codigo=="GD"|rc_producto_crediticio_codigo=="L3"|rc_producto_crediticio_codigo=="LI"|rc_producto_crediticio_codigo=="GD"|rc_producto_crediticio_codigo=="PM"|rc_producto_crediticio_codigo=="PL" ),row.names=NULL)
  CC3<-read.delim(ruta2,sep=";")
  #CC3=data.frame(CC3[-1,],row.names=NULL)
  CC3=data.frame(subset(CC3,rc_producto_crediticio_codigo=="CA" | rc_producto_crediticio_codigo=="GD"|rc_producto_crediticio_codigo=="L3"|rc_producto_crediticio_codigo=="LI"|rc_producto_crediticio_codigo=="PM"|rc_producto_crediticio_codigo=="PL" ),row.names=NULL)
  CC3[,4]=as.character(CC3[,4])
  CC3=data.frame(CC3[order(CC3[,3]),],row.names=NULL)
  CC1=data.frame(CCC,row.names=NULL)
    CC2=as.matrix(data.frame(CC1,"Saldo 27-02"=rep("-",length.out=as.numeric(dim(CC1)[1])),
                             "Tasa 27-02"=rep("-",length.out=as.numeric(dim(CC1)[1])),"Variación"=rep("-",length.out=as.numeric(dim(CC1)[1]))))
    k=1
    t=1
    CC3=as.matrix(CC3)
    while(k<as.numeric(dim(CC2)[1])+1){
      while(t<as.numeric(dim(CC3)[1]+1)){
        if(CC2[k,4]==CC3[t,4]){
          CC2[k,9]=CC3[t,6]
          CC2[k,10]=CC3[t,7]
          k=k+1
          t=as.numeric(dim(CC3)[1])+1
        }else{
          t=t+1
          if(t==as.numeric(dim(CC3)[1])+1){
            k=k+1
          }
        }    
      }
      t=1
    }
    CC2=data.frame(CC2)
    CC2[,7]=as.numeric(as.vector(CC2[,7]))
    A=as.vector(CC2[,10])
    CC2[,11]=as.vector(CC2[,11])
    while(t<as.numeric(dim(CC2)[1])+1){
      if(CC2[t,9]=="-"){
        CC2[t,10]="-"
      }else{
        CC2[t,11]=as.character(round(CC2[t,7]-as.numeric(A[t]),4))
      }      
      t=t+1
    }
    CC2=data.frame(CC2,Prox_pago=CC2[,8])
    CC2=CC2[,-8]
    CC2=data.frame(Tipo_P=CC2[,1],N_Doc=CC2[,2],N_credito=CC2[,4],
                   Cod_Producto=CC2[,5],Nombre=CC2[,3],Saldo_actual=CC2[,6],
                   Tasa_actual=CC2[,7],Saldo_anterior=CC2[,8],Tasa_anterior=CC2[,9],Variación=CC2[,10],Prox_pago=CC2[,11])
    
  write.table(CC2,file="Creditos_Menor_Tasa_TOTAL.csv",dec = ".",sep=";", row.names=FALSE,quote=TRUE)
  CC2=data.frame(subset(CC2, Cod_Producto != "PM"),row.names=NULL)
  CC2[,6]=data.frame(as.numeric(as.vector(CC2[,6])))
  CC2=data.frame(as.matrix(data.frame(subset(CC2, Saldo_actual > 5000000))))
  #CC1=data.frame(subset(CC1, rc_producto_crediticio_codigo != "PM"),row.names=NULL)
  #CC3=data.frame(subset(CC3, saldo_capital > 5000000))
  #CC3=data.frame(subset(CC3, rc_producto_crediticio_codigo != "PM"),row.names=NULL)
  write.table(CC2,file="Creditos_Menor_Tasa.csv",dec = ".",sep=";", row.names=FALSE,quote=TRUE)  
  CCC=data.frame(subset(CC,rc_producto_crediticio_codigo=="AG" | rc_producto_crediticio_codigo=="AR"|rc_producto_crediticio_codigo=="AS"|rc_producto_crediticio_codigo=="AV"|rc_producto_crediticio_codigo=="GA"|rc_producto_crediticio_codigo=="RB"|rc_producto_crediticio_codigo=="RC"|rc_producto_crediticio_codigo=="RE"|rc_producto_crediticio_codigo=="RS"|rc_producto_crediticio_codigo=="RV"|rc_producto_crediticio_codigo=="S4"),row.names=NULL)
  CC2=data.frame(as.matrix(data.frame(Tipo_P=CCC[,1],N_Doc=CCC[,2],N_credito=CCC[,4],
                 Cod_Producto=CCC[,5],Nombre=CCC[,3],Saldo=CCC[,6],
                 Tasa=CCC[,7],Próximo_pago=CCC[,8],row.names=NULL)))
  write.table(CC2,file="Créditos_Menor_Tasa_agrícola.csv",dec = ".",sep=";", row.names=FALSE,quote=TRUE) 
  CCC[,7]=as.numeric(as.vector(CCC[,7]))
  CCC=data.frame(subset(CCC,tasa_interes<13))
  CC2=data.frame(as.matrix(data.frame(Tipo_P=CCC[,1],N_Doc=CCC[,2],N_credito=CCC[,4],
                 Cod_Producto=CCC[,5],Nombre=CCC[,3],Saldo=CCC[,6],
                 Tasa=CCC[,7],Próximo_pago=CCC[,8],row.names=NULL)))
  write.table(CC2,file="Consulta_agrícola.csv",dec = ".",sep=";", row.names=FALSE,quote=TRUE) 
  CCC=data.frame(subset(CC,rc_producto_crediticio_codigo=="GF" | rc_producto_crediticio_codigo=="S2"|rc_producto_crediticio_codigo=="NA"|rc_producto_crediticio_codigo=="JN"|rc_producto_crediticio_codigo=="NL"),row.names=NULL)
  CCC[,7]=as.numeric(as.vector(CCC[,7]))
  CCC=data.frame(subset(CCC,tasa_interes<16.2))
  CC2=data.frame(as.matrix(data.frame(Tipo_P=CCC[,1],N_Doc=CCC[,2],N_credito=CCC[,4],
                 Cod_Producto=CCC[,5],Nombre=CCC[,3],Saldo=CCC[,6],
                 Tasa=CCC[,7],Próximo_pago=CCC[,8],row.names=NULL)))
  write.table(CC2,file="Consulta_manufactura.csv",dec = ".",sep=";", row.names=FALSE,quote=TRUE) 

#Ajuste de tasas
t=1
while(t<4){
  if(t==1){
  CC4=data.frame(as.matrix(subset(CC,rc_producto_crediticio_codigo=="CA" | rc_producto_crediticio_codigo=="GD"|rc_producto_crediticio_codigo=="L3"|rc_producto_crediticio_codigo=="LI"|rc_producto_crediticio_codigo=="GD"|rc_producto_crediticio_codigo=="PL" )))
  }else{
    if(t==2){
      CC4=data.frame(as.matrix(subset(CC,rc_producto_crediticio_codigo=="AG" | rc_producto_crediticio_codigo=="AR"|rc_producto_crediticio_codigo=="AS"|rc_producto_crediticio_codigo=="AV"|rc_producto_crediticio_codigo=="GA"|rc_producto_crediticio_codigo=="RB"|rc_producto_crediticio_codigo=="RC"|rc_producto_crediticio_codigo=="RE"|rc_producto_crediticio_codigo=="RS"|rc_producto_crediticio_codigo=="RV"|rc_producto_crediticio_codigo=="S4")),row.names=NULL)
    }else{
      CC4=data.frame(as.matrix(subset(CC,rc_producto_crediticio_codigo=="GF" | rc_producto_crediticio_codigo=="S2"|rc_producto_crediticio_codigo=="NA"|rc_producto_crediticio_codigo=="JN"|rc_producto_crediticio_codigo=="NL")),row.names=NULL)
    }
  }
  CC8=data.frame(CC4[order(CC4[,2]),])
  etiq=levels(CC8$numero_documento)
  tasa_promedio=c()
  tasa_ajuste=c()
  i=1
  j=1
  k=1
  while(i<length(etiq)+1){
  cont=as.numeric(as.vector(subset(CC8,numero_documento==etiq[i])[,7]))
  cont1=rep(max(cont),length.out=length(cont))
  cont=rep(mean(cont),length.out=length(cont))    
    while(j<length(cont)+1){
      tasa_promedio[k]=round(cont[j],2)
      tasa_ajuste[k]=round(cont1[j],2)
      j=j+1
      k=k+1
      }
    j=1
    i=i+1
    }
  A=data.frame(CC8,tasa_promedio,tasa_ajuste)
  A[,7]=as.numeric(as.vector(A[,7]))
  tasa_igual=c()
  while(j<as.numeric(dim(A)[1])+1){
    if(A[j,7]==tasa_promedio[j]){
      tasa_igual[j]="SI"
    }else{
      tasa_igual[j]="NO"
    }
    j=j+1
  }
  CC5=data.frame(as.matrix(data.frame(A,tasa_igual)))
  C=data.frame(Tipo_P=CC5[,1],N_Doc=CC5[,2],N_credito=CC5[,4],
               Cod_Producto=CC5[,5],Nombre=CC5[,3],Saldo=CC5[,6],
               Tasa=CC5[,7],Tasa_Promedio=CC5[,9],Tasa_Igual=CC5[,11],Próximo_pago=CC5[,8],row.names=NULL)
  c=data.frame(subset(CC5,tasa_igual=="NO"),row.names=NULL)
  IA=c()
  IF=c()
  i=1
  c[,6]=as.numeric(as.vector(c[,6]))
  c[,7]=as.numeric(as.vector(c[,7]))
  c[,10]=as.numeric(as.vector(c[,10]))
  while(i<as.numeric(dim(c)[1])+1){
    IA[i]=(c[i,6]*c[i,7])/100
    IF[i]=(c[i,6]*c[i,10])/100
    i=i+1
    }
  C=data.frame(as.matrix(data.frame(C[order(C[,5]),],row.names=NULL)))
  c=data.frame(c,V1=IA,V2=IF,row.names=NULL)
  c=data.frame(as.matrix(data.frame(Tipo_P=c[,1],N_Doc=c[,2],N_credito=c[,4],
               Cod_Producto=c[,5],Nombre=c[,3],Saldo=c[,6],
               Tasa=c[,7],Ingreso_actual=c[,12],Tasa_Promedio=c[,9],Tasa_Igual=c[,11],Ajuste_tasa=c[,10],
               Ingreso_final=c[,13],Próximo_pago=c[,8],row.names=NULL)))
  c=data.frame(as.matrix(data.frame(c[order(c[,5]),],row.names=NULL)))
  if(t==1){
  write.table(C,file='Consulta_cliente_Cartera_Comercial_TOTAL.csv',dec = ".",sep=";", row.names=FALSE)
  write.table(c,file='Ajuste_Tasa_Cartera_Comercial_Total.csv',dec = ".",sep=";", row.names=FALSE)
  }else{
    if(t==2){
    write.table(C,file='Consulta_cliente_Cartera_Agrícola.csv',dec = ".",sep=";", row.names=FALSE)
    write.table(c,file='Ajuste_Tasa_Cartera_Agrícola.csv',dec = ".",sep=";", row.names=FALSE)
    }else{
      write.table(C,file='Consulta_cliente_Cartera_Manufactura.csv',dec = ".",sep=";", row.names=FALSE)
      write.table(c,file='Ajuste_Tasa_Cartera_Manufactura.csv',dec = ".",sep=";", row.names=FALSE)
    }   
  }
t=t+1
}
return("Cálculos realizados, guardados en la ruta indicada por el usuario")
}