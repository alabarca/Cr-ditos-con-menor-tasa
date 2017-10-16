maestro=function(fecha_inicio,fecha_fin,fecha_garantia,ruta){    
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
    v=paste("select tccl.tc_tipo_persona_codigo, tccl.numero_documento,  tccl.nombre, rccc.rc_credito_numero_credito, rcmc.fecha_liquidacion, rcmc.fecha_vencimiento, rccc.rc_credito_tc_cliente_codigo, rccc.saldo_capital, rccc.tasa_interes, rccc.dias_mora, rccc.monto_provision_especifica, rccc.rc_producto_crediticio_codigo, rcmc.rc_linea_credito_codigo
    from rc_carteras_creditos rccc 
    inner join rc_creditos rcmc on rcmc.numero_credito = rccc.rc_credito_numero_credito
    inner join tc_clientes tccl on tccl.codigo = rccc.rc_credito_tc_cliente_codigo 
    where rccc.fecha_snapshot = '",fecha_fin,"';",sep="")
    v="select * from tc_agencias"
    q=dbGetQuery(con1,v)
    v="select * from rc_garantias"
    qq=dbGetQuery(con1,v)
    v=paste("select *from rc_creditos_rc_garantias
          where fecha_snapshot= '",fecha_garantia,"'",sep="")
    qqq=dbGetQuery(con1,v)
    v="select * from rc_tipos_garantias"
    qqqq=dbGetQuery(con1,v)
    loop=dim(qqqq)[1]
    j=1
    while(j<3){
      if(j==2){
        q=q[q$fecha_liquidacion>=fecha_inicio & q$fecha_liquidacion<=fecha_fin,]
      }
      credit=q$rc_credito_numero_credito
      #garantias asociadas a líneas
      credit_lineas=q[q$rc_linea_credito_codigo!=0,]
      id_lineas=substr(credit_lineas$rc_linea_credito_codigo, unique(nchar(credit_lineas$rc_linea_credito_codigo))-4,unique(nchar(credit_lineas$rc_linea_credito_codigo)))
      credit_lineas["id_lineas"] <- id_lineas

      garantia_lineas=qq[qq$tc_cliente_codigo %in% credit_lineas$rc_credito_tc_cliente_codigo,]
      id_lineas=substr(as.numeric(garantia_lineas$codigo),nchar(as.numeric(garantia_lineas$tc_cliente_codigo))+1,nchar(as.numeric(garantia_lineas$tc_cliente_codigo))+5)
      garantia_lineas["id_lineas"] <- id_lineas

      garantia_lineas_temp=merge(x=credit_lineas,y=garantia_lineas,by.x="rc_credito_tc_cliente_codigo",by.y="tc_cliente_codigo")
      garantia_lineas_temp_temp=garantia_lineas_temp[garantia_lineas_temp$id_lineas.x==garantia_lineas_temp$id_lineas.y & garantia_lineas_temp$rc_situacion_garantia_codigo==1,]

      garantia_lineas_temp_temp_temp=garantia_lineas_temp_temp[!duplicated(garantia_lineas_temp_temp[,-c(5:12,26)]),][,-c(5:12,26)]

      #garantías asociadas a créditos

      garantia_credit=qq[qq$codigo %in% qqq$rc_garantia_codigo & qq$rc_situacion_garantia_codigo!=2,]
      garantia_credit_temp=merge(x=garantia_credit,y=qqq,by.x="codigo",by.y="rc_garantia_codigo",all.x=TRUE)
      garantia_credit_temp_temp=garantia_credit_temp[garantia_credit_temp$rc_credito_numero_credito %in% q$rc_credito_numero_credito,]
      #Consolidado por código de garantía
      i=1
      tipo_credit=tipo_linea=c()
      while(i<loop+1){
        tipo_linea[i]=sum(subset(garantia_lineas_temp_temp_temp,rc_tipo_garantia_codigo==qqqq$codigo[i] & tc_moneda_codigo!="USD")$monto_garantia)
        tipo_credit[i]=sum(subset(garantia_credit_temp_temp,rc_tipo_garantia_codigo==qqqq$codigo[i] & tc_moneda_codigo!="USD")$monto_garantia)
        i=i+1
      }
      qqqq["lineas/cupos"] <- tipo_linea
      qqqq["credit"] <- tipo_credit
      #USD=sum(garantia_lineas_temp_temp_temp[garantia_lineas_temp_temp_temp$tc_moneda_codigo=="USD",]$monto_garantia)+
      #sum(garantia_credit_temp_temp[garantia_credit_temp_temp$tc_moneda_codigo=="USD",]$monto_garantia)
      #print(c(USD, j))
      agrupa=qqqq[,-c(1,2,4,5,6)]
      qqqq=qqqq[,-c(16,17)]

##en moneda extranjera
      #listado garantia moneda extranjera
      #garantia_extranjera=qq[qq$tc_moneda_codigo!="VEB" & qq$rc_situacion_garantia_codigo!=2,]
      #id_lineas=substr(garantia_extranjera$codigo, unique(nchar(garantia_extranjera$codigo))-4,unique(nchar(garantia_extranjera$codigo)))
      #garantia_extranjera["id_lineas"] <- id_lineas
      
      #Detalle créditos sin garantia
      excluir=unique(c(garantia_lineas_temp_temp$rc_credito_numero_credito,garantia_credit_temp_temp$rc_credito_numero_credito))
      sin_garantia=q[!q$rc_credito_numero_credito %in% excluir,]
      
      if(j==1){
        write.table(agrupa,file=paste(ruta,"total_cartera.csv",sep=""),sep=";",dec=",")
        write.table(sin_garantia,file=paste(ruta,"sin_garantia_total_cartera.csv",sep=""),sep=";",dec=",")
      }else{
        write.table(agrupa,file=paste(ruta,"parcial.csv",sep=""),sep=";",dec=",")
        write.table(sin_garantia,file=paste(ruta,"sin_garantia_parcial.csv",sep=""),sep=";",dec=",")
      }
      
      
    j=j+1
    }
}
maestro("2017-07-03","2017-07-28","2017-07-30","C:/Users/alabarca/Desktop/")
