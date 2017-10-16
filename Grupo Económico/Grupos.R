#install.packages("gtools")
#install.packages("iterpc")
#library(gtools)
library(iterpc)
library(readr)
Grupos_R <- read_delim("C:/Users/alabarca/Desktop/Grupos económicos/Grupos R.csv", 
                       ";", escape_double = FALSE)
suma=data.frame(sum=apply(Grupos_R[,2:dim(Grupos_R)[2]],1,sum))
Grupos_R=data.frame(Grupos_R,sum=suma)
Grupos_R=subset(Grupos_R,sum!=0)
row.names(Grupos_R)=as.character(c(1:dim(Grupos_R)[1]))
empresas=empresas_temp=levels(as.factor(Grupos_R$Empresas))
loop=length(empresas_temp)
i=k=z=1
grupo=list()
cero=function(x){
  y=sum(x!=0)
  return(y)
}
while(loop>0){
  grupo_temp=subset(Grupos_R,Empresas==empresas_temp[i])
  col_which=which(grupo_temp[2:1374]>0)
  col_names=colnames(grupo_temp[2:1374])[col_which]
  loop1=j=length(col_names)
  grupo[[k]]=data.frame(group=c(grupo_temp[1,1],col_names))
  #names(grupo[[k]])[1]=paste("Grupo ",col_names,sep="")
  empresas_temp=empresas_temp[-i]
  row_which=Grupos_R$Empresas %in% empresas_temp
  while(j>1){
    #p=permutations(loop1,j,v=col_which,repeats.allowed=FALSE)
    #p=apply(p,1,sort)
    #p=as.matrix(p)
    #p=t(p)
    #p=unique(p)
    I=iterpc(table(col_which),j)
    p=getall(I)
    grupo_p_temp_sum=grupo_p_temp_include=list()
      if(j==loop1){
        grupo_p_temp_sum[[z]]=as.numeric(names(which(apply(Grupos_R[row_which,p[z,]+1],1,sum)>0.2)))
        grupo_p_temp_include[[z]]=as.numeric(names(which(apply(Grupos_R[row_which,p[z,]+1],1,cero)==length(p[z,]))))
        if(length(grupo_p_temp_sum[[z]])!=0 & length(grupo_p_temp_include[[z]])!=0){
          vec_temp=c(grupo_p_temp_sum[[z]],grupo_p_temp_include[[z]])
          intersecc=vec_temp[duplicated(vec_temp)]
          temp=data.frame(group=Grupos_R[intersecc,1])
          grupo[[k]]=rbind(grupo[[k]],temp)
          empresas_temp=empresas_temp[which(!as.character(temp$group)==empresas_temp)]
          row_which=Grupos_R$Empresas %in% empresas_temp
        }
      }
      if(j!=loop1){
        grupo_p_temp_sum=grupo_p_temp_include=list()
        loop2=dim(p)[1]
        while(z<loop2+1){
          grupo_p_temp_sum[[z]]=as.numeric(names(which(apply(Grupos_R[row_which,p[z,]+1],1,sum)>0.2)))
          grupo_p_temp_include[[z]]=as.numeric(names(which(apply(Grupos_R[row_which,p[z,]+1],1,cero)==length(p[z,]))))
          if(length(grupo_p_temp_sum[[z]])!=0 & length(grupo_p_temp_include[[z]])!=0 & sum(duplicated(c(grupo_p_temp_sum[[z]],grupo_p_temp_include[[z]])))!=0){
            k=k+1
            vec_temp=c(grupo_p_temp_sum[[z]],grupo_p_temp_include[[z]])
            intersecc=vec_temp[duplicated(vec_temp)]
            
            grupo[[k]]=data.frame(file=c(Grupos_R[intersecc,1],colnames(grupo_temp[2:1374])[p[z,]]))
            empresas_temp=empresas_temp[which(!Grupos_R[intersecc,1]==empresas_temp)]
            row_which=Grupos_R$Empresas %in% empresas_temp
          }
          z=z+1
        }
      z=1
      }
    j=j-1
  }
  k=k+1
  loop=length(empresas_temp)
  print(loop)
}
setwd("C://users//alabarca//Desktop")
out_file <- file("grupos.csv", open="a")    
i=1
loop5=length(grupo)
while(loop5+1){
  write.table(grupo[[i]],out_file,sep=";",dec=",",row.names=TRUE,col.names=TRUE)
  print(i)
  i=i+1
}
close(out_file)
