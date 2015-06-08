
pertenece = function(item, vector)
{
  i = 1
  encontrado = FALSE
  
  while((!encontrado) && (i<length(vector)+1))
  {
    if(vector[i]==item)
    {
      encontrado = TRUE
    }
    i = i+1
  }
  
  return(encontrado)
}

comprarSobreConRepe = function()
{
  coleccion<-1:639
  sobre = sample(coleccion,size=5,replace=T)
  sobre
}

comprarSobreSinRepe = function(v)
{
  coleccion<-1:639
  sobre = sample(coleccion,size=5,replace=F)
  sobre
}

comprarSobreSinRepeDificiles = function()
{
  coleccion<-1:639
  probas<-c(rep(1/2000,10),rep((1-(10/2000))/629,629))
  sobre = sample(coleccion,size=5,replace=F,prob=probas)
  sobre
}


ejercicioA = function(repeticiones)
{  
  acumSobres = 0

  for( i in 1:repeticiones)
  {
  
  aTachar<-1:639

  cantidadSobres = 0

    while(length(aTachar)!=0)
    {
      cantidadSobres = cantidadSobres+1
  
      sobre = comprarSobreConRepe()
  
      for(i in 1:5)
      {
        aTachar <- aTachar[which(aTachar!=sobre[i])] 
      }
  
    }
    acumSobres = acumSobres + cantidadSobres
  }
  acumSobres/repeticiones
}


ejercicioB = function(repeticiones)
{  
  acumSobres = 0
  
  for( i in 1:repeticiones)
  {
    
    aTachar<-1:639
    
    cantidadSobres = 0
    
    while(length(aTachar)!=0)
    {
      cantidadSobres = cantidadSobres+1
      
      sobre = comprarSobreSinRepe()
      
      for(i in 1:5)
      {
        aTachar <- aTachar[which(aTachar!=sobre[i])] 
      }
      
    }
    acumSobres = acumSobres + cantidadSobres
  }
  acumSobres/repeticiones
}


ejercicioC = function(repeticiones)
{  
  acumSobres = 0
  
  for( i in 1:repeticiones)
  {
    
    aTachar<-1:639
    
      
    cantidadSobres = 0
    
    while(length(aTachar)!=0)
    {
      cantidadSobres = cantidadSobres+1
      
      sobre = comprarSobreSinRepeDificiles()
      
      for(i in 1:5)
      {
        aTachar <- aTachar[which(aTachar!=sobre[i])] 
      }
      
    }
    acumSobres = acumSobres + cantidadSobres
  }
  acumSobres/repeticiones
}

ejercicioD = function(repeticiones)
{
  acumSobresPedro = 0
  acumSobresGaston = 0
  
  for( i in 1:repeticiones)
  {
    aTacharPedro<-1:639
    aTacharGaston<-1:639
    cantSobresPedro = 0
    cantSobresGaston = 0
    
    
    while((length(aTacharPedro)!=0)&&(length(aTacharGaston)!=0))
    {
      #a los dos les faltan
      sobrePedro  = comprarSobreSinRepe()
      sobreGaston = comprarSobreSinRepe()
      cantSobresPedro = cantSobresPedro+1
      cantSobresGaston = cantSobresGaston+1
      pilonPedro <-vector()
      pilonGaston <-vector()
      
      for(i in 1:5)#loop de pegar figuritas
      {
        if(pertenece(sobrePedro[i],aTacharPedro))
        {
          aTacharPedro <- aTacharPedro[which(aTacharPedro!=sobrePedro[i])]
          c(sobrePedro[i], pilonPedro)
        }
        
        if(pertenece(sobreGaston[i],aTacharGaston))
        {
          aTacharGaston <- aTacharGaston[which(aTacharGaston!=sobreGaston[i])]
          c(sobreGaston[i], pilonGaston)
        }
      }
      
      for(i in 1:length(pilonGaston))#loop pegar pilones
      {
        aTacharPedro <- aTacharPedro[which(aTacharPedro!=pilonGaston[i])]
      }
      
      for(i in 1:length(pilonPedro))#loop pegar pilones
      {
        aTacharGaston <- aTacharGaston[which(aTacharGaston!=pilonPedro[i])]
      }
      
    }
    
    while((length(aTacharPedro)!=0))#le falta a pedro
    {
      sobrePedro  = comprarSobreSinRepe()
      cantSobresPedro = cantSobresPedro+1
      for(i in 1:5)
      {
        aTacharPedro <- aTacharPedro[which(aTacharPedro!=sobrePedro[i])] 
      }
      
    }
    
    while((length(aTacharGaston)!=0))#le falta a gaston
    {
      sobreGaston = comprarSobreSinRepe()
      cantSobresGaston = cantSobresGaston+1
      for(i in 1:5)
      {
        aTacharGaston <- aTacharGaston[which(aTacharGaston!=sobreGaston[i])] 
      }
    }
    acumSobresGaston = acumSobresGaston + cantSobresGaston
    acumSobresPedro = acumSobresPedro + cantSobresPedro
  }
  print(acumSobresGaston/repeticiones)
  print(acumSobresPedro/repeticiones)
}

