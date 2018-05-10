impact_calc = function(Lx, Ux, Wx,input){
  
  Rk = matrix(0,nrow = dim(Lx)[1],ncol=2)
  for(i in 1:dim(Rk)[1]){
  lb = Lx[i,]#lower bound on all attributes
  ub = Ux[i,]#higher bound on all attributes
  Rk[i,] = c(Wx%*%lb,Wx%*%ub)
  }
  colnames(Rk) <- c("Lower", "Upper")
  rownames(Rk) <- sapply(1:as.integer(input$numAlt), function(i) input[[paste0("NameA",i)]])
  return(Rk)
}

sens_calc = function(Lx, Ux, Wx, input){
  Rk = matrix(0,nrow = dim(Lx)[1],ncol=2)
  for(i in 1:dim(Rk)[1]){
    lb = Lx[i,]#lower bound on all attributes
    ub = Ux[i,]#higher bound on all attributes
    Rk[i,] = c(Wx%*%lb,Wx%*%ub)
  }
  D = Rk[,2] - Rk[,1]
  SA = matrix(0, nrow = dim(Lx)[1], ncol = dim(Lx)[2])
  for(i in 1:dim(Rk)[1]){
  SA[i,]=(Ux[i,]-Lx[i,])*Wx / D
  }
  
  crit.namn <- sapply(1:as.integer(input$numC), function(i) input[[paste0("NameC",i)]])
  
  colnames(SA) = crit.namn 
  rownames(SA) = rownames(Lx)
  return(SA)
}
