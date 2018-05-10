createmat <- function(input){
  lowmat = round(runif(as.integer(input$numC)*as.integer(input$numAlt))*10,0)
  highmat = lowmat
  for(i in 1:length(lowmat)){
    highmat[i] = round(lowmat[i]  + runif(1)*(10-lowmat[i]),0)
  }
  highmat[highmat > 10 ] = 10
  mat = matrix(c(matrix(rbind(lowmat,highmat),byrow = TRUE)),
               nrow = as.integer(input$numAlt),ncol = as.integer( input$numC)*2,byrow = TRUE)
  
  #mat = t(matrix(rep(rep(0,1, numC), numAlt),as.integer( numC)*2,as.integer( numAlt)))
  #mat = matrix(0, as.integer( numAlt), as.integer( numC)*2)
  rownames(mat) <- sapply(1:as.integer( input$numAlt), function(i) input[[paste0("NameA",i)]])
  colv1 <- c(rep("C",as.integer( input$numC)))
  colv2 <- c(rep("C",as.integer( input$numC)))
  
  for(j in 1:as.integer( input$numC)){
    colv1[j] <- paste0(input[[paste0("NameC",j)]]," lower ")
    colv2[j] <- paste0(input[[paste0("NameC",j)]], " upper ")
  }
  
  clnam <- rbind(colv1, colv2)
  colnames(mat) <- c(clnam)

  return(list(mat=mat))
}

createmat_k <- function(input,k){
  lowmat = round(runif(as.integer(input$numAlt))*10,0)
  highmat = lowmat
  for(i in 1:length(lowmat)){
    highmat[i] = round(lowmat[i]  + runif(1)*(10-lowmat[i]),0)
  }
  highmat[highmat > 10 ] = 10
  mat = matrix(c(matrix(rbind(lowmat,highmat),byrow = TRUE)),
               nrow = as.integer(input$numAlt),ncol = 2,byrow = TRUE)

  if(sum(mat[,1]-mat[,2])==0){
    mat[1,]=c(2,3)
  }
  
  rownames(mat) <- sapply(1:as.integer( input$numAlt), function(i) input[[paste0("NameA",i)]])
  colv1 <- paste0(input[[paste0("NameC",k)]]," lower value ")
  colv2 <- paste0(input[[paste0("NameC",k)]], " upper value ")
  
  clnam <- rbind(colv1, colv2)
  colnames(mat) <- c(clnam)
  
  return(list(mat=mat))
}

createmat_nointervals <- function(input){
  lowmat = round(runif(as.integer(input$numC)*as.integer(input$numAlt))*10,0)
  mat = matrix(lowmat,byrow = TRUE,
               nrow = as.integer(input$numAlt),ncol = as.integer( input$numC))
  
  rownames(mat) <- sapply(1:as.integer( input$numAlt), function(i) input[[paste0("NameA",i)]])
  colv1 <- c(rep("C",as.integer( input$numC)))
   for(j in 1:as.integer( input$numC)){
    colv1[j] <- paste0(input[[paste0("NameC",j)]])
  }
  
  clnam <- colv1
  colnames(mat) <- c(clnam)
  
  return(list(mat=mat))
}


createmat_smallintervals <- function(input){
  lowmat = round(runif(as.integer(input$numC)*as.integer(input$numAlt))*10,0)
  highmat = lowmat
  for(i in 1:length(lowmat)){
    highmat[i] = lowmat[i]+0.05
  }
  highmat[highmat > 10 ] = 10
  mat = matrix(c(matrix(rbind(lowmat,highmat),byrow = TRUE)),
               nrow = as.integer(input$numAlt),ncol = as.integer( input$numC)*2,byrow = TRUE)
  
  rownames(mat) <- sapply(1:as.integer( input$numAlt), function(i) input[[paste0("NameA",i)]])
  colv1 <- c(rep("C",as.integer( input$numC)))
  colv2 <- c(rep("C",as.integer( input$numC)))
  
  for(j in 1:as.integer( input$numC)){
    colv1[j] <- paste0(input[[paste0("NameC",j)]]," lower ")
    colv2[j] <- paste0(input[[paste0("NameC",j)]], " upper ")
  }
  
  clnam <- rbind(colv1, colv2)
  colnames(mat) <- c(clnam)
  
  return(list(mat=mat))
}


createmat_smallintervals_k <- function(input,k){
  lowmat = round(runif(as.integer(input$numAlt))*10,0)
  highmat = lowmat
  for(i in 1:length(lowmat)){
    highmat[i] = lowmat[i]+0.05
  }
  highmat[highmat > 10 ] = 10
  mat = matrix(c(matrix(rbind(lowmat,highmat),byrow = TRUE)),
               nrow = as.integer(input$numAlt),ncol = 2,byrow = TRUE)
  
  rownames(mat) <- sapply(1:as.integer( input$numAlt), function(i) input[[paste0("NameA",i)]])
  colv1 <- paste0(input[[paste0("NameC",k)]]," lower value ")
  colv2 <- paste0(input[[paste0("NameC",k)]], " upper value ")
  
  clnam <- c(colv1, colv2)
  colnames(mat) <- clnam
  
  smallmat = as.matrix(round(apply(mat,1,'mean'),0),ncol = 1)
  colnames(smallmat) = paste0(input[[paste0("NameC",k)]])
  return(list(mat=mat,smallmat=smallmat))
}
