createmat <- function(input){
  if(FALSE){
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
  }else{
    mat = as.matrix(read.csv('busval.csv',header = FALSE, skip = 1)[,-1])
    
  }
  rownames(mat) <- sapply(1:as.integer( input$numAlt), function(i) input[[paste0("NameA",i)]])
  colv1 <- c(rep("C",as.integer( input$numC)))
  colv2 <- c(rep("C",as.integer( input$numC)))
  
  for(j in 1:as.integer( input$numC)){
    colv1[j] <- paste0(input[[paste0("NameC",j)]]," lower ")
    colv2[j] <- paste0(input[[paste0("NameC",j)]], " upper ")
  }
  
  clnam <- rbind(colv1, colv2)
  colnames(mat) <- c(clnam)
  
  return(mat)
}

