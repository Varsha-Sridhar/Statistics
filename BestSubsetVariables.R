#function to show list of best variables based on regsubsets() 
#input should be the result from regsubsets()

BestSubsetVariables = function(regout){
  summary_reg <- summary(regout)
  
  #find the best variables based on best r-squared 
  truevariables = data.frame(summary_reg$which[which.min(summary_reg$cp),][summary_reg$which[which.min(summary_reg$cp),] == TRUE])
  
  #find row that gives intercept and remove it from the list 
  intercept = which(rownames(truevariables) == "(Intercept)")
  truevariables =subset(truevariables, rownames(truevariables)!="(Intercept)")
  
  #paste the variables together with a +  
  bestvariables <- noquote(paste(noquote(rownames(truevariables)), collapse = "+"))

  return(bestvariables) 
}
