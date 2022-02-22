# function to show a linear model based on the regsubsets() function. 
# Note: Make sure first input variable in the function is in quotes. For Example: BestSubsets("Price", regout)
# second input should be the result from regsubsets() 

BestSubsets = function(y, regout){
  summary_reg <- summary(all)
  
  #find the best variables based on lowest Mallows' CP 
  truevariables = data.frame(summary_reg$which[which.min(summary_reg$cp),][summary_reg$which[which.min(summary_reg$cp),] == TRUE])
  
  #find row that gives intercept and remove it from the list 
  intercept = which(rownames(truevariables) == "(Intercept)")
  truevariables =subset(truevariables, rownames(truevariables) !="(Intercept)")
  
  #paste the variables together with a "+" and create a linear model with the best subsets 
  model <- noquote(paste(y, "~", paste(noquote(rownames(truevariables)), collapse = "+")))
  
  return(model) 
}
