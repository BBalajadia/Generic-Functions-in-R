#################################################
#     Bryan R. Balajadia                     
#     Fn: Measures of Central Tendency
#################################################

# For illustration, we'll work with a dataset built into R, called mtcars.
# The data was extracted from the 1974 Motor Trend US magazine, and comprises 
# fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models).

?mtcars
mydata <- mtcars

# Function to find Central Tendency
centraltendency <- function(df)
{ 
  df <- as.data.frame(df)
  numeric <- sapply(df, is.numeric)
  df.numeric <- df[numeric]
  
  for( i in 1:ncol(df.numeric)) {
  
    print(paste("Variable: ", colnames(df.numeric)[i]))
    b <- colnames(df.numeric)[i]
    No.of.unique <- length(unique(df.numeric[[b]]))
    print(paste("Count of Unique Data Values: ", No.of.unique))
    
    if(No.of.unique < 20) {
      print("Mode")
      temp <- table(as.vector(df.numeric[[b]]))
      print(temp)
    }
    
    print(paste("Number of missing values: ", sum(is.na(df.numeric[[b]]))))
    print(paste("Min: ", min(df.numeric[[b]], na.rm = T)))
    print(paste("Mean: ", mean(df.numeric[[b]], na.rm = T)))
    print(paste("Max: ", max(df.numeric[[b]], na.rm = T)))
    a <- quantile(df.numeric[[b]], c(.05,.1,.25,.33,.5,.75,.9,.95,0.96,0.97,.98,.99,.995), na.rm = T)
    print("quantiles")
    print(a)
  }
}

# passing mtcars dataset into the function
centraltendency(mydata)
