## rankall is a function which takes in one of three outcomes from the data, as well as a desired rank, and returns 
## the hospital from each state with that rank for that outcome. 
## examples: 
## rankall("heart attack",1)
## rankall("pneumonia", "worst")
## rankall("heart failure",9)

rankall<- function(outcome,num="best")  {   
  df <- read.csv(file ="r_assignment_week4/outcome-of-care-measures.csv", colClasses = "character")
  
  if(outcome == "heart attack") {
    i <- grep("^Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack$", colnames(df))
  } else if(outcome == "heart failure") {
    i <- grep("^Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure$", colnames(df))
  } else if(outcome == "pneumonia") {
    i <- grep("^Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia$", colnames(df))
  }else {
    stop("invalid outcome")}
  
  df <- df[df[,i] != "Not Available",]     
  df[,i] <- suppressWarnings(as.numeric(x = df[,i]))
  df <- df[order(df[,7], df[,i], df[,2]),]  #order by state, then by outcome, then by hospital name in order to acheive rank when we subset
  states <- unique(df[,7]) #make a vector of states
  data = data.frame(matrix(vector(), 0, 2, dimnames=list(c(), c("hospital", "state"))), stringsAsFactors=F) #create empty data frame.
  for (s in 1:length(states)) {
    result = NULL
    df.state <- data.frame() #clear the state for this iteration
    df.state <- df[df$State==states[s],] #subset by state
    if(num == "best") {num2 = 1}
    else if(num == "worst") {num2 <- as.integer(which.max(df.state[,i])) } #get the index of the last hostpital
    else{num2 <- num}
    
    if(num2 > nrow(df.state)) {result <- data.frame(hospital="NA",state=states[s])} 
    else {result <- data.frame(hospital=df.state[num2,"Hospital.Name"],state=states[s])}
    data <- rbind(data, result) #accumulate the row for each state
  }
  return(data)
}