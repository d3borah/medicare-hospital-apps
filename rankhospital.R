## rankhospital is a function which takes in a state, one of three outcomes from the data, and a desired rank, and 
## returns the hospital for that outcome with the desired rank
##example
## rankhospital("NY","pneumonia","worst")
## rankhospital("AK","heart attack","3")

rankhospital <- function(state,outcome,num = "best") {
  df <- read.csv(file ="r_assignment_week4/outcome-of-care-measures.csv", colClasses = "character")
  if(!any(state == df$State)) {
    stop("invalid state")
  }
  if(outcome == "heart attack") {
    i <- grep("^Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack$", colnames(df))
  } else if(outcome == "heart failure") {
    i <- grep("^Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure$", colnames(df))
  } else if(outcome == "pneumonia") {
    i <- grep("^Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia$", colnames(df))
  }else {
    stop("invalid outcome")}
  
  df.state <- df[df$State == state,] 
  df.state <- df.state[df.state[,i] != "Not Available",]                               
  df.state[,i] <- suppressWarnings(as.numeric(x = df.state[,i]))
  df.state <- df.state[order(df.state[,i], df.state[,"Hospital.Name"]),]     
  if(num == "best") {num = 1}
  if(num == "worst") {num <- as.integer(which.max(df.state[,i])) }
  if(num > nrow(df.state)) {"NA"; return}
  df.state[num,"Hospital.Name"]
  
}
