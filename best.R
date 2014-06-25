## best is a function which takes in a state and one of three outcomes from the data, and returns the best hospital 
## in that state for that outcome
## example
## best("NY","heart attack")

best <- function(state,outcome) {
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
  
  df.state <- df[df$State == state, ] 
  df.state[,i] <- suppressWarnings(as.numeric(x = df.state[, i]))
  best.names <- df.state[(df.state[, i] == min((df.state[, i]),na.rm=TRUE)), ]$Hospital.Name
  sort(best.names) [1]
}

