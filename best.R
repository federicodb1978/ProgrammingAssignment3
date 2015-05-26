best <- function(state, outcome) {
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character") # read data
        validStates <- data[ , 7] #possible states
        validOutcomes <-c("heart attack", "heart failure", "pneumonia") #valid outcomes
        if (!(state %in% validStates)) { # check if the state in input exists in the table
                stop("invalid state")
        }
        if(!(outcome %in% validOutcomes)) { # check if the outcome in input is one of the requested
                stop("invalid outcome")
        }
        #fullColName contains the real name of the column
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        #through a matching, colName receives the real name of column matching the input of the user
        colName <-fullColName[match(outcome, validOutcomes)]
        #return the hospital name (column 2) in data, with the lowest mortality rate in the selected state and outcome
        data.state <-data[data$State==state, ] #select only rows of the selected state 
        idx <-which.min(as.numeric(data.state[, colName])) #find the index of the minimum mortality rate in the selected state for the selected outcome
        data.state[idx, "Hospital.Name"] #get the name of the hospital with the lowest mortality rate in the selected statebebe and outcome        
}