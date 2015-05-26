rankhospital <- function(state, outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character") # read data
        validStates <- data[ , 7] #possible states
        validOutcomes<-c("heart attack", "heart failure", "pneumonia")
        if (!(state %in% validStates)) { # check if the state in input exists in the table
                stop("invalid state")
        }
        if(!(outcome %in% validOutcomes)) { # check if the outcome in input is one of the requested
                stop("invalid outcome")
        } 
        #fullColName contains the real name of the column
        #fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        #through a matching, colName receives the real name of column matching the input of the user
       # colName <-fullColName[match(outcome, validOutcomes)]
        data1 <-data[data$State==state, ] #select only rows of the selected state
       #there are three options, one per every outcome admitted
        if (outcome == "heart attack") { # check what's the outcome
                dataValid<-data1[!(is.na(data1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), ] # check which rows have data of the selected outcome
                # the following line order the list of valid data in the selected state, considering the selected outcome
                data.ordered <-dataValid[ order( as.numeric( as.character(dataValid$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), dataValid$Hospital.Name), ]
                #check what's the worst result
                indMax <-which.max( as.numeric(as.character(data.ordered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack )))
        }
        else if (outcome == "heart failure") {
                dataValid<-data1[!(is.na(data1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), ]
                data.ordered <-dataValid[order(as.numeric(as.character(dataValid$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), dataValid$Hospital.Name), ]
                indMax <-which.max( as.numeric(as.character(data.ordered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure )))        
        }
        else {
                dataValid<-data1[!(is.na(data1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), ]
                data.ordered <-dataValid[order(as.numeric(as.character(dataValid$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), dataValid$Hospital.Name), ]
                indMax <-which.max( as.numeric(as.character(data.ordered$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Pneumonia )))
        }
        if (num =="best") {
                best <-data.ordered[1, ]$"Hospital.Name"
        }
        else if (num=="worst") {
                best <-data.ordered[indMax, ]$"Hospital.Name"
        }
        else if (num >nrow(data.ordered)) {
                best <-"NA"
        }
        else {
                index <-as.numeric(num)
                best<-data.ordered[num, ]$"Hospital.Name"
        }
        return(best) #return the final result
}
        