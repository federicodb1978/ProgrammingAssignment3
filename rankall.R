#Federico Della Bella - May 2015
#rankall.R function is part of the R Programming Course at Coursersa - assignment 3 Week 4
#The function receives the type of outcome to be evaluated, as the performances in
# heart attack, heart failure, and pneumonia, and the type of ranking (best, worst, or the position 
# in the chart), and return the selected hospital in each state
# The function reads the source of data, outcome-of-care-measures.csv, a db of
# the results of main US hospitals; 

rankall <-function(outcome, num="best"){
        data<-read.csv("outcome-of-care-measures.csv", colClasses="character") #read data
        validOutcomes <- c("heart attack", "heart failure", "pneumonia") #the accepted performances
        if(!(outcome %in% validOutcomes)) { # check if the outcome in input is one of the requested
                stop("invalid outcome") #if the outcome requested is not in the list, the function return an error
        }
        result <- data.frame(hospital = c(), state = c()) # an empty data frame to contain the data
        uniqueStates <- unique(data$State) # list of US countries in the data
        states<-uniqueStates[order(uniqueStates)] # the list of states in alphabetical order
        numStates<-length(states) # the number of states to be evaluated
        i<-0 # inizialization of the counter
        # there are 3 main IF, one per every outcome; then, every OUTCOME has 3 possible charts: for the best, the worst, and the 
        #hospitals in the middle. The comments are referred only to the first case, the following are exactly the same
        if (outcome == "heart attack") { # check if the outcome is heart attack
                  if (num == "best") { # check if the request is to see the best, the worst or a hospital in the middle of the chart
                        for (i in 1:numStates){ # for cycle, it checks state by state the perfoamnces of each hospital in the selected outocme
                                dataAn<-subset(data, State == states[i]) # selection of the rows in the data frame with the selected state
                                # the rows and thus the hospitals in the state are ordered according with the specific outcome
                                data.ordered <-dataAn[ order( as.numeric( as.character(dataAn$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), dataAn$Hospital.Name), ]
                                bestHosp <-data.ordered[1, ]$"Hospital.Name" # the hospital with the selected position in the chart
                                stateBest<-states[i] # the name of the state
                                dateState<-data.frame(hospital=bestHosp, state =stateBest) # the data frame of the specific state
                                result<-rbind(result, dateState)    #the updated result is updated
                        }
                        return(result) # the result is returned
                }
                else if (num == "worst") {
                        for (i in 1:numStates){
                                dataAn<-subset(data, State == states[i])
                                data.ordered <-dataAn[ order( as.numeric( as.character(dataAn$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), dataAn$Hospital.Name), ]
                                last<-nrow(data.ordered)
                                bestHosp <-data.ordered[last, ]$"Hospital.Name"
                                stateBest<-states[i]
                                dateState<-data.frame(hospital=bestHosp, state =stateBest)
                                result<-rbind(result, dateState) 
                        }
                        return(result)
                }
                else {
                        for (i in 1:numStates){
                                dataAn<-subset(data, State == states[i])
                                data.ordered <-dataAn[ order( as.numeric( as.character(dataAn$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), dataAn$Hospital.Name), ]
                                index <-as.numeric(num)
                                bestHosp <-data.ordered[num, ]$"Hospital.Name"
                                stateBest<-states[i]
                                dateState<-data.frame(hospital=bestHosp, state =stateBest)
                                result<-rbind(result, dateState)    
                        }
                        return(result)
                }
        }
        else if (outcome == "heart failure") {
                if (num == "best") {
                        for (i in 1: numStates){
                                dataAn<-subset(data, State == states[i])
                                data.ordered <-dataAn[ order( as.numeric( as.character(dataAn$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), dataAn$Hospital.Name), ]
                                bestHosp <-data.ordered[1, ]$"Hospital.Name"
                                stateBest<-states[i]
                                dateState<-data.frame(hospital=bestHosp, state =stateBest)
                                result<-rbind(result, dateState) 
                        }
                        return(result)
                }
                else if (num == "worst") {
                        for (i in 1: numStates){
                                dataAn<-subset(data, State == states[i])
                                data.ordered <-dataAn[ order( as.numeric( as.character(dataAn$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), dataAn$Hospital.Name), ]
                                lastIndex<- nrow(data.ordered) 
                                bestHosp <-data.ordered[lastIndex, ]$"Hospital.Name"
                                stateBest<-states[i]
                                dateState<-data.frame(hospital=bestHosp, state =stateBest)
                                result<-rbind(result, dateState) 
                        }
                        return(result)
                }
                else {
                        for (i in 1:numStates){
                                dataAn<-subset(data, State == states[i])
                                data.ordered <-dataAn[ order( as.numeric( as.character(dataAn$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), dataAn$Hospital.Name), ]
                                index <-as.numeric(num)
                                bestHosp <-data.ordered[num, ]$"Hospital.Name"
                                stateBest<-states[i]
                                dateState<-data.frame(hospital=bestHosp, state =stateBest)
                                result<-rbind(result, dateState)    
                        }
                        return(result)
                }
                
        }
        else {
                if (num == "best") {
                        for (i in 1:numStates){
                                dataAn<-subset(data, State == states[i])
                                dataValid<-dataAn[!(is.na(dataAn$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), ]
                                data.ordered <-dataAn[ order( as.numeric( as.character(dataAn$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), dataAn$Hospital.Name), ]
                                bestHosp <-data.ordered[1, ]$"Hospital.Name"
                                stateBest<-states[i]
                                dateState<-data.frame(hospital=bestHosp, state =stateBest)
                                result<-rbind(result, dateState)    
                        }
                        return(result)
                }
                else if (num == "worst") {
                        for (i in 1: numStates){
                                dataAn<-subset(data, State == states[i])
                                dataValid<-dataAn[!(is.na(dataAn$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), ]
                                data.ordered <-dataAn[ order( as.numeric( as.character(dataAn$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), dataAn$Hospital.Name), ]
                                indMax <-which.max( as.numeric(as.character(data.ordered$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia )))
                                bestHosp <-data.ordered[indMax, ]$"Hospital.Name"
                                stateBest<-states[i]
                                dateState<-data.frame(hospital=bestHosp, state =stateBest)
                                result<-rbind(result, dateState)    
                        }
                        return(result)
                }
                else {
                        for (i in 1: numStates){
                                dataAn<-subset(data, State == states[i])
                                dataValid<-dataAn[!(is.na(dataAn$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), ]
                                data.ordered <-dataAn[ order( as.numeric( as.character(dataValid$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), dataAn$Hospital.Name), ]
                                index <-as.numeric(num)
                                bestHosp <-data.ordered[num, ]$"Hospital.Name"
                                stateBest<-states[i]
                                dateState<-data.frame(hospital=bestHosp, state =stateBest)
                                result<-rbind(result, dateState)    
                        }
                        return(result)
                }
                
        }
}