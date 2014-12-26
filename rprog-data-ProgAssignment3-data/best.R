best <- function(state, outcome){
      ## This function takes two arguments: the 2-character abbreviated name of a state and an outcome name.
      ## Valid outcome names are:
      ## "heart attack"
      ## "heart failure"
      ## "pneumonia"
      ##
      ## The function reads the outcome-of-care-measures.csv file and returns a character vector 
      ## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for 
      ## the specified outcome in that state. The hospital name is the name provided in the 
      ## Hospital.Name variable. The outcomes can be one of “heart attack”, “heart failure”, 
      ## or “pneumonia”. Hospitals that do not have data on a particular outcome are 
      ## excluded from the set of hospitals when deciding the rankings.
      ## Relevant columns are:
      ## [7] "State" 
      ## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
      ## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
      ## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
      ##
      ## If there is a tie for the best hospital for a given outcome, then the hospital names 
      ## will be sorted in alphabetical order and the first hospital in that set will 
      ## be chosen (i.e. if hospitals “b”, “c”, and “f” are tied for best, then 
      ## hospital “b” will be returned).
      ##
      ## The function checks the validity of its arguments. If an invalid state value 
      ## is passed to best, the function throws an error via the stop function with 
      ## the exact message “invalid state”. If an invalid outcome value is passed to 
      ## best, the function will throw an error via the stop function with the exact 
      ## message “invalid outcome”.

      ## Read outcome data.  Make sure all columns are character class
      MyOutcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Check that state and outcome are valid by surching for the args.
      ## Verify that the State is valid
      ## Get a logical vector with TRUE set for Rows that contain the requested state 
      MyStateRows <- MyOutcomeData[,7] == toupper(state)
      ## If every no "state" is found (i.e. all FALSE) then throw an error.
      if(all(MyStateRows == "FALSE")) stop("invalid state")   

      ## Check to see if outcome is valid, if so, store the appropriate outcome column in MyOutcomeCol"
      ## If outcome is not valid then throw an error using stop("invalid outcome")
      MyOutcomeCol <- switch(tolower(outcome),
            'heart attack' = 11,
            'heart failure' = 17,
            'pneumonia' = 23,
            stop("invalid outcome")
      )
      
      ## Store the Hospital Name and the Outcome Rates for the specified State
      MyWorkingData <- MyOutcomeData[MyStateRows,c(2,MyOutcomeCol)]
      
      ## Coherce the MyOutcome column from Character to integer
      MyWorkingData[,2] <- as.numeric(MyWorkingData[,2])
      
      MyBestHospital <- MyWorkingData[order(MyWorkingData[,2], MyWorkingData[,1]), ]      
      
      ## Find the Best hospital in that state with lowest 30-day death rate
      MyBestHospital <- MyWorkingData[order(MyWorkingData[,2], MyWorkingData[,1]), ]      
      
      ## Return hospital name in that state with lowest 30-day death rate
      return(MyBestHospital[1,1])      
}
