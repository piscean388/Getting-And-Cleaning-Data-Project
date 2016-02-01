best <- function(state, outcome) {
        
        ## Read outcome data
        all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available", stringsAsFactors=FALSE)
        columnNumber <- getColumnNumber(outcome)
        
        all_data <- all_data[all_data$State==state,]
        
        data <- cbind.data.frame(all_data[, 2], as.numeric(all_data[, columnNumber]))
        
        completeRows <- complete.cases(data)
        outcome_data <- data[completeRows, ]
        names(outcome_data) <- c('name', 'rate')
        
        ## Check that state and outcome are valid
        valid <- checkValid(state, outcome, all_data)
        
        sorted_data <- outcome_data[order(outcome_data$rate, outcome_data$name),]

        best_row <- sorted_data[1,]
        best_row <- data.frame(lapply(best_row, as.character), stringsAsFactors=FALSE)
        
        print(best_row)
        
        result <- best_row$name
        
        print(result)
        ##smallest.state.pop <- min(population$POPESTIMATE2009)
        ##print(population[population$POPESTIMATE2009==smallest.state.pop,])
        
        ##if(valid) {
        ##        best_row <- outcome_data[outcome_data$Rate == min_rate, ]
        ##}
        
        ##best_row
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}

getColumnNumber <- function(outcomeName) {
        col <- NULL
        
        if(outcomeName == 'heart attack') {
           col <- 11     
        }
        else if(outcomeName == 'heart failure') {
           col <- 17     
        }
        else if(outcomeName == 'pneumonia') {
           col <- 23     
        }
        
        col
}

checkValid <- function(state, outcome, data) {
        
        state_data <- unique(data[, 7])
        outcome_names <- c('heart attack', 'heart failure', 'pneumonia')
        
        valid_state <- is.element(state, state_data)
        valid_outcome <- is.element(outcome, outcome_names)
        
        if(!valid_state) {
                stop('invalid state')
        }
        
        if(!valid_outcome) {
                stop('invalid outcome')
        }
        
        valid_state & valid_outcome
        
}