rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        result_row <- NULL
        if(num == "best") {
                result_row <- sorted_data[1,]
        }
        else if(num == "worst") {
                result_row <- sorted_data[nrow(sorted_data),]
        }
        else {
                result_row <- sorted_data[num,]
        }
        
        result_row <- data.frame(lapply(result_row, as.character), stringsAsFactors=FALSE)
        
        result <- result_row$name
        
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
        print(state_data)
        outcome_names <- c('heart attack', 'heart failure', 'pneumonia')
        
        valid_state <- is.element(state, state_data)
        valid_outcome <- is.element(outcome, outcome_names)
        
        print(valid_outcome)
        
        if(!valid_state) {
                stop('invalid state')
        }
        
        if(!valid_outcome) {
                stop('invalid outcome')
        }
        
        valid_state & valid_outcome
        
}