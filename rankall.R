rankall <- function(outcome, num = "best") {
        ## Read outcome data
        all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available", stringsAsFactors=FALSE)
        columnNumber <- getColumnNumber(outcome)
        
        ##all_data <- all_data[all_data$State==state,]
        
        data <- cbind.data.frame(all_data[, 2], all_data[, 7], as.numeric(all_data[, columnNumber]))
        
        completeRows <- complete.cases(data)
        outcome_data <- data[completeRows, ]
        names(outcome_data) <- c('name', 'state', 'rate')
        
        
        ## Check that state and outcome are valid
        valid <- checkValid(outcome)
        
        state_data <- unique(all_data[, 7])
        
        result_frame <- data.frame(hospital= character(), state= character(), stringsAsFactors = FALSE)
        
        ## For each state, find the hospital of the given rank
        for(s in state_data) {
                filtered_data <- outcome_data[outcome_data$State==s,]
                sorted_data <- filtered_data[order(filtered_data$rate, filtered_data$name),]
                
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
                ## df <- rbind(df, data.frame(x = i, y = toString(i)))
                result_frame <- rbind(result_frame, data.frame(state=s, hospital=result_row$name, stringsAsFactors = FALSE))
                
                
        }
        
        result_frame
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
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

checkValid <- function(outcome) {
        
       outcome_names <- c('heart attack', 'heart failure', 'pneumonia')
        
        valid_outcome <- is.element(outcome, outcome_names)
        
        if(!valid_outcome) {
                stop('invalid outcome')
        }
        
        valid_outcome
        
}
