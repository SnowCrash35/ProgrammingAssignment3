rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    # ===========================
    # Column 2: Hospital Name
    # Column 7: State
    # Column 11: 30-day death rate for heart attack
    # Column 17: 30-day death rate for heart failure
    # Column 23: 30-day death rate for Pneumonia
    #print("reading data from file...")
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # check that input arguments are valid
    # ======================================
    states <- unique(data$State)
    # sort results by state abbreviation
    states <- states[order(states)]
    
    if( !(state %in% states) )
    {
        stop("invalid state")
    }
    
    if(outcome == "heart attack")
    {   f <- 11 }
    else if(outcome == "heart failure")
    {   f <- 17 }
    else if(outcome == "pneumonia")
    {   f <- 23 }
    else
    {
        stop("invalid outcome")
    }
    
    
    ## Clean the data
    # =========================================================================
    
    # remove columns we are not intersted with
    z <- data[ , c(2, 7, f)]
    
    # coerce death rate data to numeric type
    z[, 3] <- as.numeric(z[, 3])
    
    # omit NA values
    z <- na.omit(z)
    
    # sort the list by Death Rate, then by Hospital Name (both ascending)
    z <- z[order(z[,3], z[,1]), ]
    
    ## For each state, find the hospital of the given rank
    # ====================================================================
    #holder for the output
    result <- data.frame()
    
    # loop thru each state
    for(state in states)
    {
        # look at just the hospitals for the given state
        y <- z[z$State == state, ]
        
        # reset the num varialbe for each state search
        rank <- num
        
        # find the "num" ranked hospital and return Name
        if(rank == "best")
        {
            rank <- 1
        }
        else if(rank == "worst")
        {
            rank <- nrow(y)
            #print(y)
        }
        
        if(rank > nrow(y))
        {
            hospitalName <- NA
        }
        else
        {
            w <- y[rank, ]
            hospitalName <- w$Hospital.Name
        }
        
        result <- rbind(result, data.frame(hospitalName, state))
    }
    
    ## Return a data frame with the hospital names and the (abbreviated) state name
    result
    
}


test_rankall <- function()
{
    print(head(rankall("heart attack", 20), 10))
    print(tail(rankall("pneumonia", "worst"), 3))
    print(tail(rankall("heart failure"), 10))
}