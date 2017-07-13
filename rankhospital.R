rankhospital <- function(state, outcome, num = "best") {
    
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
    
    ## Return hospital name in that state with the given rank 30-day death rate
    # =========================================================================
    
    # remove columns we are not intersted with
    z <- data[data$State == state, c(2, 7, f)]
    
    # coerce death rate data to numeric type
    z[, 3] <- as.numeric(z[, 3])
    
    # omit NA values
    z <- na.omit(z)
    
    # sort the list by Death Rate, then by Hospital Name (both ascending)
    z <- z[order(z[,3], z[,1]), ]
    
    # find the "num" ranked hospital and return Name
    if(num == "best")
    {
        num <- 1
    }
    else if(num == "worst")
    {
        num <- nrow(z)
    }
    
    if(num > nrow(z))
    {
        result <- NA
    }
    else
    {
        w <- z[num, ]
        result <- w$Hospital.Name
    }
    
    result
    
}

test_rankhospital <- function()
{
    print(rankhospital("TX", "heart failure", 4))
    print(rankhospital("MD", "heart attack", "worst"))
    print(rankhospital("MN", "heart failure", 5000))
}