best <- function(state, outcome) 
{
    # read outcome data from file
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
    
    # return hospital name in that state with lowest 30-day death rate
    # ================================================================
    
    # remove columns we are not intersted with
    z <- data[data$State == state, c(2, 7, f)]
    
    # coerce death rate data to numeric type
    z[, 3] <- as.numeric(z[, 3])
    
    # omit NA values
    z <- na.omit(z)
    
    # find the MIN death rate and return the respective Hospital Name
    z <- z[ z[,3] == min(z[,3]), ]
    z$Hospital.Name
}

testbest <- function()
{
    print(best("TX", "heart attack"))
    print(best("TX", "heart failure"))
    print(best("MD", "heart attack"))
    print(best("MD", "pneumonia"))
    print(best("BB", "heart attack"))
    print(best("NY", "hert attack"))
}