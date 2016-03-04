getOppositionDetails <- function(){
    
    # Capture both combinations
    fl1 <- Sys.glob("C:\\software\\cricket-package\\cricsheet\\odi-rdata1\\Australia-India*")
    fl2 <- Sys.glob("C:\\software\\cricket-package\\cricsheet\\odi-rdata1\\India-Australia*")
    fl3 <-c(fl1,fl2)
    
    match <- NULL
    for(i in 1:length(fl3)){
        load(fl3[i])
        match <- rbind(match,overs) 
    }
    match
}