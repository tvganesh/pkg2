batsmanPartnershipOppn <- function(match,theTeam,report="summary"){
    # Capture both combinations
    fl1 <- Sys.glob("C:\\software\\cricket-package\\cricsheet\\odi-rdata1\\Australia-India*")
    fl2 <- Sys.glob("C:\\software\\cricket-package\\cricsheet\\odi-rdata1\\India-Australia*")
    fl3 <-c(fl1,fl2)
    
    match <- NULL
    for(i in 1:length(fl3)){
        load(fl3[i])
        match <- rbind(match,overs) 
    }
    
    a <-filter(match,team==theTeam)
    #Get partnerships
    df <- data.frame(summarise(group_by(a,batsman,nonStriker),sum(runs)))
    names(df) <- c("batsman","nonStriker","runs")
    b <- summarise(group_by(df,batsman),partnershipRuns=sum(runs))
    c <- arrange(b,desc(partnershipRuns))
    d <- full_join(df,c,by="batsman")
    if(report == "detailed"){
        partnerships <- arrange(d,desc(partnershipRuns))
    } else{
        partnerships <- arrange(c,desc(partnershipRuns))
    }
    
}