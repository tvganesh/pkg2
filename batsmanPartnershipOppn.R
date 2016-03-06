batsmanPartnershipOppn <- function(match,theTeam,report="summary"){
    
    
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