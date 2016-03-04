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
    
    df <- arrange(df,desc(runs))
    
    
    ggplot(data=df,aes(x=batsman,y=runs,fill=nonStriker))+
        geom_bar(data=df,stat="identity") +
        xlab("Batsman") + ylab("Partnership runs") +
        ggtitle(expression(atop("Partnership runs  by batsman against Opposition",
                                atop(italic("Data source:http://cricsheet.org/"),"")))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    
}