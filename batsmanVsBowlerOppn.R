batsmanVsBowlersOppn <- function(match,theTeam)
{
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
    b <-summarise(group_by(a,batsman,bowler),sum(runs))
    names(b) <- c("batsman","bowler","runsConceded")
    ggplot(data=b,aes(x=bowler,y=runsConceded)) + 
        facet_grid(~ batsman) + geom_bar(stat="identity") + 
        ggtitle(expression(atop("Batsmen vs Bowler opposition",
                                atop(italic("Data source:http://cricsheet.org/"),"")))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}