bowlersVsBatsmanOppn <- function(match,theTeam){
    
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
    
    b <-summarise(group_by(a,bowler,batsman),sum(runs))
    names(b) <- c("bowler","batsman","runsConceded")
    # Compute total runs conceded
    c <- summarise(group_by(b,bowler),runs=sum(runsConceded))
    # Sort by descneding
    d <- arrange(c,desc(runs))
    
    # Pick 5 highest run givers
    d <- head(d,5)
    
    bowlers <- as.character(d$bowler)
    e <- NULL
    for(i in 1:length(bowlers)){
        f <- filter(b,bowler==bowlers[i])
        e <- rbind(e,f)
        
    }
    
    names(e) <- c("bowler","batsman","runsConceded")
    ggplot(data=e,aes(x=batsman,y=runsConceded,fill=factor(batsman))) + 
        facet_grid(. ~ bowler) + geom_bar(stat="identity") +
        #facet_wrap( ~ bowler,scales = "free", ncol=3,drop=TRUE) + #Does not work.Check!
        xlab("Batsman") + ylab("Runs conceded") +
        ggtitle(expression(atop("Performances of bowlers against opposition",
                                atop(italic("Data source:http://cricsheet.org/"),"")))) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
