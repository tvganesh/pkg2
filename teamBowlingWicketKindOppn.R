teamBowlingWicketKindOppn <- function(match,theTeam){
    # Capture both combinations
    fl1 <- Sys.glob("C:\\software\\cricket-package\\cricsheet\\odi-rdata1\\Australia-India*")
    fl2 <- Sys.glob("C:\\software\\cricket-package\\cricsheet\\odi-rdata1\\India-Australia*")
    fl3 <-c(fl1,fl2)
    
    match <- NULL
    for(i in 1:length(fl3)){
        load(fl3[i])
        match <- rbind(match,overs) 
    }
    # Compute the maidens,runs conceded and overs for the bowlers
    a <-filter(match,team==theTeam)
    
    # only wides and noballs need to be included with runs for bowlers.
    # Note: byes and legbyes should not be included
    b <-  a %>%    
        select(bowler,ball,noballs,wides,runs,wicketKind,wicketPlayerOut) %>%
        mutate(over=gsub("1st\\.","",ball)) %>%
        mutate(over=gsub("\\.\\d+","",over))
    
    #Calculate the number of maiden overs
    c <- summarise(group_by(b,bowler,over),sum(runs,wides,noballs))
    names(c) <- c("bowler","over","runsConceded")
    d <-summarize(group_by(c,bowler),maidens=sum(runsConceded==0))
    
    #Compute total runs conceded (runs_wides+noballs)
    e <- summarize(group_by(c,bowler),runs=sum(runsConceded))
    
    # Calculate the number of overs bowled by each bwler
    #f <- select(c,bowler,over)
    #g <- summarise(group_by(f,bowler),overs=length(unique(over)))
    
    
    #Compute number of wickets
    h <- b %>%
        select(bowler,wicketKind,wicketPlayerOut) %>%
        filter(wicketPlayerOut != "nobody")
    i <- summarise(group_by(h,bowler),wickets=length(unique(wicketPlayerOut)))
    
    #Join the over & maidens
    #j <- full_join(g,d,by="bowler")
    # Add runs
    #k <- full_join(j,e,by="bowler")
    # Add wickets
    #l <- full_join(k,i,by="bowler")
    
    r <- full_join(h,e,by="bowler")
    
    # Set NAs to 0
    if(sum(is.na(r$wicketKind)) != 0){
         r[is.na(r$wicketKind),]$wicketKind="noWicket"
    }
    if(sum(is.na(r$wicketPlayerOut)) !=0){
         r[is.na(r$wicketPlayerOut),]$wicketPlayerOut="noWicket"
    }
    
    
    ggplot(data=r,aes(x=wicketKind,y=runs,fill=factor(wicketKind))) + 
        facet_wrap( ~ bowler,scales = "fixed", ncol=8) +
        geom_bar(stat="identity") + 
        xlab("Wicket kind") + ylab("Runs conceded") +
        ggtitle(expression(atop("Bowlers vs wicketkind opposition",
                                atop(italic("Data source:http://cricsheet.org/"),"")))) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
   
    
}