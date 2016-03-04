teamBowlingWicketsOppn <- function(match,theTeam){
    
    
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
    
    
    #Compute number of wickets
    h <- b %>%
        select(bowler,wicketKind,wicketPlayerOut) %>%
        filter(wicketPlayerOut != "nobody")
    i <- summarise(group_by(h,bowler),wickets=length(unique(wicketPlayerOut)))
    
    r <- full_join(h,e,by="bowler")
    
    # Set NAs to 0
    if(sum(is.na(r$wicketKind)) != 0){
        r[is.na(r$wicketKind),]$wicketKind="noWicket"
    }
    if(sum(is.na(r$wicketPlayerOut))!= 0){
        r[is.na(r$wicketPlayerOut),]$wicketPlayerOut="noWicket"
    }
    
    # Filter the top 9 bowlers
    # Call the function to get the summary for the bowlers
    l <- teamBowlingDetailsOpposition(match,theTeam)
    
    # Pick the top 9
    m <- l[1:9,]
    n <- as.character(m$bowler)
    s <- NULL
    for(i in 1:length(n)){
        a <- filter(r,bowler==n[i])
        s <- rbind(s,a)
    }
    
    # Plot as a grid of plots
    ggplot(data=s,aes(x=wicketPlayerOut,y=runs,fill=factor(wicketPlayerOut))) + 
        facet_wrap( ~ bowler,scales = "fixed",ncol=3,drop=TRUE)+ 
        geom_bar(stat="identity") + 
        xlab("Wickets player out") + ylab("Runs conceded") +
        ggtitle(expression(atop("Wickets taken by bowlers vs Runs conceded",
                                atop(italic("Data source:http://cricsheet.org/"),"")))) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    
}