teamBowlingWicketKindMatch <- function(match,theTeam){
    # Compute the maidens,runs conceded and overs for the bowlers
    a <-filter(match,team==theTeam)
    
    a1 <- unlist(strsplit(a$ball[1],"\\."))
    # Create a string for substitution 1st or 2nd
    a2 <- paste(a1[1],"\\.",sep="")
    # only wides and noballs need to be included with runs for bowlers.
    # Note: byes and legbyes should not be included
    b <-  a %>%    
        select(bowler,ball,noballs,wides,runs,wicketKind,wicketPlayerOut) %>%
        #mutate(over=gsub("1st\\.","",ball)) %>%
        mutate(over=gsub(a2,"",ball)) %>%
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
    
    r <- full_join(h,e,by="bowler")
    
    # Set NAs to 0
    r[is.na(r$wicketKind),]$wicketKind="noWicket"
    r[is.na(r$wicketPlayerOut),]$wicketPlayerOut="noWicket"
    
    
    ggplot(data=r,aes(x=wicketKind,y=runs,fill=factor(wicketKind))) + 
        facet_grid(. ~ bowler,scales = "free_x", space = "free_x") +
        geom_bar(stat="identity") + 
        xlab("Wicket kind") + ylab("Total runs conceded") +
        ggtitle(expression(atop("Wicket-kind vs Runs conceded by bowlers",
                                atop(italic("Data source:http://cricsheet.org/"),"")))) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
}