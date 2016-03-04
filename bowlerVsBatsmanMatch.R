bowlersVsBatsmanMatch <- function(match,theTeam){
    a <-filter(match,team==theTeam)
    b <-summarise(group_by(a,bowler,batsman),sum(runs))
    names(b) <- c("bowler","batsman","runsConceded")
    ggplot(data=b,aes(x=batsman,y=runsConceded,fill=factor(batsman))) + 
        facet_grid(. ~ bowler) + geom_bar(stat="identity") + 
        ggtitle(expression(atop("Bowler vs Batsman",
                                atop(italic("Data source:http://cricsheet.org/"),"")))) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
