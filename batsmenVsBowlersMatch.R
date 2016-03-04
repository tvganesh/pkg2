batsmenVsBowlersMatch <- function(match,theTeam)
{
    a <-filter(match,team==theTeam)
    b <-summarise(group_by(a,batsman,bowler),sum(runs))
    names(b) <- c("batsman","bowler","runsConceded")
    ggplot(data=b,aes(x=bowler,y=runsConceded,fill=factor(bowler))) + 
        facet_grid(~ batsman) + geom_bar(stat="identity") + 
        xlab("Opposition bowlers") + ylab("Runs scored") +
        ggtitle('Batsmen vs Bowlers in Match') +
        ggtitle(expression(atop('Batsmen vs Bowlers in Match',
                                atop(italic("Data source:http://cricsheet.org/"),"")))) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}