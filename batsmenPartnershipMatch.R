batsmenPartnershipMatch <- function(match,theTeam){
    a <-filter(match,team==theTeam)
    #Get partnerships
    df <- data.frame(summarise(group_by(a,batsman,nonStriker),sum(runs)))
    names(df) <- c("batsman","nonStriker","runs")
    ggplot(data=df,aes(x=batsman,y=runs,fill=nonStriker))+
        geom_bar(data=df,stat="identity") +
        xlab("Batmen") + ylab("Runs Scored") +
        ggtitle(expression(atop("Batting partnerships in match",
                                atop(italic("Data source:http://cricsheet.org/"),"")))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
}