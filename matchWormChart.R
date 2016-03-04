matchWormGraph <- function(match,team1,team2) {
    a <-filter(match,team=="England")
    b <- select(a,ball,totalRuns)
    c <-mutate(b,ball=gsub("1st\\.","",ball))
    d <- mutate(c,total=cumsum(totalRuns))
    
    a <-filter(match,team=="Ireland")
    b1 <- select(a,ball,totalRuns)
    c1 <-mutate(b1,ball=gsub("2nd\\.","",ball))
    d1 <- mutate(c1,total=cumsum(totalRuns))
    plot(d$ball,d$total,col="blue",type="l")
    lines(d1$ball,d1$total,type="l",col="red")
}