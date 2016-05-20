load(file="/Users/vpoluser/Code/MOOCs-src/desc1.Rdata")

## To begin to understand this variation, we first describe variation in item characteristics (e.g. mean item response correctness) across MOOCs.


png("~/Downloads/moocs1.png",units="in",height=18,width=14,res=100)
par(mfrow=c(4,3),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
fun<-function(course,dat) {
    dat[[course]]->L
    infun<-function(resp) {
        ifelse(as.matrix(resp)=="correct",1,0)->resp
        rowMeans(resp,na.rm=TRUE)
    }
    infun(L$first_grade)->g1
    infun(L$second_grade)->g2
    infun(L$third_grade)->g3
    infun(L$fourth_grade)->g4
    infun(L$fifth_grade)->g5
    infun(L$last_grade)->gl
    density(g1)->dg1
    density(g2)->dg2
    density(g3)->dg3
    density(g4)->dg4
    density(g5)->dg5
    density(gl)->dgl
    range(c(dg1$y,dg2$y,dg3$y,dg4$y,dg5$y,dgl$y))->ran
    plot(dgl,col="purple",ylim=ran,xlim=c(0,1),xlab=" ",ylab=" ",sub=" ",main="",lwd=1)
    mtext(side=3,line=.2,nm)
    lines(dg5,col="blue",lwd=1)
    lines(dg4,col="green",lwd=1)
    lines(dg3,col="gold",lwd=1)
    lines(dg2,col="orange",lwd=1)
    lines(dg1,col="red",lwd=1)
    legend("topleft",bty="n",c("first","second","third","fourth","fifth","last"),lty=1,lwd=2,col=c("red","orange","gold","green","blue","purple"))
}
for (nm in names(dat)) fun(nm,dat)
dev.off()
