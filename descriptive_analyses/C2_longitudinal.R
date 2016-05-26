load(file="/home/bd/Dropbox/moocs/data/proc/desc1.Rdata")


#trajectory
png("~/Downloads/moocs4.png",units="in",height=9,width=7,res=100)
par(mfrow=c(3,4),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
fun<-function(course,dat) {
    options(set.seed=11012021)
    dat[[course]]->L
    L$first_attempt->fa.hold
    sample(1:nrow(fa.hold),100)->random.index
    fa.hold[random.index,]->fa.hold
    apply(fa.hold,2,as.numeric)->fa.hold
    min(unlist(fa.hold),na.rm=TRUE)->m
    as.POSIXct(m,origin="1970-01-01 00:00.00 UTC")->m.conv
    fa.hold-m -> fa
    fa/(30*24*60*60)->fa
    max(unlist(fa),na.rm=TRUE)-> M
    plot(NULL,xlim=c(1,ncol(fa.hold)),ylim=c(0,M),ylab="months after start",xlab="")
    mtext(side=3,line=.2,nm)
    mtext(side=1,line=2,paste("start time",m.conv),cex=.6)
    for (i in 1:nrow(fa)) lines(1:ncol(fa),fa[i,])
}
for (nm in names(dat)) fun(nm,dat)
dev.off()

fun<-function(course,dat) {
    dat[[course]]->L
    L$first_attempt->fa.hold
    apply(fa.hold,2,as.numeric)->fa.hold
    min(unlist(fa.hold),na.rm=TRUE)->m
    as.POSIXct(m,origin="1970-01-01 00:00.00 UTC")->m.conv
    fa.hold-m -> fa
    fa/(30*24*60*60)->fa
    max(unlist(fa),na.rm=TRUE)-> M
    oo<-list()
    for (i in 2:ncol(fa)) fa[,i]-fa[,i-1]->oo[[i]]
    sapply(oo,function(x) sum(x<0,na.rm=TRUE)/length(x))->z
    summary(z)
}
oo<-list()
for (nm in names(dat)) fun(nm,dat)->oo[[nm]]


#sequence of item taking
png("~/Downloads/moocs5.png",units="in",height=9,width=7,res=100)
par(mfrow=c(3,4),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
fun<-function(course,dat) {
    options(set.seed=11012021)
    dat[[course]]->L
    L$first_attempt->fa.hold
    sample(1:nrow(fa.hold),20)->random.index
    fa.hold[random.index,]->fa.hold
    apply(fa.hold,2,as.numeric)->fa.hold
    min(unlist(fa.hold),na.rm=TRUE)->m
    apply(fa.hold,1,min,na.rm=TRUE)->m1
    apply(fa.hold,1,max,na.rm=TRUE)->M1
##########################################################
##Variability in when people take items. 
    fa.hold->fa
    fa-m -> fa
    max(unlist(fa),na.rm=TRUE)-> M
    plot(NULL,xlim=c(0,M),ylim=c(1,nrow(fa)),xaxt="n",xlab="days",ylab="")
    mtext(side=3,line=.2,nm)
    axis(side=1,at=seq(0,M,length.out=5),round(seq(0,M,length.out=5)/(60*60*24),1))
    for (i in 1:nrow(fa)) {
        abline(h=i,lty=1,lwd=.3)
        points(fa[i,],rep(i,ncol(fa)),cex=.4,pch=19)
        #text(fa[i,],rep(i,ncol(fa)),1:ncol(fa),cex=.4)
    }
}
for (nm in names(dat)) fun(nm,dat)
dev.off()

#
png("~/Downloads/moocs6.png",units="in",height=9,width=7,res=100)
par(mfrow=c(3,4),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
for (course in names(dat)) {
    plot(NULL,xlim=c(0,100),ylim=c(0,.1),xlab="days",ylab="",main="")
    mtext(side=3,line=.2,course)
    for (course2 in names(dat)) {
        dat[[course2]]->x
        x$first_attempt->fa.hold
        apply(fa.hold,2,as.numeric)->fa.hold
        min(unlist(fa.hold),na.rm=TRUE)->m
        apply(fa.hold,1,min,na.rm=TRUE)->m1
        apply(fa.hold,1,max,na.rm=TRUE)->M1
        ##########################################################
        ##Variability in when people take items. 
        (M1-m1)/(24*60*60)->days
        density(days)->den
        if (course==course2) {
            den->hold
        } 
        lines(den,xlab="days",lwd=1,col="gray",ylab="",main="")
    }
    lines(hold,xlab="days",lwd=3,col="black",ylab="",main="")
}
dev.off()
