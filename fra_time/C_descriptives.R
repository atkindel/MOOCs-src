load(file="/home/bd/Dropbox/moocs/data/proc/desc1.Rdata")


#plot mean rfp versus median rt
png("~/Downloads/moocs-time.png",units="in",height=9,width=7,res=100)
par(mfrow=c(3,4),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
fun<-function(course,dat,M=20) {
    dat[[course]]->L
    infun<-function(tfa) {    
        for (i in 1:ncol(tfa)) {
            as.numeric(tfa[,i])->z
            z->tfa[,i]
        }
        as.matrix(tfa)->tfa
        tfa/60 -> tfa
        ifelse(tfa>M,NA,tfa)->tfa
        apply(tfa,1,median,na.rm=TRUE)->tab
    }
    infun(L$time_to_first_attempt)->fp.y
    #
    infun<-function(resp) {
        ifelse(as.matrix(resp)=="correct",1,0)->resp
        rowMeans(resp,na.rm=TRUE)
    }
    infun(L$first_grade)->fp.x
    plot(fp.x,fp.y,xlim=c(0,1),pch=19,cex=.25,ylim=c(0,M),xlab="mean frp",ylab="median time till response")
    loess(fp.y~fp.x)->mod
    cbind(mod$x,mod$fitted)->tmp
    tmp[order(tmp[,1]),]->tmp
    lines(tmp,col="red",lwd=2)
    mtext(side=3,line=.2,nm)
}
for (nm in names(dat)) fun(nm,dat)
dev.off()


#plot mean rfp versus median rt
png("~/Downloads/moocs-time-item.png",units="in",height=9,width=7,res=100)
par(mfrow=c(3,4),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
fun<-function(course,dat,M=20) {
    dat[[course]]->L
    infun<-function(tfa) {    
        for (i in 1:ncol(tfa)) {
            as.numeric(tfa[,i])->z
            z->tfa[,i]
        }
        as.matrix(tfa)->tfa
        tfa/60 -> tfa
        ifelse(tfa>M,NA,tfa)->tfa
        apply(tfa,2,median,na.rm=TRUE)->tab
    }
    infun(L$time_to_first_attempt)->fp.y
    #
    infun<-function(resp) {
        ifelse(as.matrix(resp)=="correct",1,0)->resp
        colMeans(resp,na.rm=TRUE)
    }
    infun(L$first_grade)->fp.x
    plot(fp.x,fp.y,xlim=c(0,1),pch=19,cex=1,ylim=c(0,M),xlab="mean frp",ylab="median time till response")
    loess(fp.y~fp.x)->mod
    cbind(mod$x,mod$fitted)->tmp
    tmp[order(tmp[,1]),]->tmp
    lines(tmp,col="red",lwd=2)
    mtext(side=3,line=.2,nm)
}
for (nm in names(dat)) fun(nm,dat)
dev.off()

## ##look at association between items and responses
## fun<-function(course,dat,t=15) {
##     dat[[course]]->L
##                                         #
##     infun<-function(tfa) {    
##         for (i in 1:ncol(tfa)) {
##             as.numeric(tfa[,i])->z
##             z->tfa[,i]
##         }
##         as.matrix(tfa)->tfa
##         tfa/60 -> tfa
##         ifelse(tfa>t,NA,tfa)->tfa
##     }
##     sum(is.na(L$time_to_first_attempt))->miss1
##     infun(L$time_to_first_attempt)->fp.x
##     sum(is.na(fp.x))->miss2
##                                         #
##     infun<-function(resp) {
##         ifelse(as.matrix(resp)=="correct",1,0)->resp
##         resp
##     }
##     infun(L$first_grade)->fp.y
##     lm(as.numeric(fp.y)~as.numeric(fp.x))->mod
##     mean(fp.x,na.rm=TRUE)->M
##     sd(fp.x,na.rm=TRUE)->S
##     coef(mod)->co
##     co[1]+co[2]*(M-S)->lo
##     co[1]+co[2]*(M+S)->hi
##     nrow(fp.x)*ncol(fp.x)->D
##     c(lo,hi,miss1/D,miss2/D)
## }

## par(mfrow=c(2,3),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
## for (t in c(10,15,20,30,45,60)) {
##     tab<-list()
##     for (nm in names(dat)) fun(nm,dat,t=t)->tab[[nm]]
##     do.call("rbind",tab)->tab
##     plot(tab[,1:2],type="n",xlab=paste(round(c(mean(tab[,3]),mean(tab[,4])),2),collapse=" "))
##     abline(0,1)
##     mtext(side=3,t,line=.3)
##     text(tab[,1],tab[,2],rownames(tab),cex=.6)
## }
