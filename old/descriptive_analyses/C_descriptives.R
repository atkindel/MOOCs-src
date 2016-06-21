load(file="/home/bd/Dropbox/moocs/data/proc/desc1.Rdata")
##todo
##semantic load
##attrition as a function of item position
 
#n attempts
## png("~/Downloads/moocs2.png",units="in",height=9,width=7,res=100)
## par(mfrow=c(4,3),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
## fun<-function(course,dat) {
##     dat[[course]]->L
##     L$n_attempt->zz
##     infun<-function(x) quantile(as.numeric(x),c(.01,.1,.25,.5,.75,.9,.99),na.rm=TRUE)
##     apply(zz,2,infun)->z
##     matplot(t(z),type="l",col="black",lty=1)    
##     mtext(side=3,line=.2,nm)
## }
## for (nm in names(dat)) fun(nm,dat)
## dev.off()

png("~/Downloads/moocs2.png",units="in",height=9,width=7,res=100)
par(mfrow=c(4,3),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
fun<-function(course,dat) {
    dat[[course]]->L
    L$n_attempt->zz
    #infun<-function(x) quantile(as.numeric(x),c(.01,.1,.25,.5,.75,.9,.99),na.rm=TRUE)
    infun<-function(x) quantile(as.numeric(x),c(.5,.9),na.rm=TRUE)
    apply(zz,2,infun)->z
    density(z[1,])->df
    density(z[2,])->dl
    range(c(dl$y,df$y))->ran
    plot(df,col="gray",ylim=ran,xlim=c(0,5),xlab=" ",ylab=" ",sub=" ",main="",lwd=3)
    mtext(side=3,line=.2,nm)
    lines(dl,lwd=3)
    legend("topright",bty="n",c(".5 quantile",".9 quantile"),lty=1,lwd=2,col=c("gray","black"))
}
for (nm in names(dat)) fun(nm,dat)
dev.off()

##response time
png("~/Downloads/moocs3.png",units="in",height=9,width=7,res=100)
par(mfrow=c(4,5),mgp=c(2,1,0),mar=c(3.3,3.3,2,1))
fun<-function(course,dat) {
    dat[[course]]->L
                                        #How much time people spend on items
    infun<-function(tfa) {    
        for (i in 1:ncol(tfa)) {
            as.numeric(tfa[,i])->z
            z->tfa[,i]
        }
        as.matrix(tfa)->tfa
        tfa/60 -> tfa
        if (sum(!is.na(tfa))>0) {
            ifelse(tfa>3600,3600,tfa)->tfa
            apply(tfa,2,median,na.rm=TRUE)->tab
        }
    }
    infun(L$time_to_first_attempt)->fp
    infun(L$time_to_last_attempt)->lp
    density(lp,na.rm=TRUE)->dl
    density(fp,na.rm=TRUE)->df
    range(c(dl$y,df$y))->ran
    plot(df,col="gray",ylim=ran,xlim=c(0,15),xlab="minutes",ylab=" ",sub=" ",main="",yaxt="n",lwd=3)
    mtext(side=3,line=.2,nm)
    lines(dl,lwd=3)
    legend("topright",bty="n",c("first","last"),lty=1,lwd=2,col=c("gray","black"))
}
for (nm in names(dat)) fun(nm,dat)
dev.off()


    


## pdf(paste("/tmp/mooc.pdf",sep=""),width=12,height=12)
## par(mfrow=c(5,3),mgp=c(2,1,0),mar=c(4,3.2,4,1))
##         if (sum(pi)>100) {
##             ##########################################################
##             #looking at how many people completed how many items
##             #apply(gr,2,table)
##             plot(density(rs),main="distribution over people of % items completed",sub=paste(length(rs),"people"))
##             mtext(side=3,adj=0,"A")
##             abline(v=0)
##             abline(v=N)
##             ##########################################################
##             #consider attrition
##             hist(breaks=20,rs[pi],xlim=c(0.5,1),main="hist (% completed | completing half)",sub=paste(length(rs[pi]),"people"))
##             mtext(side=3,adj=0,"B")
##             ##########################################################
##             #attrition as a function of item position
##             colMeans(!is.na(gr[pi,]))->cs
##             plot(cs,type="l",main="completion % by item for who who complete half the items")
##             mtext(side=3,adj=0,"C")
##             ##########################################################
##             #N attempts
##             #apply(n_attempts[pi,-1],2,function(x) median(as.numeric(x),na.rm=TRUE))->med.n.attempts
##             #cumsum(table(med.n.attempts))/N
##             #
##             #n_attempts[pi,-1]->z
##             #max(as.numeric(unlist(z)),na.rm=TRUE)->M
##             #ifelse(M>50,50,M)->M
##             ## plot(NULL,
##             ##      xlim=c(1,ncol(z)),
##             ##      ylim=c(0,M),
##             ##      main="scatterplot (# attempts for each item|completing half)"
##             ##      )
##             ## for (i in 1:ncol(z)) {
##             ##     as.numeric(z[,i])->yy
##             ##     if (sum(!is.na(yy))>0) points(rep(i,nrow(z)),jitter(yy),pch=19,cex=.5)
##             ## }
##             n_attempts[pi,-1]->z
##             tmp<-list()
##             for (i in 1:ncol(z)) cbind(rep(i,nrow(z)),z[,i])->tmp[[i]]
##             do.call("rbind",tmp)->z
##             data.frame(z)->z
##             as.numeric(z[,2])->z[,2]
##             factor(z[,1],ordered=TRUE,levels=1:N)->z[,1]
##             #boxplot(as.numeric(z[,2])~z[,1],xlab="boxplots(# attempts | completing half)")
##             #mtext(side=3,adj=0,"D")
##             ##########################################################
##             #cfc versus ec
##             last_grade[pi,-1]->resp
##             ifelse(as.matrix(resp)=="correct",1,0)->resp
##             NULL->colnames(resp)
##             rowSums(resp,na.rm=TRUE)->ec
##             n_attempts[pi,-1]->tmp
##             as.matrix(apply(tmp,2,as.numeric))->tmp
##             ifelse(tmp==1,1,0)->tmp
##             rowSums(resp*tmp,na.rm=TRUE)->cfa
##             plot(density(cfa),col="red",xlim=c(0,N),main="density (sum scores | completed half)",sub="red is cfa, black is ec")
##             mtext(side=3,adj=0,"E")
##             lines(density(ec),col="black")
##             ##########################################################
##             n_attempts[pi,-1]->tmp
##             as.matrix(apply(tmp,2,as.numeric))->tmp
##             rowMeans(tmp,na.rm=TRUE)->mean.tries
##             cor(cfa,-1/mean.tries)->C
##             plot(cfa,mean.tries,main="cfa versus mean tries | completed half",sub=paste("cor",round(C,2)))
##             mtext(side=3,adj=0,"F")
##             ##########################################################
##             #Variability in time invested in course. 
##             as.numeric(first_view[pi,ncol(first_view)])->max.time
##             if (sum(!is.na(max.time))>0) {
##                 max.time-as.numeric(first_view[pi,2])->del
##                 del/(60^2)->time.invested
##                 summary(time.invested)
##                 summary(time.invested/24)
##                 #plot(density(time.invested/24,na.rm=TRUE))
##                 hist(time.invested/24,breaks=25,xlab="days",main="hist (time invested | complete half")
##                 mtext(side=3,adj=0,"G")
##             }
##             ## coe<-numeric()
##             ## for (i in which(pi)) {
##             ##     as.numeric(first_view[i,-1])->t
##             ##     cbind(1:length(t),t)->tmp
##             ##     lm(tmp[,2]~tmp[,1])->mod
##             ##     coef(mod)[2]->coe[as.character(i)]
##             ## }
##             ## hist(coe)
##             ##########################################################
##             first_attempt[pi,-1]->fa.hold
##             if (nrow(fa.hold)>30) {
##                 sample(1:nrow(fa.hold),30)->random.index
##                 fa.hold[random.index,]->fa.hold
##             }
##             apply(fa.hold,2,as.numeric)->fa.hold
##             min(unlist(fa.hold),na.rm=TRUE)->m
##             apply(fa.hold,1,min,na.rm=TRUE)->m1
##             apply(fa.hold,1,max,na.rm=TRUE)->M1
##             ##########################################################
##             ##Variability in when people take items. 
##             fa.hold->fa
##             fa-m -> fa
##             max(unlist(fa),na.rm=TRUE)-> M
##             plot(NULL,xlim=c(0,M),ylim=c(1,nrow(fa)),xaxt="n",xlab="days",ylab="person number",main="item FAs")
##             mtext(side=3,adj=0,"H")
##             axis(side=1,at=seq(0,M,length.out=5),round(seq(0,M,length.out=5)/(60*60*24),1))
##             for (i in 1:nrow(fa)) {
##                 abline(h=i,lty=1,lwd=.3)
##                 #points(fa[i,],rep(i,ncol(fa)),cex=.4,pch=19)
##                 text(fa[i,],rep(i,ncol(fa)),1:ncol(fa),cex=.4)
##             }
##             plot(NULL,xlim=c(1,N),ylim=c(1,M))
##             for (i in 1:nrow(fa)) lines(1:ncol(fa),fa[i,])
##             ##########################################################
##             #first_attempt[pi,-1]->fa
##             fa.hold->fa
##             apply(fa,2,as.numeric)->fa
##             plot(NULL,xlim=c(0,1),ylim=c(1,nrow(fa)),xlab="proportion of elapsed course",ylab="person number")
##             mtext(side=3,adj=0,"I")
##             for (i in 1:nrow(fa)) {
##                 abline(h=i,lty=1,lwd=.3)
##                 points((fa[i,]-m1[i])/(M1[i]-m1[i]),rep(i,ncol(fa)),cex=.4,pch=19)
##             }
##             ##########################################################
##             #How much time people spend on items
##             time_to_first_attempt[pi,-1]->tfa
##             for (i in 1:ncol(tfa)) {
##                 as.numeric(tfa[,i])->z
##                 z->tfa[,i]
##                 ## if (all(is.na(z))) {
##                 ##     dev.off()
##                 ##     return("no time")
##                 ## }
##             }
##             as.matrix(tfa)->tfa
##             if (sum(!is.na(tfa))>0) {
##                 ifelse(tfa>3600,3600,tfa)->tfa
##                 apply(tfa,2,median,na.rm=TRUE)->tab
##                 hist(tab/60,breaks=35,xlab="time in minutes till first attempt")
##                 mtext(side=3,adj=0,"J")
##                 #
##                 time_to_first_attempt[pi,-1]->tfa
##                 apply(tfa,2,as.numeric)->tfa
##                 time_to_last_attempt[pi,-1]->tla
##                 apply(tla,2,as.numeric)->tla
##                 #ifelse(tfa>3600,3600,tfa)->tfa
##                 #ifelse(tla>3600,3600,tla)->tla
##                 tla-tfa -> time.attempts
##                 ifelse(time.attempts>(60*60*3),60*60*3,time.attempts)->time.attempts
##                 #
##                 hist(apply(tla,2,median,na.rm=TRUE)/60,breaks=35,xlab="time in minutes until last attempt",sub="capped at 3 hours",main="")
##                 mtext(side=3,adj=0,"K")
##                 ifelse(tfa>(60*60*2),60*60*2,tfa)->tfa
##                 ifelse(tla>(60*60*2),60*60*2,tla)->tla
##                 plot(apply(tfa,2,median,na.rm=TRUE)/60,apply(tla,2,median,na.rm=TRUE)/60,xlab="time till first attempt, minutes",ylab="time till first attempt, minutes",sub="all capped at 2 hours")
##                 n_attempts[pi,-1]->na
##                 apply(na,2,as.numeric)->na
##                 plot(apply(tla,2,median,na.rm=TRUE)/60,colMeans(na,na.rm=TRUE),xlab="time till last attempt, minutes",ylab="mean # of attempts",sub="all capped at 2 hours")
##                 mtext(side=3,adj=0,"I")
##                 mtext(side=3,adj=0,"")
##             }
##             ##########################################################
##             plot(NULL,bty="n",xlim=c(0,1),ylim=c(0,1),sub=""main=course)
##             frame()
##             NULL
##         }
##     }
## }
## dev.off()
