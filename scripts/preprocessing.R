##this list contains basically the shorthand course names i am going to use.
##we can anonymize if need be.
courses<-list(ResGeo="EarthSciences_ResGeo202_Spring2015",
              GlobalHealth="GlobalHealth_IWHHR_Summer2014",
              MathEdu="Education_115SP_2015",
              Stocks="GSB_StocksBonds_SelfPaced",
              MathEdu.v2="Education_EDUC115-S_Spring2014",
              StatLearn="HumanitiesandScience_StatLearning_Winter2015",
              OpenKnowl="Education_OpenKnowledge_Fall2014",
              Econ1="HumanitiesSciences_Econ-1_Summer2014",
              Compilers="Engineering_Compilers_Fall2014",
              Econ1.v2="HumanitiesSciences_Econ_1_Summer2015",
              ChemE="Engineering_IntroChE_SelfStudy",
              EP="HumanitiesSciences_EP-101_Spring2015",
              Nano="Engineering_Nano_Summer2014",
              Anes="Medicine_ANES204_Fall2014",
           #"Engineering_Networking-SP_SelfPaced", #was a problem with this course
              Anes.v2="Medicine_ANES205_Fall2014",
              QMSE="Engineering_QMSE01._Autumn2015",
              MedStat="Medicine_MedStats_Summer2014",
              QMSE.v2="Engineering_QMSE-01_Fall2014",
              MedStat.v2="Medicine_MedStats._Summer2015",
              QMSE.v3="Engineering_QMSE-02_Winter2015",
              MolFound="Medicine_MolFoundations_SelfPaced",
              DigDeeper="English_DiggingDeeper1_Winter2015",
              SciWrite="Medicine_Sci-Write_Fall2014",
              DigDeep2="English_diggingdeeper2_Spring2015",
              SciWrite.v2="Medicine_SciWrite._Fall2015",
              WomensHealth="GlobalHealth_IntWomensHealth_Jan2015",
              Haptics="SelfPaced_Haptics_2014",
              WomensHealth.v2="GlobalHealth_INT.WomensHealth_July2015"
           )

set.dir<-"/Users/vpoluser/Code/irt/data/"

tab<-dat<-list()
for (course in unlist(courses)) {
    id.list<-L<-list()
    read.csv(paste(set.dir,"raws/",course,"_ProblemMetadata.csv",sep=""))->pm
    setwd(paste(set.dir,"exports/",course,sep=""))
    list.files()->lf
    grep("export_summary.txt",lf)->index
    lf[-index]->lf #this is the list of files that will get processed
    for (ii in 1:length(lf)) {
        lf[ii]->fn
        skip<-0 #this is going to be used to escape from the loop when need be (e.g., sample size is too small)
        read.csv(fn,header=FALSE)->fv
        if (ii==1) {
            fv[,1]->ids #this is the learner ids that i will match to in later iterations of this loop.
        } else {
            match(ids,fv[,1])->index
            fv[index,]->fv
        }
        fv[,1]->id.list[[fn]] #am going to get rid of the learner ids (next line), but want to first save these to make sure everything looks ok.
        fv[,-1]->fv
        strsplit(as.character(fv[1,]),"-problem-",fixed=TRUE)->tmp
        sapply(tmp,"[",2)->tmp
        strsplit(tmp,"_")->tmp
        sapply(tmp,"[",1)->tmp
        # match(pm[,1],tmp)->index #make sure the items are in the right order
        unlist(lapply(pm[,1], function(x) which(tmp %in% x))) -> index
        if (any(is.na(index))) {
            skip<-1 #if there are missing items, let's escape.
        }
        if (skip==0) {
            fv[,index]->fv
            fv[-1,]->fv #get rid of the first row which is just a header.
            fv->L[[gsub(".csv","",fn)]]
        }
    }
    id.test<-logical()
    for (i in 2:length(id.list)) id.test[i-1]<-all(id.list[[1]]==id.list[[i]]) #this is just a check
    if (!all(id.test,na.rm=TRUE)) print("problem!!!") #the NAs come from places where there are different numbers of learners in the various csv files
    ##
    if (skip==0) {
        ##now get just those people that have valid responses to half the items
        L$last_grade->gr
        as.matrix(gr)->gr
        ncol(gr)->N
        ifelse(gr=="none",NA,gr)->gr
        rowSums(!is.na(gr))->rs
        rs/ncol(gr)->rs
        rs>=0.5 -> pi #these are people that complete at least half the items
        grep(course,courses)->index
        if (sum(pi)>300) { #300 here was somewhat arbitarary. decent sample, but also leaves 12 courses which is 4x3 (nice for figure)
            for (ii in 1:length(L)) L[[ii]][pi,]->L[[ii]]
            L->dat[[names(courses)[index] ]]
        }
        c(course,nrow(gr),sum(pi),N)->tab[[names(courses)[index]]]
    }
}
order(names(dat))->index
dat[index]->dat
do.call("rbind",tab)->tab
tab #N people, N>.5, N items

#change below directory to suit yourself
save(dat,file="/Users/vpoluser/Code/irt/data/bd_processed.Rdata")
