######## Required variables - all in input dataset!
#COUNTRY 
#SURVEY 
#STRATUM_ID 
#PSU_SN
#PSU_SIZE 
#ST1_PROB 
#ST2_DOMAINS 
#DOMAIN_ID 
#DOMAIN_SIZE_
#ST2_PROB 
#ST2_ANT_SSU
#ST2_SEL_SSU
##########


stage1Sample.fn <-function(stage1df, PSU, strata, inclus, samplingtype, randomPSU){

#take out demographic variables
x<-stage1df
x<-x[order(x[,strata],x[,PSU]),]

y<-unique(x[,c(PSU,strata,inclus)])

PSU2<-match(names(x)[PSU], names(y))
strata2<-match(names(x)[strata],names(y))
inclus2<-match(names(x)[inclus],names(y))

y$strata.id<-as.numeric(y[,strata2])

#Set indicator I to 1 for those with inclusion probability =1
y$I<-ifelse(y[,inclus2]==1,1,0)

#select those with inclusion probability <1
sample<-y[which(y[,inclus2]<1),]
if (nrow(y)!=nrow(sample)) {
	message("Sampling has proceeded, however this dataset contains PSU(s) with inclusion probabilities = 1")
	}

#sample those with inclusion prob<1
total<-max(sample$strata.id)
pb<-tkProgressBar(title = "progress bar", min = 0,
                    max = total, width = 300)
selectionI<-matrix(NA, nrow=total, ncol=1)
for (i in 1: max(sample$strata.id)){
	selectionI[i]<-list(samplingtype(sample[,inclus2][which(sample$strata.id==i)]))
	setTkProgressBar(pb, i, label=paste( round(i/total*100, 0),"% done"))
	}
close(pb)
sample$I<-c(selectionI,recursive=TRUE)
sample<-rbind(subset(sample,sample$I==1),y[which(y$I==1),])

#Combine 
Selected.PSU<-x[which(x[,PSU] %in% sample[,PSU2]),]

#Randomize PSU order if check button selected
if (randomPSU==1){
	Selected.PSU$rand<-runif(nrow(Selected.PSU))
	rrr<-tapply(Selected.PSU$rand, FUN=min, INDEX=factor(Selected.PSU[,PSU]))
	Selected.PSU$rrr<-rrr[match(Selected.PSU[,PSU], rownames(rrr))]
	Selected.PSU<-Selected.PSU[order(Selected.PSU[,strata], Selected.PSU$rrr),]

}

#Tidy up 
row.names(Selected.PSU)<-Selected.PSU$I<-Selected.PSU$strata.id<-Selected.PSU$PSUorder<-Selected.PSU$rand<-Selected.PSU$rrr<-NULL
return(Selected.PSU)
}