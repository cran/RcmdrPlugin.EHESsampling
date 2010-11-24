
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
	sample.ordered<-NULL
	for (i in 1:max(sample$strata.id)){
		temp<-subset(sample, strata.id==i)
		temp$rand<-runif(nrow(temp))
		temp<-temp[order(temp$rand),]
		sample.ordered<-rbind(sample.ordered,temp)
	}
	sample.ordered$PSUorder<-seq(1:nrow(sample.ordered))
	Selected.PSU$PSUorder<-sample.ordered$PSUorder[match(Selected.PSU$postcode,sample.ordered$postcode)]
	Selected.PSU<-Selected.PSU[order(Selected.PSU$PSUorder),]
}

#Tidy up 
row.names(Selected.PSU)<-Selected.PSU$I<-Selected.PSU$strata.id<-Selected.PSU$PSUorder<-NULL
return(Selected.PSU)
}