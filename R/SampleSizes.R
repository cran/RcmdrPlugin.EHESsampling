SampleSizes<-function(df1, df2, PSU1, strata1, size1, demo1,  demoyesno, stratayesno, strata2, size2, 
		calcsizevalue, mkvalue, mkvar, ssyesno, ssvar, sstotal, ppstype, nminimum, 
		nfixed, Parcost, PSUcost, Vwithin, Vamong, PSUV, PSUmean, optyesno, costyesno)
	{

	x<-df1
	PSUname<-names(x[PSU1])

#if df2 missing - create dataset
	if(stratayesno==1) {
		strata.levels<-levels(x[,strata1])
		strata.levels<-strata.levels[order(strata.levels)]
		strata.id<-seq(1:length(strata.levels))
		z<-data.frame(strata.levels, strata.id)
		strata2<-1 
		if (ssyesno=="1" & mkvalue==1) {
			z$PSUcost<-tapply(x[,PSUcost], list(x[,strata1]), mean)[]
			z$Parcost<-tapply(x[,Parcost], list(x[,strata1]), mean)[]
			PSUcost<-match("PSUcost", names(z))
			Parcost<-match("Parcost", names(z))
			}
		} else {
		z<-df2
		z<-z[order(z[,strata2]),]
		z$strata.id<-row.names(z)
		}
	
#Standardise
	x<-x[order(x[,strata1]),]
	x$strata.id<-as.numeric(x[,strata1])


#add in calculated size
	strata.size<-as.numeric(xtabs(x[,size1]~x$strata.id))
	if (calcsizevalue=="1"|stratayesno==1) {
				z$size<-strata.size
				size2<-match("size", names(z))
				}

#Add in Vwithin and Vamong
if (optyesno=="0"){
	temp<-data.frame(xtabs(x[,size1]~x[,strata1]))
	names(temp)<-c("strata","size")
	Nk<-temp$size[x$strata.id]
	mu.calc<-x[,size1]*x[,PSUmean]/Nk
	mu<-xtabs(mu.calc~x$strata.id)

	##Calculating Vwithin
	Vwith.calc<-x[,size1]*x[,PSUV]/Nk
	z$Vwithin<-xtabs(Vwith.calc~x$strata.id)
	Vwithin<-match("Vwithin",names(z))

	##Calculating Vamong
	x$Vamong.calc<-x[,size1]*((x[,PSUmean]-mu[x$strata.id])^2)/Nk
	z$Vamong<-xtabs(x$Vamong.calc~x$strata.id)
	Vamong<-match("Vamong",names(z))
	}

#n strata calculations
if (ssyesno=="0" & stratayesno==0){z$n<-z[,ssvar]}
if (ssyesno=="0" & stratayesno==1){
		temp<-cbind(x$strata.id, x[,ssvar])
		z$n<-unique(temp)[,2]}

if (ssyesno=="1"){
	if (ppstype=="1"){		
		sumsize<-sum(z[,size2])
		z$n<-(z[,size2]/sumsize)*sstotal
		}
	if (ppstype=="2"){
		sumsize<-sum(z[,size2])
		z$n<-z[,size2]/sumsize*sstotal
		z$n<-ifelse (z$n < nminimum, nminimum, z$n)
		}
	if (ppstype=="3"){
		z$n<-rep(nfixed, nrow(z))
		}
	}

#Add in number of PSU in each strata
	z$M.PSU<-c(table(x[,strata1])[])

	if(mkvalue==1){
		z$Vratio<-z[,Vwithin]/z[,Vamong]
		z$Cratio<-z[,PSUcost]/z[,Parcost]
		z$p.opt<-sqrt(z$Vratio*z$Cratio)
		z$m<-mk.calc(z$n,z$p.opt,z$M.PSU)
		}
	if (mkvalue==0 & stratayesno==0){z$m<-z[,mkvar]}
	if (mkvalue==0 & stratayesno==1){
		temp<-cbind(x$strata.id, x[,mkvar])
		z$m<-unique(temp)[,2]}

#calculate inclus prob 
	x$m<-z$m[x$strata.id]
	stage1.prob<-matrix(NA, max(x$strata.id),ncol=1)
	for (i in 1:max(x$strata.id)){stage1.prob[i]<-list(inclusionprobabilities(x[,size1][which(x$strata.id==i)], z$m[i]))}
	x$stage1.prob<-c(stage1.prob,recursive=TRUE)
	x$strata.pop<-z[,size2][x$strata.id] #Nk
	x$strata.n<-z$n[x$strata.id]

#rearrange dataset if demographics used
	if(demoyesno=="0"){
		dem<-x
		names(dem)[demo1]<-paste("domain",1:length(demo1),sep="")
		dem<-reshape(dem,direction="long",varying=demo1,sep="",v.names="domain.pop",times=names(x[,demo1]))
		dem$domain<-as.factor(dem$time)
		dem$strata.pop<-z[,size2][dem$strata.id] #Nk
		demographSums<-xtabs(domain.pop~strata.id+domain,data=dem)
		dem$stratademo.pop<-demographSums[cbind(dem$strata.id,dem$domain)] #Nk+l 
		dem$strata.n<-z$n[dem$strata.id]
		dem$time<-dem$id<-NULL
		row.names(dem)<-NULL
		PSU1<-match(names(x)[PSU1], names(dem))
		size1<-match(names(x)[size1], names(dem))
		strata1<-match(names(x)[strata1], names(dem))
	}

#calculate n for those with inc==1 first 
	big<-x[which(x$stage1.prob==1),]
	if (nrow(big)>0) {
		if (demoyesno=="0"){
			big<-dem[dem[,PSU1]%in%big[,PSU1],]
			big$n<-big$strata.n*(big[,size1]/big$strata.pop)/length(demo1)}
		else {big$n<-big$strata.n*(big[,size1]/big$strata.pop)}

#re-adjust strata n
		demfreq<-data.frame(xtabs(big$n~big$strata.id))
		z$minus.n[z$strata.id%in%demfreq[,1]]<-demfreq[,2]
		z$n.new<-ifelse(is.na(z$minus.n),z$n,z$n-z$minus.n)

#adjust m
		if(demoyesno=="0"){
			bigmk<-data.frame(table(big$strata.id)/length(demo1))
			} 
		else {bigmk<-data.frame(table(big$strata.id))}
		z$minus.mk[z$strata.id%in%bigmk[,1]]<-bigmk[,2]
		problem2<-ifelse(z$minus.mk>z$m,1,0)
		if(sum(problem2,na.rm=TRUE)>0) {
			tkmessageBox(message="Sampling has not occured because one (or more) stratum has too many PSU with inclusion probability of 1")
			tkfocus(CommanderWindow())
			}
		z$m.new<-ifelse(is.na(z$minus.mk),z$m,z$m-z$minus.mk)
	
#Join dem/x and big and add in other n's
		if (demoyesno==0) {
			x<-merge(dem,big,all=TRUE)
			x$demo.n<-z$n[x$strata.id]/length(demo1)
			x$n<-ifelse(is.na(x$n),(x$demo.n/z$m.new[x$strata.id])*((x$strata.pop*x$domain.pop)/(x$stratademo.pop*x[,size1])),x$n)
			x$stage2.prob<-x$n/x$domain.pop
			} 
		else{
			x<-merge(x,big,all=TRUE)
			x$n<-ifelse(is.na(x$n),z$n.new[x$strata.id]/z$m.new[x$strata.id],x$n) #nkj
			x$stage2.prob<-x$n/x[,size1]
			}

#Clean up
		z$minus.n<-z$n.new<-z$minus.mk<-z$m.new<-NULL
		message("Sampling has proceeded, however the one or more PSUs have inclusion probabilities equal to 1")
		}
	
#Back to if there are no PSU with inc=1
	else {
		if (demoyesno==0){
			x<-dem
			x$demo.n<-z$n[x$strata.id]/length(levels(x$domain))
			x$n<-(x$demo.n/x$m)*((x$strata.pop*x$domain.pop)/(x$stratademo.pop*x[,size1]))
			x$stage2.prob<-ifelse(x$n==0, 0, x$n/x$domain.pop)                    
			}
		if (demoyesno==1) 	{
			x$n<-x$strata.n/x$m    #nkj
			x$stage2.prob<-ifelse(x$n==0, 0, x$n/x[,size1])                      
			} 
		}

#Add in extras and General Clean-up
	if(costyesno==0) z$cost<-(z[,PSUcost]*z$m)+(z[,Parcost]*z$n)
	if (optyesno==0 | optyesno==1) z$Var<-(z[,Vamong]/z$m)+(z[,Vwithin]/z$n)
	z$strata.id<-x$strata.id<-x$strata.pop<-x$strata.n<-x$stratademo.pop<-x$demo.n<-x$row.names<-NULL
	x<-x[order(x[,match(PSUname, names(x))]),]
	row.names(x)<-c(seq(1:nrow(x)))


#return 2 datasets	
	utfall<-list(as.data.frame(z),as.data.frame(x))
	names(utfall)<-c("StrataSS","PSUSS")
	return(utfall)
}


