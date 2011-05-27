#function for taking a sample of individuals of addresses (SSUs)


stage2Sample.fn<-function(stage2df, PSU, strata, dem, demyesno, inclus2) {
	stage2df[,PSU]<-factor(stage2df[,PSU])
	x<-stage2df

#Create random numbers
	x$rand<-runif(nrow(x))

#order by PSU, demo, random no.
	if (demyesno==0) {x<-x[order(x[,strata],x[,PSU],x[,dem],x$rand),]} else	{x<-x[order(x[,strata],x[,PSU],x$rand),]}
	
#calculate cumulative inclus probs.
	x$b<-cumsum(x[,inclus2])
	x$a[1]<-0
	x$a[2:nrow(x)]<-x$b[1:(nrow(x)-1)]

#calculate a random number between 0 and 1 to start
	r<-runif(1)

#calculate rounded down a value
	x$base<-floor(x$a)

#Add in modified r value
	x$r<-r+x$base

#select sample 
	x$I<-ifelse(x$r>=x$a & x$r<x$b, 1, 0)
	
#create frame with selected individs and tidy up

	data<-x[which(x$I==1),]

#Add in extras and tidy up
	if (demyesno==0) {
		data$PSUdemovar<-paste(data[,PSU], data[,dem], sep="")
		SEL_SSU<-table(data$PSUdemovar)
		data$ST2_SEL_SSU<-SEL_SSU[match(data$PSUdemovar, rownames(SEL_SSU))]
	} else {
		SEL_SSU<-table(data[,PSU])
		data$ST2_SEL_SSU<-SEL_SSU[match(data[,PSU], rownames(SEL_SSU))]
	}

	data$b<-data$a<-data$I<-data$rand<-data$base<-data$r<-data$PSUdemovar<-data$strat.id<-NULL
	if (demyesno==0) {data<-data[order(data[,PSU],data[,dem]),]} else {
		data<-data[order(data[,PSU]),]
		}
	row.names(data)<-NULL
	return(data)
	}

