stage2Sample.fn<-function(stage2df, PSU, strata, dem, inclus1, inclus2) {
	stage2df[,PSU]<-factor(stage2df[,PSU])
	x<-stage2df

####Create random numbers
	x$rand<-runif(nrow(x))

#####order by PSU, demo, random no.
	if (!is.na(dem)) {
		x<-x[order(x[,strata],x[,PSU],x[,dem],x$rand),]
		} else	{
			x<-x[order(x[,strata],x[,PSU],x$rand),]
			}
	
####calculate cumulative inclus probs.
	x$b<-cumsum(x[,inclus2])
	x$a[1]<-0
	x$a[2:nrow(x)]<-x$b[1:(nrow(x)-1)]

####calculate a random number between 0 and 1 to start
	r<-runif(1)

####calculate rounded down a value
	x$base<-floor(x$a)

####Add in modified r value
	x$r<-r+x$base

####select sample 
	x$I<-ifelse(x$r>=x$a & x$r<x$b, 1, 0)
	
#create frame with selected individs and tidy up

	data<-x[which(x$I==1),]
	
	data$inclusprob<-data[,inclus1]*data[,inclus2]
	data$sample.weights<-1/data$inclusprob
	data$b<-data$a<-data$I<-data$rand<-data$base<-data$r<-NULL

	if (!is.na(dem)) {data<-data[order(data[,PSU],data[,dem]),]} else {
		data<-data[order(data[,PSU]),]
		}

#Tidy up and save working file
	row.names(data)<-NULL
	data$strat.id<-NULL
	return(data)
	}

