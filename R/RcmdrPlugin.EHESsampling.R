.packageName<-"RcmdrPlugin.EHESsampling"

#Internal functions for EHES plugin

.First.lib <- function(libname, pkgname){ 
	if (!interactive()) return()
	Rcmdr<-options()$Rcmdr
	plugins <- Rcmdr$plugins
	if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
		Rcmdr$plugins <- c(plugins, pkgname)
		options(Rcmdr=Rcmdr)
		closeCommander(ask=FALSE, ask.save=TRUE)
		Commander()
		}
	}


OpenSampleSizes<-function(){
	initializeDialog(title="Installed Dataset Check")
	onOK<- function(){
		closeDialog()
		Sample.notebook()
		}
	OKCancelHelp(helpSubject="sample")
	tkgrid(labelRcmdr(top, text="The PRIMARY SAMPLING UNITS dataset needs to be imported before \nsample sizes are calculated. To proceed click OK.\n"))
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	dialogSuffix(rows=2, columns=3, focus=buttonsFrame)
	}


OpenSampling1<-function(){
	initializeDialog(title="Sampling Stage 1")
	onOK<- function(){
		closeDialog()
		Stage1.notebook()
		}
	OKCancelHelp(helpSubject="sample")
	tkgrid(labelRcmdr(top, text="The PRIMARY SAMPLING UNITS dataset needs to be imported and \nSAMPLE SIZES calculated before stage 1 sampling. To proceed click OK.\n"))
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	dialogSuffix(rows=2, columns=3, focus=buttonsFrame)
	}


OpenSampling2<-function(){
	initializeDialog(title="Sampling Stage 2")
	tkgrid(tklabel(top, text="STAGE 1 SAMPLING needs to be completed and the main UNIT PER LINE \ndataset imported before stage 2 sampling. To continue click OK", justify="left"), columnspan=3, sticky="w")
	onOK<- function(){
		closeDialog()
		Stage2.notebook()
		}
	OKCancelHelp(helpSubject="sample")
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	dialogSuffix(rows=2, columns=3, focus=buttonsFrame)
	}


OpenWeights<-function(){
	tkmessageBox(message="Sorry, the analysis function is not complete")
	tkfocus(CommanderWindow())
	}


mk.calc<-function(x,y,z) {
	mk.opt<-x/y
	mk<-ifelse(mk.opt<2,2,mk.opt)
	mk<-ifelse(z==1,1,mk) 
	mk<-ifelse(z<mk,floor(mk),mk)
	mk<-special.round(mk)
	return(mk)
	}


special.round <-function (x){
	uk<-x-floor(x)
	diff<-sum(x)-floor(sum(x))
	number<-ifelse(diff<0.5,floor(sum(uk)),ceiling(sum(uk)))
	order.x<-order(-uk)
	x<-x[order(-uk)]
	x<-c(ceiling(x[if(number==0) 0 else seq(number)]), 
	floor(x[(number+1):length(x)])) 
	return(x[order(order.x)])
	}


info.bn<-function(frame, text1){
	tk2button(frame, text="Help", width=5, command=function(){
	initializeDialog(title="Information")
	tkgrid(tklabel(top, text=text1, justify="left"))
	tkgrab.set(top)
	tkfocus(top)})
	}

		
DataPreview<- function(Box){
	variables<-listDataSets()
	ds<-variables[as.numeric(tkcurselection(Box))+1]
	if (length(ds)==0) {tkbell()} else {
		initializeDialog(title="Dataset Preview")
		colnum<-min(6,ncol(get(ds)))
		datanames<-names(get(ds))[1:colnum]
		data<-as.matrix(get(ds)[c(1:6),c(1:colnum)])
		data<-rbind(datanames,data)
		for (i in (0:5))
			for (j in (0:(colnum-1)))
				.Tcl(paste("set tclarray(", i, ",", j, ") ", data[i+1, j+1], sep=""))	
		table1<-tk2table(top, variable="tclarray", background="white", rows=6, cols=colnum, colwidth = "15")
		tkpack(table1)
	
		tkbind(top,"<Destroy>",function()tkfocus(CommanderWindow()))
		tkgrab.set(top)
		tkfocus(top)
		}
	}

#Change from a character string of numbers to a vector of real digits
digits<-function(x){
	return(as.real(c(strsplit(as.character(x),""))[[1]]))
	}

#make from vector of digits to single character number
undigits<-function(x){
	return(paste(x, collapse = ""))
	}

#get the transformation of a number function including if longer than 8 digits
#control is true when the control digit is included in the number and false when its not

Perm.fn <- function(x, control=TRUE){
	y<-rev(seq(length(x)))
	if (!control) {y<-y+1}
	if (max(y)>8) {
		y<-y %% 8
		y<-ifelse(y==0, 8, y)
		}
	return(digi$Perm[cbind(y,x+1)])
	}


### Add the transformated numbers function ###
Add.fn<-function(x){	
	n<-x[1]
	for (i in 1:(length(x)-1)){
		n<-digi$Add[n+1,x[i+1]+1]
		}
	return(n)
	}

### Checking Numbers ###
Check.number<-function(x){
	return(Add.fn(Perm.fn(digits(x))))
	}

ListCheck<-function(dataset, x, z){
	data(digi)
	m<-sapply(dataset[,x], Check.number)	
	n<-sapply(dataset[,x], nchar)
	y<-rbind(dataset[which(m!="0"),], dataset[which(n!=z),])
	if (nrow(y)>0) {
		y<-unique(y)
		y<-y[order(y[,x]),]
		row.names(y)<-NULL		
		return(y)
		} else return()
	}


### Add control digits to a list of numbers
Add.control<-function(dataset, x){
	data(digi)
	lmax<-max(sapply(dataset[,x], function(y)length(digits(y))))
	z<-as.double(dataset[,x])
	l<-sapply(z, function(y)length(digits(y)))
	z[which(l<lmax)]<-sprintf(paste("%0", lmax, "d", sep=""), z[which(l<lmax)])
	z<-sapply(z, function(x){
	m<-Add.fn(Perm.fn(digits(x), control=FALSE))
	z<-undigits(c(as.character(x), digi$Inv[m+1]))
	return(z)})
	return(z)
	}


###Create a list of encoded numbers ###
Create.control<-function(x, n){
	data(digi)
	z<-sprintf(paste("%0", x-1, "d", sep=""), seq(1:n))
	z<-sapply(z, function(x){
	m<-Add.fn(Perm.fn(digits(x), control=FALSE))
	z<-undigits(c(as.character(x), digi$Inv[m+1]))
	return(z)})
	names(z)<-NULL
	return(z)
	}


# General Disabling function
disable.fn<-function(x,y,z){ 
	if (tclvalue(x)=="1") {
		tkconfigure(y, state="disabled")
		tkconfigure(z, state="disabled")
		}
	if (tclvalue(x)=="0") {
		tkconfigure(y, state="normal")
		tkconfigure(z, state="normal")
		}
	}


#General changing of state in widgets
setstate<-function(x, y) for (i in 1:length(x)) tkconfigure(x[[i]], state=y)
