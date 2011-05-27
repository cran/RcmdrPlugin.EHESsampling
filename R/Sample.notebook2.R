
### PSU DETAILS ###
PSU.options <- function(df1, df2, stratayesno) {
	initializeDialog(title="PSU Dataset Options")
	variables2<-names(get(df1))

#PSU List Box - PSU dataframe
	PSUBox<-variableListBox2(top, variableList=variables2, title="The PSU variable is:")

#Size list box - PSU dataframe
	sizeBox<-variableListBox2(top, variableList=variables2, title="The SIZE variable is:")
	
#Strata Strata List Box
	strataBox<-variableListBox2(top, variableList=variables2, title="The STRATIFICATION variable is:")

#Demographics
	demoBox<-variableListBox2(top, variableList=variables2, selectmode="multiple", title="The stage 2 stratification variables are: [select all] ")
	demoValue<-tclVar("0")
	demoCB<-tkcheckbutton(top, text="No second stage stratification Variables", command=function()disable.fn2(demoValue, demoBox))
	tkconfigure(demoCB, variable=demoValue)

#Grid
	tkgrid(getFrame(PSUBox), sticky="w")
	tkblank(top)
	tkgrid(getFrame(sizeBox), sticky="w")
	tkblank(top)
	tkgrid(getFrame(strataBox), sticky="w")
	tkblank(top)
	tkgrid(getFrame(demoBox), sticky="w")
	tkgrid(demoCB, sticky="w")
	tkblank(top)

#forward function
	onOK<-function(){
		PSU1 <- match(getSelection(PSUBox), variables2)
	 	strata1 <- match(getSelection(strataBox), variables2)
	 	size1 <- match(getSelection(sizeBox), variables2)
		demoyesno <- tclvalue(demoValue)
		demo1 <- ifelse(demoyesno==0, match(getSelection(demoBox), variables2), "")
		if (length(strata1)==0) {check.fn2("STRATIFICATION") 
			return()}
		if (length(PSU1)==0) {check.fn2("PSU")
			return()}
		if (length(size1)==0) {check.fn2("SIZE")
			return()}
		if (is.na(demo1)) {check.fn2("STAGE 2 STRATIFICATION") 
			return()}
		tkdestroy(top)
		strata.options(df1, df2, stratayesno, PSU1, strata1, size1, demoyesno, demo1)
		return()
	}

#help and buttons
	OKCancelHelp2(window=top, onHelp=PSUonHelp)
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	dialogSuffix(rows=3, window=top, focus=buttonsFrame)
	}


### Strata Options window ###

strata.options<-function(df1, df2, stratayesno, PSU1, strata1, size1, demoyesno, demo1) {
	initializeDialog(title="Stratification Dataset Options")
	if (stratayesno==0) {variables<-names(get(df2))} else { variables<-names(get(df1))}

# autoSelection between PSU and strata Datasets for strata variable
	strata.selection<-NULL
	if (stratayesno==0) {
		strata.variable<-names(get(df1))[strata1]
		strata.selection<-ifelse(is.na(match(strata.variable, variables)), NA, match(strata.variable,variables)-1)
		}

#Strata List Box
	strataBox<-variableListBox2(top, variableList=variables, title="The STRATIFICATION variable is:",
		initialSelection = strata.selection)

#mk q and listbox
	radioButtons2(top, name="mkValue", buttons=c("rb1", "rb2"), values=c(0,1),
		labels=c("yes                                                         ", "no"), right.buttons=FALSE, 
		title="Is there a STAGE 1 SAMPLE SIZE variable?               ", click.command=function()disable.fn2(mkValueVariable, mkBox))
	mkBox<-variableListBox2(top, variableList=variables, title="The stage 1 sample size variable is:")
	
#Strata Sizes
	radioButtons2(top, name="ssValue", buttons=c("rbss1", "rbss2"),values=c(0,1),
		labels=c("yes - the data set contains one", "no - this needs calculating"), 
		right.buttons=FALSE, title="Is there a STRATA SAMPLE SIZE variable?", click.command=function()ssdisable())
	ssBox<-variableListBox2(top, variableList=variables, title="The strata sample size variable is:")

#Total Sample Size
	allocframe<-tkframe(top, borderwidth=2, relief="groove")
	ssframe<-tkframe(allocframe)
	sstot<-tclVar(4000)
	ssField<-tkentry(ssframe, width="10", textvariable=sstot)
	sstotallabel<-tklabel(ssframe, text="The total sample size is approximately:", fg="blue")
	
#Allocation options
	radioButtons2(allocframe, name="ppsValue", buttons=c("rbpps1", "rbpps2", "rbpps3", "rbpps4"), values=c(1,2,3,4),
		labels=c("Proportional to size", "Proportional to size with a minimum in each stratum of: ", 
		"Fixed sample size per stratum", "Neymans allocation"), title="How is the sample distributed among the STRATA?",
		right.button=FALSE, click.command=function()psdisable())
	minsize<-tclVar(1000)
	minField<-tkentry(allocframe, width="5", textvariable=minsize)
	
#Gridding
	tkgrid(getFrame(strataBox), sticky="w")
	tkblank(top)
	tkgrid(mkValueFrame, getFrame(mkBox), sticky="nw")
	tkblank(top)
	tkgrid(ssValueFrame, getFrame(ssBox), sticky="nw")
	tkgrid(sstotallabel, ssField, columnspan=1, sticky="w")
	tkgrid(ssframe, sticky="w")
	tkgrid(tklabel(allocframe, text=""))
	tkgrid(ppsValueFrame, minField, sticky="w",columnspan=1)
	tkblank(top)
	tkgrid(allocframe, sticky="w", columnspan=2)

#forward function
	onOK<-function(){
		strata2<-match(getSelection(strataBox), variables)
		mkvar<-match(getSelection(mkBox), variables)
		mkvalue<-tclvalue(mkValueVariable) #0 is strataset contains mk value, 1 is optim
		ssyesno<-tclvalue(ssValueVariable) # 1 for no sss variable, 0 for sss variable
		ssvar<-match(getSelection(ssBox), variables)
		sstotal<-as.numeric(tclvalue(sstot)) #total sample size
		ppstype<-tclvalue(ppsValueVariable) #1 for PPS, 2 for PPS with min, 3 for fixed
		nminimum<-as.numeric(tclvalue(minsize))
		if (length(strata2)==0) {check.fn2("STRATIFICATION") 
			return()}
		if (length(mkvar)==0 & mkvalue==0) {check.fn2("Stage 1 sample size")
			return()}
		if (length(ssvar)==0 & ssyesno==0) {check.fn2("Strata sample size")
			return()}
		tkdestroy(top)
		costvar.options(df1, df2, stratayesno, PSU1, strata1, size1, demoyesno, demo1,
			strata2, mkvar, mkvalue, ssyesno, ssvar, sstotal, ppstype, nminimum)
	}

#disable functions - title not working
	ssdisable<-function(){
		box.vars<-list(sstotallabel, ssField, rbpps1Button, rbpps2Button, rbpps3Button, rbpps4Button,
			rbpps1Label, rbpps2Label, rbpps3Label,  rbpps4Label,  ppsValueTitle, minField)
		if (tclvalue(ssValueVariable)==0) {
			setstate2(box.vars, "disabled")
			setstate(ssBox, "normal") }
		if (tclvalue(ssValueVariable)==1) {
			setstate2(box.vars, "normal")
			setstate(ssBox, "disabled")
			psdisable()}
		}

	psdisable<-function(){
		if (tclvalue(ppsValueVariable)==2) tkconfigure(minField,state="normal")
		if (tclvalue(ppsValueVariable)!=2) tkconfigure(minField,state="disabled")
	}
	ssdisable()

#help and buttons
	tkblank(top)
	OKCancelHelp2(window=top, onHelp=strataonHelp)
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	dialogSuffix(rows=3, window=top, focus=buttonsFrame)
	}





### cost variance variables ###
costvar.options <- function(df1, df2, stratayesno, PSU1, strata1, size1, demoyesno, demo1,
			strata2, mkvar, mkvalue, ssyesno, ssvar, sstotal, ppstype, nminimum) {
	
	initializeDialog(title= "Cost and variance Options")
	if (stratayesno==0) {variables<-names(get(df2))} else { variables<-names(get(df1))}
	variables2<-names(get(df1))

#Button disable

costdisable<-function(){
	disable.fn2(costValue, PSUcostBox)
	disable.fn2(costValue, ParcostBox)
	}
variabledisable<-function(){
	disable.fn2(optimalValue, PSUmeanBox)
	disable.fn2(optimalValue, PSUVBox)
	}

#Cost boxes
	PSUcostBox<-variableListBox2(top, variableList=variables, title="The PSU COST variable is:")
	ParcostBox<-variableListBox2(top, variableList=variables, title="The PARTICIPANT COST variable is:")
	costValue<-tclVar("0")
	costCB<-tkcheckbutton(top, text="No cost information is available",command=function()costdisable())
	tkconfigure(costCB, variable=costValue)

#optimal variables boxes
	PSUmeanBox<-variableListBox2(top, variableList=variables2, title="The PSU MEAN variable is:")
	PSUVBox<-variableListBox2(top, variableList=variables2, title="The PSU VARIANCE variable is:")
	optimalValue<-tclVar("0")
	optimalCB<-tkcheckbutton(top,text="No variable information is available", command=function()variabledisable())
	tkconfigure(optimalCB, variable=optimalValue)

#Gridding
	tkgrid(getFrame(PSUcostBox), getFrame(ParcostBox), sticky="w")
	if (ssyesno==1 & ppstype==1 | ssyesno==1 & ppstype==2) {} else	tkgrid(costCB, sticky="w", columnspan=2)
	tkgrid(tklabel(top, text="                                                             "))	
	tkgrid(getFrame(PSUmeanBox), getFrame(PSUVBox), sticky="w")
	if (ssyesno==1 & ppstype==1 | ssyesno==1 & ppstype==2) {} else	tkgrid(optimalCB, sticky="w", columnspan=1)
	tkblank(top)


	onOK<-function(){
		costyesno<-tclvalue(costValue) #0 is there is cost information, 1 is no information
		optyesno<-tclvalue(optimalValue)  #0 is there are variable information, 1 is no information 
		Parcost <- match(getSelection(ParcostBox), variables)
		PSUcost <- match(getSelection(PSUcostBox), variables)
		PSUV<-match(getSelection(PSUVBox), variables2)
		PSUmean<-match(getSelection(PSUmeanBox), variables2)

#Checks
		if (length(PSUV)==0 & optyesno==0) {check.fn2("PSU VARIANCE") 
			return()}
		if (length(PSUV)==0 & ppstype==4) {check.fn2("PSU VARIANCE") 
			return()}
		if (length(PSUmean)==0 & optyesno==0) {check.fn2("PSU MEAN") 
			return()}
		if (length(Parcost)==0 & mkvalue==1) {check.fn2("PARTICIPANT COST") 
			return()}
		if (length(PSUcost)==0 & mkvalue==1) {check.fn2("PSU COST") 
			return()}
	#check for strata name mismatches between datasets
		if (stratayesno==0){
			bad<-is.element(get(df1)[,strata1], get(df2)[,strata2])
			bad2<-is.element(get(df2)[,strata2], get(df1)[,strata1])
			if (is.element("FALSE", bad)|is.element("FALSE", bad2)) {
				tkmessageBox(message="Not all of the strata names match between the two datasets provided. Please check them!")
				return()
				}
			}
		closeDialog(top)

		initializeDialog(title="Calculate Sample Sizes?", window=samplecheck)
		buttonsFrame<-tkframe(samplecheck)
		report<-tclVar("1")
		reportCB<-tkcheckbutton(samplecheck,text="Write report of sample size calculation details called:", command=function()filedisable())
		tkconfigure(reportCB, variable=report)
		reportname<-tclVar("Sample_Sizes_Report")
		fileField<-tkentry(samplecheck, width="20", textvariable=reportname)
		
		tkgrid(tklabel(samplecheck, text="Do you want to proceed with calculating sample sizes?"), sticky="w")
		tkgrid(tklabel(samplecheck,text="Click OK to proceed with calculating sample sizes or cancel to revise options"), sticky="w", columnspan=2)
		tkgrid(tklabel(samplecheck, text=""))
		tkgrid(reportCB, fileField, sticky="w")
		tkgrid(tklabel(samplecheck, text=""))

		filedisable<-function(){
			if (tclvalue(report)==0) tkconfigure(fileField, state="disabled")
			if (tclvalue(report)==1) tkconfigure(fileField, state="normal")
		}

		onOK<-function(){
			reportyesno<-tclvalue(report)
			reportname<-tclvalue(reportname)	
			closeDialog(samplecheck)
		command<-paste("SampleSizes(df1=",df1, ", df2=",df2,", PSU1=",PSU1,", strata1=",strata1,", size1=",size1,", demo1=",list(demo1),", demoyesno=", demoyesno, 
		", strata2=",strata2,", stratayesno=", stratayesno,
		", mkvar=", mkvar,", mkvalue=", mkvalue,", ssyesno=", ssyesno, ", ssvar=", ssvar, ", sstotal=", sstotal, ", ppstype=", ppstype,
		", nminimum=", nminimum, ", Parcost=", Parcost, ", PSUcost=",PSUcost,
		", PSUV=",PSUV,", PSUmean=", PSUmean, ", optyesno=", optyesno, ", costyesno=", costyesno, ")\n",sep="")
		samplesizedatasets<-justDoIt(command)

       ### Add in report ###
	if (reportyesno==1) {
		
		if (costyesno==0) costtotal<-round(sum(as.double(samplesizedatasets$StrataSS$ST1_COST)))
		df1names<-names(get(df1))
		if (stratayesno==0) df2names<-names(get(df2))
		if (stratayesno==1) df2names<-names(get(df1))
		
		cat("Documentation for EHES sampling - Calculating Sample Sizes\n\nDate:",date(),"\nR Version:", paste(getRversion()),"\nRcmdrPlugin.EHESsampling Version: ", packageDescription("RcmdrPlugin.EHESsampling")$Version, "\nWorking Directory:", getwd(),
		"\n\nDatasets...\nThe dataset containing Primary Sampling Units was called:", df1,
		ifelse(stratayesno==0, "\nThe dataset containing stratification information was called:", "\nNo separate stratification dataset was used"), 
		if (stratayesno==0) df2,
		"\n\nPSU Dataset Details...\nThe Primary Sampling Unit variable was:", df1names[PSU1], "\nThe Size variable was: ", df1names[size1],
		if (demoyesno==0) c("\nThe second stage stratification (domain) variables were: ", df1names[demo1]),
		if (demoyesno==1) "\nNo second stage stratification (domain) variables were used",
		if (stratayesno==0) c("\n\nStratification Dataset Details...\nThe stratitfication variable was: ", df2names[strata2]), 
		if (mkvalue==0) "\nThe stage 1 sample size variable was called:",
		if (mkvalue==0) ifelse(stratayesno==0, df2names[mkvar], df1names[mkvar]),
		if (mkvalue==1) "\nOptimisation of stage 1 sample sizes was selected",
		if (ssyesno==0) "\nThe datset contained the strata sample sizes variable: ",
		if (ssyesno==0) ifelse(stratayesno==0, df2names[ssvar], df1names[ssvar]),
		if (ssyesno==1) c("\nThe dataset did not contain a strata sample size variable"),
		if (ssyesno==1 & ppstype==1) "\nThe strata sample sizes were selected to be distributed proportional to size",
		if (ssyesno==1 & ppstype==2) c("\nThe strata sample sizes were selected to be distributed proportional to size with a minumum of: ", nminimum),
		if (ssyesno==1 & ppstype==3) c("\nThe strata sample sizes were selected to be a fixed size of: ", nfixed),
		if (ssyesno==1 & ppstype==4) c("\nThe strata sample sizes were selected to distributed using Neymans Allocation"),
		if (mkvalue==1 & optyesno==0) c("\nOptimal sample sizes were calculated using the PSU dataset mean and variance variables: ", df1names[PSUmean], df1names[PSUV]),
		if (costyesno==0) c("\nThe cost was calculated using the variables: ", df2names[PSUcost], df2names[Parcost]),
		"\n\nAdditional Details...\nThe total sample size was: ", sstotal,
		if (costyesno==0) c("\nThe total cost came to: ", costtotal),
		if (optyesno==0 & demoyesno==1) c("\nThe weighted mean for the key variable is estimated as: ", signif(T.Var,digits=4), "\nThe overall variance for the key variable is estimated as: ", signif(TotalVar, digits=4)),
		if (optyesno==0 & demoyesno==1) c("\nThe Coefficient of Variation (as a percentage) for the key variable is estimated as: ", signif(COEF, digits=4), "\nThe Design effect is estimated as: ", signif(Deff, digits=4)),
		"\n", file=reportname)
		}
		doItAndPrint(paste("samplesizedatasets<-", command, sep=""))
		doItAndPrint("StrataSampleSize<-samplesizedatasets$StrataSS")
		doItAndPrint("PSUSampleSize<-samplesizedatasets$PSUSS\n")
		activeDataSet("PSUsampleSize")
		View(samplesizedatasets$StrataSS, title="StrataSampleSize")
		tkfocus(CommanderWindow())
		}
	tkblank(samplecheck)
	OKCancelHelp2(window=samplecheck, onHelp=reportonHelp)
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	dialogSuffix(rows=3, window=samplecheck, focus=buttonsFrame)
	}

#help and buttons
	tkblank(top)
	OKCancelHelp2(window=top, onHelp=costonHelp)
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	dialogSuffix(rows=3, window=top, focus=buttonsFrame)
}
