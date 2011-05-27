Stage1.notebook<-function() {
	one<-inclus<-samplingtypeFrame<-run1<-NULL
	initializeDialog(title="Stage 1 Sampling")
	variables2<-names(get(activeDataSet()))

# PSU Box
	PSUselection<-ifelse(is.na(match("PSU_SN", variables2)), NA, match("PSU_SN", variables2)-1)
	PSUBox<-variableListBox2(top, variableList=variables2, title="Select the PRIMARY SAMPLING UNIT variable:",
		initialSelection=PSUselection)
	tkgrid(getFrame(PSUBox), sticky="w")
	
# Strata Box
	strataselection<-ifelse(is.na(match("STRATUM_ID", variables2)), NA, match("STRATUM_ID", variables2)-1)
	strataBox<-variableListBox2(top, variableList=variables2, title="Select the STRATIFICATION variable:",
		initialSelection=strataselection)
	tkblank(top)
	tkgrid(getFrame(strataBox), sticky="w")

# inclusion probability Box
	inclusselection<-ifelse(is.na(match("ST1_PROB", variables2)), NA, match("ST1_PROB", variables2)-1)
	inclusBox<-variableListBox2(top, variableList=variables2, title="Select the STAGE 1 INCLUSION PROBABILITY variable:",
		initialSelection=inclusselection)
	tkblank(top)
	tkgrid(getFrame(inclusBox), sticky="w")

# Sampling type variable
	radioButtons(top, name="samplingtype", buttons=c("Maxentropy","system"), values=c(1,2), 
		labels=c("Maximum entropy sampling (recommended) ","Systematic Sampling"), 
		title="What type of sampling do you want to do?",right.buttons=FALSE)
	tkblank(top)
	tkgrid(samplingtypeFrame, sticky="w")

# Randomise PSUs
	RandomValue<-tclVar("1")
	RandomCB<-tkcheckbutton(top, text="Randomize the PSU order within each stratum")
	tkconfigure(RandomCB, variable=RandomValue)
	tkblank(top)
	tkgrid(RandomCB, sticky="w")	

# On ok
onOK<-function(){
	initializeDialog(title="Run Stage 1 Sampling?", window=run1)
	buttonsFrame<-tkframe(run1)
	
	report<-tclVar("1")
	reportCB<-tkcheckbutton(run1,text="Write report of Sampling Stage One details called:", command=function()filedisable())
	tkconfigure(reportCB, variable=report)
	reportname<-tclVar("Stage1_Report")
	fileField<-tkentry(run1, width="20", textvariable=reportname)
	
	tkgrid(tklabel(run1, text="Do you want to proceed with Stage 1 Sampling?"), sticky="w")
	tkgrid(tklabel(run1,text="Click OK to proceed with Stage 1 sampling or cancel to revise options"), sticky="w", columnspan=2)
	tkgrid(tklabel(run1, text=""))
	tkgrid(reportCB, fileField, sticky="w")
	tkgrid(tklabel(run1, text=""))

	filedisable<-function(){
		if (tclvalue(report)==0) tkconfigure(fileField, state="disabled")
		if (tclvalue(report)==1) tkconfigure(fileField, state="normal")
		}

	onOK<-function(){
		reportyesno<-tclvalue(report)
		reportname<-tclvalue(reportname)
		closeDialog(run1)
		stage1df<-activeDataSet()
		PSU <- as.double(tkcurselection(PSUBox$listbox))+1
		strata <- as.double(tkcurselection(strataBox$listbox))+1
		inclus<- as.double(tkcurselection(inclusBox$listbox))+1
		RandomPSU<-tclvalue(RandomValue)
		if (tclvalue(samplingtypeVariable)=="1") {samplingtype<-"UPmaxentropy1"} else {samplingtype<-"UPsystematic"}

# Add in Checks 
		if (length(PSU)==0) {check.fn2("Primary Sampling Unit")
			return()}
		if (length(strata)==0) {check.fn2("STRATIFICATION")
			return()}
		if (length(inclus)==0) {check.fn2("First Stage Inclusion Probability")
			return()}

# Report
		closeDialog()
		if (reportyesno==1) {
			df1names<-names(get(stage1df))
			cat("Documentation for EHES sampling - Stage 1 Sampling\n\nDate:",date(),"\nR Version:", paste(getRversion()),"\nRcmdrPlugin.EHESsampling Version:", packageDescription("RcmdrPlugin.EHESsampling")$Version,"\nWorking Directory:", getwd(),
			"\n\nDatasets...\nThe dataset containing Primary Sampling Units and Sample Sizes was called:", stage1df,
			"\n\nVariable Details...\nThe Primary Sampling Unit variable was:", df1names[PSU],
			"\nThe stratitification variable was:", df1names[strata],
			"\nThe First Stage Inclusion Probability variable was:", df1names[inclus],
			"\nThe Sampling Method was:", samplingtype,
			ifelse(RandomPSU==0, "\nPSUs were not randomly ordered within each stratum", "\nPSUs were randomly ordered within each stratum"),
			"\n",file=reportname)
			}
		doItAndPrint(paste("Stage1Sample <- stage1Sample.fn(stage1df=",stage1df,", PSU=",PSU,", strata=",strata,", inclus=",inclus, ", samplingtype=",samplingtype,", randomPSU=", RandomPSU,")\n",sep=""))
		activeDataSet("Stage1Sample")
		View(Stage1Sample)
		}

	OKCancelHelp2(window=run1, onHelp=tkbell)
	tkgrid(buttonsFrame, columnspan="2", sticky="w")	
	dialogSuffix(rows=3, columns=2, window=run1, focus=buttonsFrame)
	}

OKCancelHelp2(window=top, onHelp=Stage1onHelp)
tkgrid(buttonsFrame, columnspan="2", sticky="w")	
dialogSuffix(rows=3, columns=2, window=top, focus=buttonsFrame)
}
