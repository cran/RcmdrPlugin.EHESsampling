Stage1.notebook<-function() {
	one<-inclus<-samplingtypeFrame<-run1<-NULL

	initializeDialog(title="Stage 1 Sampling")
	nb<-tk2notebook(top, tabs="Stage 1 Sampling")
	tkpack(nb, fill="both", expand=2)
	tkconfigure(nb, width=450)
	tkgrid(nb,columnsp=2)
	
	buttons.frame<-tkframe(top)
	next.bn<-tk2button(buttons.frame, text="Next >>>",command=function()sampling1.run())
	tkgrid(next.bn)
	tkgrid(buttons.frame, sticky="e", columnspan=2)


### DATASET SELECTION
	stage1<-tk2notetab(nb, "Stage 1 Sampling")
	variables<-listDataSets()
	selection<-ifelse(is.na(match("PSUSampleSize", variables)), "NOT FOUND", "PSUSampleSize")
	buttonlabel<-tclVar("Select Dataset...")
	if (selection!="NOT FOUND"){
		tclvalue(buttonlabel)<-"Change Dataset..."
		}
	datasetframe<-tkframe(stage1)
	databuttonframe<-tkframe(stage1)
	data.bn<-tk2button(databuttonframe, text=tclvalue(buttonlabel), command=function()getdataset())
	tkconfigure(data.bn, textvariable=buttonlabel)
	data.label<-tklabel(datasetframe, text="The dataset to sample from is:", fg="blue")
	datasettext<-tclVar(selection)
	dataset.label<-tklabel(datasetframe, text=tclvalue(datasettext))
	tkconfigure(dataset.label, textvariable=datasettext)

	tkgrid(tklabel(stage1, text=""))
	tkgrid(data.label, dataset.label)
	tkgrid(data.bn)
	tkgrid(datasetframe, databuttonframe, sticky="ew",columnspan=1)

	getdataset<-function(){
		initializeDialog(title="Select Dataset", window=getdata)
		buttonsFrame<-tkframe(getdata)
		label<-tklabel(getdata, text="Select the PRIMARY SAMPLING UNIT dataset to sample from:", fg="blue")
		dataframe<-tkframe(getdata)
		datascr <- tkscrollbar(dataframe, repeatinterval=5,command=function(...)tkyview(dataBox,...))
		dataBox<-tklistbox(dataframe, height=8, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(datascr,...),background="white")
		view.but<-tk2button(getdata, text="dataset preview", command=function()DataPreview(dataBox))
		datainfotext<-"The Primary Sampling Unit (PSU) dataset should contain a list of all the PSUs and \ntheir sample sizes and inclusion probabilities. If second stage stratifications \n(domains) are used, the dataset should have one line for each domain in each PSU."
		infodata<-info.bn(getdata, datainfotext)
		tkgrid(label, infodata, sticky="w")
		tkgrid.configure(dataBox,datascr, sticky="nsw")
		tkgrid(dataframe, view.but, sticky="w")
		tkgrid(tklabel(getdata, text=""))
		for (i in 1:length(variables)) tkinsert(dataBox, "end", variables[i])
		onOK<-function(){
			selection<-variables[as.numeric(tkcurselection(dataBox))+1]
			if (length(selection)>0) {tclvalue(buttonlabel)<-"Change Dataset..."}
			tclvalue(datasettext)<-selection
			getvariables(selection)
			closeDialog(getdata)
			}
		OKCancelHelp(helpSubject="sample", window=getdata)
		tkgrid(buttonsFrame, columnspan="2", sticky="w")
		dialogSuffix(rows=2, columns=3, window=getdata, focus=buttonsFrame)
		}

### PSU List Box
	PSUlabel<-tklabel(stage1,text="Select the PRIMARY SAMPLING UNIT variable", fg="blue")
	PSUframe<-tkframe(stage1)
	PSUscr <- tkscrollbar(PSUframe, repeatinterval=5,command=function(...)tkyview(PSUBox,...))
	PSUBox<-tklistbox(PSUframe, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(PSUscr,...),background="white")
	tkgrid.configure(PSUBox, PSUscr)

### Strata Box
	stratalabel<-tklabel(stage1,text="Select the STRATIFICATION variable", fg="blue")
	strataframe<-tkframe(stage1)
	stratascr <- tkscrollbar(strataframe, repeatinterval=5,command=function(...)tkyview(strataBox,...))
	strataBox<-tklistbox(strataframe, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(stratascr,...),background="white")
	tkgrid.configure(strataBox, stratascr)


### inclusion probability label
	inclusframe<-tkframe(stage1)
	inclusbuttonframe<-tkframe(stage1)
	variables2<-ifelse(tclvalue(datasettext)=="NOT FOUND", " ", names(get(tclvalue(datasettext))))
	inclusselection<-ifelse(is.na(match("stage1.prob",variables2)), "NOT FOUND", "stage1.prob")
	inclusbuttonlabel<-tclVar("Select Variable...")
	if (inclusselection!="NOT FOUND"){
		tclvalue(inclusbuttonlabel)<-"Change Variable..."
		}
	inclus.bn<-tk2button(inclusbuttonframe, text=tclvalue(inclusbuttonlabel), command=function()getvariable())
	tkconfigure(inclus.bn, textvariable=inclusbuttonlabel)
	inclus.label<-tklabel(inclusframe, text="The FIRST STAGE INCLUSION PROBABILITY variable is:", fg="blue")
	incluslabeltext<-tclVar(inclusselection)
	variable.label<-tklabel(inclusframe, text=tclvalue(incluslabeltext))
	tkconfigure(variable.label, textvariable=incluslabeltext)

	tkgrid(inclus.label, variable.label)
	tkgrid(inclus.bn)


getvariable<-function(){
	initializeDialog(title="Inclusion Probability Variable", window=inclus)
	buttonsFrame<-tkframe(inclus)
	if (tclvalue(datasettext)=="NOT FOUND") variables2<-""
	if (tclvalue(datasettext)!="NOT FOUND") variables2<-names(get(tclvalue(datasettext)))
	inclusBox<-variableListBox(inclus, variables2, title="Select the FIRST STAGE INCLUSION PROBABILITY variable:")
	inclustext<-"The first stage inclusion probability is how likely each PSU is to being \nselected for sampling in the first stage. This can be calculated with \nthe sample sizes option in the RcmdrPluing.EHES package."
	infoinclus<-info.bn(inclus, inclustext)
	tkgrid(getFrame(inclusBox),infoinclus, sticky="w")
	tkgrid(tklabel(inclus, text=""))

	onOK<-function(){
		selection<-getSelection(inclusBox)
		if (length(selection)>0) {tclvalue(inclusbuttonlabel)<-"Change Variable..."}
		tclvalue(incluslabeltext)<-selection
		closeDialog(inclus)
		}
	OKCancelHelp(helpSubject="sample", window=inclus)
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	dialogSuffix(rows=2, columns=3, window=inclus, focus=buttonsFrame)
	}



### Sampling type variable
	typeframe<-tkframe(stage1)
	radioButtons(typeframe, name="samplingtype", buttons=c("Maxentropy","system"), values=c(1,2), labels=c("Maximum entropy sampling (recommended) ","Systematic Sampling"), 
	title="What type of sampling do you want to do",right.buttons=FALSE)
	tkgrid(samplingtypeFrame, sticky="w")

### Randomise PSUs
	RandomValue<-tclVar("1")
	RandomCB<-tkcheckbutton(stage1,text="Randomize the PSU order within each stratum")
	tkconfigure(RandomCB, variable=RandomValue)


### Info buttons
	PSUtext<-"The PSU variable should contain a unique identifying number or name \nfor each Primary Sampling Unit."
	infoPSU<-info.bn(stage1, PSUtext)
	Stratatext<-"The STRATA variable should contain a unique identifying number or name \nfor each STRATUM. If you are sampling every PSU, this is called a \none stage sample. This type of sample is not supported by this program yet."
	infoStrata<-info.bn(stage1, Stratatext)

### Gridding
	tkgrid(PSUlabel, infoPSU, sticky="w")
	tkgrid(PSUframe, sticky="w")
	tkgrid(tklabel(stage1,text=""))
	tkgrid(stratalabel, infoStrata, sticky="w")
	tkgrid(strataframe,sticky="w")
	tkgrid(tklabel(stage1,text="                                                                                                                  "), columnspan=1)
	tkgrid(inclusframe, inclusbuttonframe, sticky="w", columnspan=1)
	tkgrid(tklabel(stage1,text=""))
	tkgrid(typeframe, sticky="w")
	tkgrid(tklabel(stage1,text=""))
	tkgrid(RandomCB, sticky="w")
	tkgrid(tklabel(stage1,text=""))


### Add in Variables getvariables()
getvariables<-function(selection){
	if (selection!="NOT FOUND"){
	variables2<-names(get(tclvalue(datasettext)))
	tkdelete(PSUBox,0,1000)
	tkdelete(strataBox,0,1000)
	for (i in 1:length(variables2)) tkinsert(PSUBox, "end", variables2[i])
	for (i in 1:length(variables2)) tkinsert(strataBox, "end", variables2[i])
	inclusselection<-ifelse(is.na(match("stage1.prob",variables2)), "NOT FOUND", "stage1.prob")
	if (inclusselection!="NOT FOUND"){
		tclvalue(inclusbuttonlabel)<-"Change Variable..."
		}
	tclvalue(incluslabeltext)<-inclusselection
	}}


### Add in variables if no clicking
	if (selection!="NOT FOUND"){
		variables2<-names(get(tclvalue(datasettext)))
		for (i in 1:length(variables2)) tkinsert(PSUBox, "end", variables2[i])
		for (i in 1:length(variables2)) tkinsert(strataBox, "end", variables2[i])
		inclusselection<-ifelse(is.na(match("stage1.prob",variables2)), "NOT FOUND", "stage1.prob")
		if (inclusselection!="NOT FOUND"){
			tclvalue(inclusbuttonlabel)<-"Change Variable..."
			}
		tclvalue(incluslabeltext)<-inclusselection
	}


### On ok
sampling1.run<-function(){
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
		stage1df<-tclvalue(datasettext)
		PSU <- as.numeric(tkcurselection(PSUBox))+1
		strata <- as.numeric(tkcurselection(strataBox))+1
		inclus<-match(tclvalue(incluslabeltext), names(get(stage1df)))
		RandomPSU<-tclvalue(RandomValue)
		if (tclvalue(samplingtypeVariable)=="1") {samplingtype<-"UPmaxentropy"} else {
			samplingtype<-"UPsystematic"
			}

# Add in Checks #
	check.fn<-function(v, nbtab){
			mes<-paste("Please select a ", v, " variable", sep="")
 			tkmessageBox(message=mes)
			tk2notetab.select(nb, nbtab)		
		}

		if (stage1df=="NOT FOUND") {check.fn("dataset", "Stage 1 Details")
			return()}
		if (length(PSU)==0) {check.fn("Primary Sampling Unit", "Stage 1 Details")
			return()}
		if (length(strata)==0) {check.fn("STRATIFICATION", "Stage 1 Details")
			return()}
		if (inclus=="NA") {check.fn("First Stage Inclusion Probability", "Stage 1 Details")
			return()}

		closeDialog()
		if (reportyesno==1) {
			df1names<-names(get(stage1df))
			cat("Documentation for EHES sampling - Stage 1 Sampling\n\nDate:",date(),"\nR Version:", paste(getRversion()),"\nRcmdrPlugin.EHESsampling Version:", packageDescription("RcmdrPlugin.EHESsampling")$Version,"\nEHESsampling Version: 2.0\nWorking Directory:", getwd(),
			"\n\nDatasets...\nThe dataset containing Primary Sampling Units and Sample Sizes was called:", stage1df,
			"\n\nVariable Details...\nThe Primary Sampling Unit variable was:", df1names[PSU],
			"\nThe stratitification variable was:", df1names[strata],
			"\nThe First Stage Inclusion Probability variable was:", df1names[inclus],
			"\nThe Sampling Method was:", samplingtype,
			ifelse(RandomPSU==0, "\nPSUs were not randomly ordered within each stratum", "\nPSUs were randomly ordered within each stratum"),
			"\n",file=reportname)
			}
		doItAndPrint(paste("Stage1Sample <- stage1Sample.fn(stage1df=",stage1df,", PSU=",PSU,", strata=",strata,", inclus=",inclus, ", samplingtype=",samplingtype,", randomPSU=", RandomPSU,")\n",sep=""))
		View(Stage1Sample)
		}

	OKCancelHelp(helpSubject="sample", window=run1)
	tkgrid(buttonsFrame, columnspan="2", sticky="w")	
	dialogSuffix(rows=3, columns=2, window=run1, focus=buttonsFrame)
	}

	onOK<-function(){}
	OKCancelHelp(helpSubject="sample")
	dialogSuffix(focus=next.bn)
}