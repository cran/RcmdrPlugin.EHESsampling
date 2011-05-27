Stage2.notebook<-function() {
	top<-NULL
	initializeDialog(title="Sampling Stage 2", window=top)
	variables2<-names(get(activeDataSet()))

# PSU List Box
	PSUselection<-ifelse(is.na(match("PSU_SN", variables2)), NA, match("PSU_SN", variables2)-1)
	PSUBox<-variableListBox2(top, variableList=variables2, title="Select the PRIMARY SAMPLING UNIT variable:", 
		initialSelection=PSUselection)
	tkgrid(getFrame(PSUBox), sticky="w")

# Strata Variable
	strataselection<-ifelse(is.na(match("STRATUM_ID", variables2)), NA, match("STRATUM_ID", variables2)-1)
	strataBox<-variableListBox2(top, variableList=variables2, title="Select the STRATIFICATION variable:",
		initialSelection=strataselection)
	tkblank(top)
	tkgrid(getFrame(strataBox), sticky="w")

# Domain variable function
	demselection<-ifelse(is.na(match("DOMAIN_ID", variables2)), NA, match("DOMAIN_ID", variables2)-1)
	demBox<-variableListBox2(top, variableList=variables2, title="Select the STAGE 2 STRATIFICATION (domain) variable:",
		initialSelection=demselection)
	tkblank(top)
	tkgrid(getFrame(demBox), sticky="w")
	demoValue<-tclVar("0")
	demoCB<-tkcheckbutton(top, text="No second stage stratification Variables", command=function()disable.fn(demoValue, demBox$listbox, demBox$label))
	tkconfigure(demoCB, variable=demoValue)
	tkgrid(demoCB, sticky="w")

# Inclusion probability 2 variable
	inclus2selection<-ifelse(is.na(match("ST2_PROB", variables2)), NA, match("ST2_PROB", variables2)-1)
	inclus2Box<-variableListBox2(top, variableList=variables2, title="Select the STAGE 2 INCLUSION PROBABILITY variable:",
		initialSelection=inclus2selection)
	tkblank(top)
	tkgrid(getFrame(inclus2Box), sticky="w")

# Add in Controlled ID numbers
	IDframe<-tkframe(top)
	IDValue<-tclVar("1")
	IDCB<-tkcheckbutton(IDframe, command=function()disable.fn3(IDValue, IDfield, IDlabel))
	IDlabel<-tklabel(IDframe, text="Create ID numbers of length:", fg="blue")
	tkconfigure(IDCB, variable=IDValue)
	idlength<-tclVar("8")
	IDfield<-tkentry(IDframe, width="5", textvariable=idlength)
	tkblank(top)
	tkgrid(IDCB, IDlabel, IDfield, sticky="w")
	tkgrid(IDframe, columnspan=2, sticky="w")

# on ok function
	onOK<-function(){
		df1 <- activeDataSet()
		PSU <- as.double(tkcurselection(PSUBox$listbox))+1
		strata <- as.double(tkcurselection(strataBox$listbox))+1
		dem <- as.double(tkcurselection(demBox$listbox))+1
		demyesno <- tclvalue(IDValue)
		inclus2 <- as.double(tkcurselection(inclus2Box$listbox))+1
		createID <- tclvalue(IDValue) #0 is don't create, 1 is create numbers	 	
		if (createID==1) IDn <- as.numeric(tclvalue(idlength))

	#checks
		if (length(PSU)==0) {check.fn2("PSU")
				return()}
		if (length(strata)==0) {check.fn2("STRATIFICATION")
				return()}
		if (length(inclus2)==0) {check.fn2("SECOND STAGE INCLUSION PROBABILITY")
				return()}
		closeDialog(top)

# Add in report frame 
		initializeDialog(window=run2, title="Run Stage 2?")
		buttonsFrame<-tkframe(run2)
		report<-tclVar("1")
		reportCB<-tkcheckbutton(run2,text="Write report of Sampling Stage 2 details called:", command=function()filedisable())
		tkconfigure(reportCB, variable=report)
		reportname<-tclVar("Stage2_Report")
		fileField<-tkentry(run2, width="20", textvariable=reportname)
	
		tkgrid(tklabel(run2, text="Do you want to proceed with Stage 2 Sampling?"), sticky="w")
		tkgrid(tklabel(run2,text="Click OK to proceed with Stage 2 sampling or cancel to revise options"), sticky="w", columnspan=2)
		tkgrid(tklabel(run2, text=""))
		tkgrid(reportCB, fileField, sticky="w")
		tkgrid(tklabel(run2, text=""))

		filedisable<-function(){
		if (tclvalue(report)==0) tkconfigure(fileField, state="disabled")
		if (tclvalue(report)==1) tkconfigure(fileField, state="normal")
		}

		onOK<-function(){
			reportyesno<-tclvalue(report)
			reportname<-tclvalue(reportname)
			closeDialog(run2)
			if (reportyesno==1) {
				df1names<-names(get(df1))
				cat("Documentation for EHES sampling - Stage 2 Sampling\n\nDate:",date(),"\nR Version:", paste(getRversion()),"\nRcmdr Version:", packageDescription("Rcmdr")$Version,"\nRcmdrPlugin.EHESsampling Version: ", packageDescription("RcmdrPlugin.EHESsampling")$Version, "\nWorking Directory:", getwd(),
				"\n\nDatasets...\nThe main dataset containing Unit per LINE and Sample Sizes was called:", df1,
				"\n\nVariable Details...\nThe Primary Sampling Unit variable was:", df1names[PSU],
				"\nThe Stratification variable was:", df1names[strata], 
				ifelse(demyesno==0, "\nThe Second Stage Stratification variable was:", "No SECOND STAGE STRATIFICATION variable was used"),
				if (demyesno==0) df1names[dem],
				"\nThe Second Stage Inclusion Probability variable was:", df1names[inclus2],
				"\n", file=reportname)
				}
			command<-paste("SampleUnits<-stage2Sample.fn(stage2df=",df1,", PSU=",PSU,", strata=",strata,", dem=", dem, ", demyesno=", demyesno, ", inclus2=", inclus2, ")", sep="")
			doItAndPrint(command)
			if (createID==1) doItAndPrint(paste("SampleUnits$SERIAL<-Create.control(", IDn,", ", nrow(SampleUnits), ")", sep=""))
			activeDataSet("SampleUnits")
			View(SampleUnits)
		}
		OKCancelHelp2(window=run2, onHelp=reportonHelp)
		tkgrid(buttonsFrame, columnspan="2", sticky="w")
		dialogSuffix(rows=2, columns=3, window=run2, focus=buttonsFrame)
	}

	OKCancelHelp2(window=top, onHelp=Stage2onHelp)
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	dialogSuffix(rows=2, columns=3, window=top, focus=buttonsFrame)
}
