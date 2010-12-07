Stage2.notebook<-function() {
#start lines to solve no visible binding for global variables messages...
	top<-run2<-running<-inclus<-mergedf<-selectdf<-NULL
	
	initializeDialog(title="Sampling Stage 2", window=top)
	nb<-tk2notebook(top, tabs=c("Merge Datasets", "Stage 2 Details"))
	tkpack(nb, fill="both", expand=2)
	tkconfigure(nb, width=500)
	tkgrid(nb,columnspan=2)

	next.bn<-tk2button(top, text="Next >>>", width=10, command=function()forward())
	back.bn<-tk2button(top, text="<<< Back", width=10,command=function()backward())
	tkgrid(back.bn, next.bn)
	
	forward<-function(){
		if (tk2notetab.text(nb)=="Merge Datasets") tk2notetab.select(nb,"Stage 2 Details")
		else if (tk2notetab.text(nb)=="Stage 2 Details") sampling2.run()
	}

	backward<-function(){
		if (tk2notetab.text(nb)=="Merge Datasets") tk2notetab.select(nb,"Merge Datasets")	
		else if (tk2notetab.text(nb)=="Stage 2 Details") tk2notetab.select(nb,"Merge Datasets")	
	}


###  Merge Dataset tab   ####
	mergeds<-tk2notetab(nb, "Merge Datasets")
	usemainFrame<-tkframe(mergeds, relief="groove", borderwidth=2)
	usemergedFrame<-tkframe(mergeds, relief="groove", borderwidth=2)

### Radio Buttons?
	rb1<-tkradiobutton(mergeds)
	rb2<-tkradiobutton(mergeds)
	mergeValue<-tclVar(1)
	tkconfigure(rb1, variable=mergeValue, value=1, text="Merge Datasets", fg="red", command=function()boxdisable())
	tkconfigure(rb2, variable=mergeValue, value=2, text="Merge Already Completed", fg="red", command=function()boxdisable())
	
	tkgrid(tklabel(mergeds, text=""))
	tkgrid(rb1)
	
## Get datasets function  ###
	getdataset<-function(title, window, heading, infotext, vars, label.bn, label.text, preview=TRUE){
		initializeDialog(title=title, window=window)
		buttonsFrame<-tkframe(window)
		label<-tklabel(window, text=heading, fg="blue")
		dataframe<-tkframe(window)
		datascr <- tkscrollbar(dataframe, repeatinterval=5,command=function(...)tkyview(dataBox,...))
		dataBox<-tklistbox(dataframe, height=8, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(datascr,...),background="white")
		view.but<-tk2button(window, text="dataset preview", command=function()DataPreview(dataBox))
		infodata<-info.bn(window, infotext)
		tkgrid(label, infodata, sticky="w")
		tkgrid.configure(dataBox, datascr, sticky="nsw")
		if (preview) tkgrid(dataframe, view.but, sticky="w")
		if (!preview) tkgrid(dataframe, sticky="w")
		tkgrid(tklabel(window, text=""))
		for (i in 1:length(vars)) tkinsert(dataBox, "end", vars[i])
		onOK<-function(){
			selection<-variables[as.numeric(tkcurselection(dataBox))+1]
			if (length(selection)>0) {tclvalue(label.bn)<-"Change Dataset..."}
			tclvalue(label.text)<-selection
			getvars()
			closeDialog(window)
			}
		OKCancelHelp(helpSubject="sample", window=window)
		tkgrid(buttonsFrame, columnspan="2", sticky="w")
		dialogSuffix(rows=2, columns=3, window=window, focus=buttonsFrame)
		}


####  Selected PSU datasets   ####
	variables<-listDataSets()
	selectFrame<-tkframe(usemainFrame)
	selection<-ifelse(is.na(match("Stage1Sample", variables)), "NOT FOUND", "Stage1Sample")
	selectlabel<-tklabel(selectFrame, text="The SELECTED PSUs dataset is:", fg="blue")
	selectiontext<-tclVar(selection)
	selectionlabel<-tklabel(selectFrame, text=tclvalue(selectiontext))
	tkconfigure(selectionlabel, textvariable=selectiontext)
	
	buttonlabel<-tclVar("Select Dataset...")
	if (selection!="NOT FOUND"){
		tclvalue(buttonlabel)<-"Change Dataset..."
		}
	databuttonframe<-tkframe(usemainFrame)
	data.bn<-tk2button(databuttonframe, text=tclvalue(buttonlabel), width=15, command=function()getdataset(title="Get Dataset", 
		window=selectdf, heading="Choose the dataset of SELECTED STAGE 1 PSUs:",
		infotext="The Chosen PSUs dataset should contain a list of the PSUs that were selected\n during stage 1 sampling. It should also contain first and second \nstage inclusion probabilities.",
		vars=variables, label.bn=buttonlabel, label.text=selectiontext))
	tkconfigure(data.bn, textvariable=buttonlabel)
	
	tkgrid(selectlabel, selectionlabel)
	tkgrid(data.bn)
	tkgrid(selectFrame, databuttonframe, sticky="w")
	tkgrid(usemainFrame)


###  Mainframe ###
	mainFrame<-tkframe(usemainFrame)
	mainlabel<-tklabel(usemainFrame, text="Select the MAIN dataset(s) [choose one or more]", fg="blue")
	mainscr <- tkscrollbar(mainFrame,command=function(...)tkyview(mainBox,...))
	mainBox<-tklistbox(mainFrame, height=6, selectmode="multiple", export="FALSE", yscrollcommand=function(...)tkset(mainscr,...),background="white")
	tkgrid.configure(mainBox, mainscr, sticky="nsw")
	for (i in 1:length(variables)) tkinsert(mainBox, "end", variables[i])

	framestext<-"The MAIN data set is a data set containing one individual (person, household, \naddress) per line. This datset should contain an individual identifier \nas a reference to contact information. \n\nThe SELECTED PSUs dataset should contain a list of the PSUs that were selected\n during stage 1 sampling. It should also contain first and second \nstage inclusion probabilities."
	infoframes<-info.bn(usemainFrame, framestext)
	tkgrid(mainlabel, infoframes, sticky="w")

	tkgrid(mainFrame, sticky="w")



### dataset name ###
	nameframe<-tkframe(usemainFrame)
	dataname<-tclVar("SelectedMainFrame")
	namelabel<-tklabel(nameframe, text="Enter name for new merged dataset", fg="blue")
	nameField<-tkentry(nameframe, width="20", textvariable=dataname)
	nametext<-"The new dataset will be a merge of the dataset containing the selected \nPSUs and the main frame containing one individual unit per line. The \nmerged dataset will be given the default name: SelectedMainFrame, if this\nis not changed. The new dataset will only be saved in the working environment\nand will also need to be saved to disk."
	infoname<-info.bn(usemainFrame, nametext)
	tkgrid(tklabel(usemainFrame, text="                                                                                         "))
	tkgrid(namelabel, nameField, infoname)	
	tkgrid(nameframe, sticky="w", columnspan=2)


### Merge completed Option ###
	tkgrid(tklabel(mergeds, text=""))
	tkgrid(tklabel(mergeds, text="or"))
	tkgrid(rb2)


### Var label for merged dataset

	selection2<-ifelse(is.na(match("SelectedMainFrame",variables)), "NOT FOUND", "SelectedMainFrame")
	mergedframe<-tkframe(usemergedFrame)
	mergedlabel<-tklabel(mergedframe,text="The MERGED dataset is", fg="blue")
	mergedtext<-tclVar(selection2)
	mergedlabel2<-tklabel(mergedframe, text=tclvalue(mergedtext))
	tkconfigure(mergedlabel2, textvariable=mergedtext)
	buttonlabel2<-tclVar("Select Dataset...")
	if (selection2!="NOT FOUND"){
		tclvalue(buttonlabel2)<-"Change Dataset..."
		}
	mergebuttonframe<-tkframe(usemergedFrame)
	merge.bn<-tk2button(mergebuttonframe, text=tclvalue(buttonlabel2), width=15, command=function()getdataset(title="Get Dataset", 
		window=mergedf, heading="Select the MERGED dataset:",
		infotext="If the dataset containing the selected PSUs has already been merged with \nthe main dataset containing individual units per line, this is called\nthe merged dataset.",
		vars=variables, label.bn=buttonlabel2, label.text=mergedtext))
	tkconfigure(merge.bn, textvariable=buttonlabel2)

	tkgrid(mergedlabel, mergedlabel2)
	tkgrid(merge.bn)
	tkgrid(mergedframe, mergebuttonframe, sticky="w")
	tkgrid(tklabel(usemergedFrame, text="                                                                                               "))
	tkgrid(usemergedFrame, sticky="w")
	tkgrid(tklabel(mergeds, text=""))


### initial disable and disable function
	boxdisable<-function(){
		a<-list(namelabel, nameField, selectlabel, selectionlabel, data.bn, mainlabel, mainBox)
		b<-list(mergedlabel, mergedlabel2, merge.bn)
		if (tclvalue(mergeValue)=="2") { setstate(a, "disabled")
			setstate(b, "normal")
			}
		if (tclvalue(mergeValue)=="1") {setstate(a, "normal")
			setstate(b, "disabled")
			}
		getvars()
		}


### details tab ###
	details<-tk2notetab(nb, "Stage 2 Details")
	tkgrid(tklabel(details, text=""))

### Get variables functions  ###
getallvar<-function(){
	if (tclvalue(mergeValue)==1){
		if (tclvalue(selectiontext)=="NOT FOUND") {allvar<-"NA"}
		if (length(as.numeric(tkcurselection(mainBox)))==0) {allvar<-"NA"}
		if (tclvalue(selectiontext)!="NOT FOUND" & length(as.numeric(tkcurselection(mainBox)))>0) {
			var1<-names(get(tclvalue(selectiontext)))
			datasets.main<-variables[as.numeric(tkcurselection(mainBox))+1]
			var2<-names(get(datasets.main[1]))
			if (length(datasets.main)>1) {
				for (i in 2:length(datasets.main)) {
					matched<-names(get(datasets.main[i]))[match(var2,names(get(datasets.main[i])))]
					var2<-subset(matched, !is.na(matched))
					}
				}
			allvar<-unique(c(var1,var2))
			}
		}
	if (tclvalue(mergeValue)==2){
		if (tclvalue(mergedtext)=="NOT FOUND") {allvar<-"NA"} else {
			allvar<-names(get(tclvalue(mergedtext)))
			}
		}
	return(allvar)
	}

getmatchedvar<-function(){
	if (tclvalue(mergeValue)==1){
		if (tclvalue(selectiontext)=="NOT FOUND") {matchedvar<-"NA"}
		if (length(as.numeric(tkcurselection(mainBox)))==0) {matchedvar<-"NA"}
		if (tclvalue(selectiontext)!="NOT FOUND" & length(as.numeric(tkcurselection(mainBox)))>0) {
			var1<-names(get(tclvalue(selectiontext)))
			datasets.main<-variables[as.numeric(tkcurselection(mainBox))+1]
			var2<-names(get(datasets.main[1]))
			if (length(datasets.main)>1) {
				for (i in 2:length(datasets.main)) {
					matched<-names(get(datasets.main[i]))[match(var2,names(get(datasets.main[i])))]
					var2<-subset(matched, !is.na(matched))
					}
				}
			temp<-var2[match(var1,var2)]
			matchedvar<-subset(temp, !is.na(temp))
			}
		}	
	if (tclvalue(mergeValue)==2){
		if (tclvalue(mergedtext)=="NOT FOUND") {matchedvar<-"NA"} else {
			matchedvar<-names(get(tclvalue(mergedtext)))
			}
		}
	return(matchedvar)
	}


getvars<-function(){
	matchedvar<-getmatchedvar()
	allvar<-getallvar()
		
	tkdelete(PSUBox,0,"end")
	for (i in 1:length(matchedvar)) tkinsert(PSUBox, "end", matchedvar[i])

	tkdelete(strataBox,0,1000)
	for (i in 1:length(allvar)) tkinsert(strataBox, "end", allvar[i])

	demselection<-ifelse(is.na(match("domain", getallvar())), "No Stage 2 Stratification", "domain")
	if (demselection=="domain") {
		tclvalue(dembuttonlabel)<-"Change Variable..."
		tclvalue(demlabel2text)<-demselection
		tclvalue(demlabeltext)<-"The SECOND STAGE STRATIFICATION (domain) variable is:"
		} else {
		tclvalue(dembuttonlabel)<-"Add Variable..."
		tclvalue(demlabeltext)<-demselection
		tclvalue(demlabel2text)<-" "
		}

	inclus1selection<-ifelse(is.na(match("stage1.prob", getallvar())), "NOT FOUND", "stage1.prob")
	tclvalue(inclus1buttonlabel)<-ifelse(inclus1selection=="NOT FOUND", "Select Variable...", "Change Variable...")
	tclvalue(inclus1labeltext)<-inclus1selection
	
	inclus2selection<-ifelse(is.na(match("stage2.prob", getallvar())), "NOT FOUND", "stage2.prob")
	tclvalue(inclus2buttonlabel)<-ifelse(inclus2selection=="NOT FOUND", "Select Variable...", "Change Variable...")
	tclvalue(inclus2labeltext)<-inclus2selection
	}


####  PSU List Boxes  ####
	PSUframe<-tkframe(details)
	PSUlabel<-tklabel(details, text="Select the PSU variable", fg="blue")
	PSUscr <- tkscrollbar(PSUframe, repeatinterval=5,command=function(...)tkyview(PSUBox,...))
	PSUBox<-tklistbox(PSUframe, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(PSUscr,...),background="white")
	PSUtext<-"The PSU variable should contain a unique identifying number or name \nfor each Primary Sampling Unit."
	infoPSU<-info.bn(details, PSUtext)
	tkgrid(PSUlabel, infoPSU, sticky="w")
	tkgrid.configure(PSUBox, PSUscr, sticky="ew")
	tkgrid(PSUframe, sticky="w")
	tkgrid(tklabel(details, text=""))


####   strata Box   ####
	strataframe<-tkframe(details)
	stratalabel<-tklabel(details, text="Select the STRATA variable", fg="blue")
	stratascr <- tkscrollbar(strataframe, repeatinterval=5,command=function(...)tkyview(strataBox,...))
	strataBox<-tklistbox(strataframe, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(stratascr,...),background="white")
	strataValue<-tclVar("0")
	strataCB<-tkcheckbutton(details,text="This is a one stage sample",command=function()disable.fn(strataValue,strataBox,stratalabel))
	tkconfigure(strataCB, variable=strataValue)
	Stratatext<-"The STRATA variable should contain a unique identifying number or name \nfor each STRATUM. If you are sampling every PSU, this is called a \none stage sample and the dataset does not need a strata variable.\nIn the case of a one stage sample, select the one stage sample \ncheck box."
	infoStrata<-info.bn(details, Stratatext)
	
	tkgrid(stratalabel, infoStrata, sticky="w")
	tkgrid.configure(strataBox, stratascr, sticky="ew")
	tkgrid(strataframe, sticky="w")
	tkgrid(strataCB, sticky="w")
	tkgrid(tklabel(details, text=""))


####   Get demo box function   ###

	getdemo<-function(){
		initializeDialog(title="Domain Variable", window=demo)
		buttonsFrame<-tkframe(demo)
		demoBox<-variableListBox(demo, variableList=c("No Stage 2 Stratification", getallvar()), title="Select the SECOND STAGE STRATIFICATION (domain) variable:")
		demotext<-"The Stage 2 stratification variable should specify which domain each of the \n individual units belongs to."
		infodemo<-info.bn(demo, demotext)
		tkgrid(getFrame(demoBox),infodemo, sticky="w")
		tkgrid(tklabel(demo, text=""))
		onOK<-function(){
			selection<-getSelection(demoBox)
			if (selection=="No Stage 2 Stratification") {
				tclvalue(dembuttonlabel)<-"Add Variable..."
				tclvalue(demlabeltext)<-selection
				tclvalue(demlabel2text)<-" "
				} else {
				tclvalue(dembuttonlabel)<-"Change Variable..."
				tclvalue(demlabel2text)<-selection
				tclvalue(demlabeltext)<-"The SECOND STAGE STRATIFICATION (domain) variable is:"
				}
			closeDialog(demo)
			}
		OKCancelHelp(helpSubject="sample", window=demo)
		tkgrid(buttonsFrame, columnspan="2", sticky="w")
		dialogSuffix(rows=2, columns=3, window=demo, focus=buttonsFrame)
	}


####   demo label   ####

	demframe<-tkframe(details)
	dembuttonframe<-tkframe(details)
	dembuttonlabel<-tclVar("Select Variable...")
	dem.bn<-tk2button(dembuttonframe, text=tclvalue(dembuttonlabel), width=15, command=function()getdemo())
	tkconfigure(dem.bn, textvariable=dembuttonlabel)
	demlabeltext<-tclVar("The SECOND STAGE STRATIFICATION (domain) variable is:")
	demlabel<-tklabel(demframe, text=tclvalue(demlabeltext), fg="blue")
	tkconfigure(demlabel, textvariable=demlabeltext)
	demlabel2text<-tclVar("NOT FOUND")
	demlabel2<-tklabel(demframe, text=tclvalue(demlabel2text))
	tkconfigure(demlabel2, textvariable=demlabel2text)

	tkgrid(demlabel, demlabel2)
	tkgrid(dem.bn)
	tkgrid(demframe, dembuttonframe, sticky="w")
	tkgrid(tklabel(details, text=""))


### Get inclus
	getinclus<-function(title, inclustext, inclusbuttonlabel, incluslabeltext){
		initializeDialog(title="Inclusion Probability", window=inclus)
		buttonsFrame<-tkframe(inclus)
		inclusBox<-variableListBox(inclus, variableList=getallvar(), title=title)
		infoinclus<-info.bn(inclus, inclustext)
		tkgrid(getFrame(inclusBox), infoinclus, sticky="w")
		tkgrid(tklabel(inclus, text=""))
		onOK<-function(){
			selection<-getSelection(inclusBox)
			if (length(selection)>0) {
				tclvalue(inclusbuttonlabel)<-"Change Variable..."
				tclvalue(incluslabeltext)<-selection
				}
			closeDialog(inclus)
			}
		OKCancelHelp(helpSubject="sample", window=inclus)
		tkgrid(buttonsFrame, columnspan="2", sticky="w")
		dialogSuffix(rows=2, columns=3, window=inclus, focus=buttonsFrame)
	}


####  inclus1 prob Box ####

	inclus1frame<-tkframe(details)
	inclus1butframe<-tkframe(details)
	inclus1buttonlabel<-tclVar("Select Variable...")
	inclus1label<-tklabel(inclus1frame, text="The STAGE 1 INCLUSION PROBABILITY variable is:", fg="blue")
	inclus1labeltext<-tclVar("NOT FOUND")
	inclus1label2<-tklabel(inclus1frame, text=tclvalue(inclus1labeltext))
	tkconfigure(inclus1label2, textvariable=inclus1labeltext)
	inclus1.bn<-tk2button(inclus1butframe, text=tclvalue(inclus1buttonlabel), width=15, command=function()getinclus(title="Select the STAGE 1 INCLUSION PROBABILITY variable:",
		inclustext="The first stage inclusion probability is how likely each PSU is to being \nselected for sampling in the first stage. This can be calculated with \nthe sample sizes option in the RcmdrPluing.EHES package.",
		inclusbuttonlabel=inclus1buttonlabel, incluslabeltext=inclus1labeltext
	))
	tkconfigure(inclus1.bn, textvariable=inclus1buttonlabel)
	
	tkgrid(inclus1label, inclus1label2)
	tkgrid(inclus1.bn)
	tkgrid(inclus1frame, inclus1butframe, sticky="w")
	tkgrid(tklabel(details, text="                                                                                                               "))


####   inclus2 prob Box  ####
	inclus2frame<-tkframe(details)
	inclus2butframe<-tkframe(details)
	inclus2buttonlabel<-tclVar("Select Variable...")
	inclus2label<-tklabel(inclus2frame, text="The STAGE 2 INCLUSION PROBABILITY variable is:", fg="blue")
	inclus2labeltext<-tclVar("NOT FOUND")
	inclus2label2<-tklabel(inclus2frame, text=tclvalue(inclus2labeltext))
	tkconfigure(inclus2label2, textvariable=inclus2labeltext)
	inclus2.bn<-tk2button(inclus2butframe, text=tclvalue(inclus2buttonlabel), width=15,  command=function()getinclus(title="Select the STAGE 2 INCLUSION PROBABILITY variable:",
		inclustext="The second stage inclusion probability is how likely an individual (person,\n household, address) is to being \nselected for sampling, given that the \nPSU they are in was selected in stage 1 sampling. This can be calculated with \nthe sample sizes option in the RcmdrPluing.EHES package.",
		inclusbuttonlabel=inclus2buttonlabel, incluslabeltext=inclus2labeltext
		))
	tkconfigure(inclus2.bn, textvariable=inclus2buttonlabel)
	
	tkgrid(inclus2label, inclus2label2)
	tkgrid(inclus2.bn)
	tkgrid(inclus2frame, inclus2butframe, sticky="w")
	tkgrid(tklabel(details, text=""))


	tkbind(mainBox, "<ButtonRelease-1>", function()getvars())

### If initial selection is automatic then put in variables
	if (selection2=="NOT FOUND") {boxdisable()} else {
		tclvalue(mergeValue)<-2
		boxdisable()
		}
	

###Add in Controlled ID numbers
	idframe<-tkframe(details)
	IDValue<-tclVar("1")
	IDCB<-tkcheckbutton(idframe, command=function()IDdisable.fn())
	IDlabel<-tklabel(idframe, text="Create ID numbers of length:", fg="blue")
	tkconfigure(IDCB, variable=IDValue)
	idlength<-tclVar("8")
	IDfield<-tkentry(idframe, width="5", textvariable=idlength)
	idtext<-"Choose this option to create an ID number for all the selected participants in the survey. The ID number is encoded with a control digit using the dihedral group of order 10 addition and can be checked later using the CHECK ENCODED NUMBERS option. "
	infoID<-info.bn(idframe, idtext)	
	tkgrid(IDCB, IDlabel, IDfield, infoID, sticky="w")
	tkgrid(idframe, sticky="w")

IDdisable.fn<-function(){
	if (tclvalue(IDValue)==1) {
		tkconfigure(IDfield, state="normal")
		tkconfigure(IDlabel, state="normal")}
	if (tclvalue(IDValue)==0){
		tkconfigure(IDfield, state="disabled")
		tkconfigure(IDlabel, state="disabled")}
	}


### On sampling ###
sampling2.run<-function(){
	initializeDialog(window=run2, title="Run Stage 2?")
	buttonsFrame<-tkframe(run2)

	report<-tclVar("1")
	reportCB<-tkcheckbutton(run2,text="Write report of Sampling Stage One details called:", command=function()filedisable())
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

### On OK
onOK<-function(){
		reportyesno<-tclvalue(report)
		reportname<-tclvalue(reportname)
		closeDialog(run2)

### get information in
	#merge datasets
		allvariables<-getallvar()
		matchedvar<-getmatchedvar()
		mergeyesno<-tclvalue(mergeValue)
		PSUselect<-tclvalue(selectiontext)
		maindata<-variables[as.numeric(tkcurselection(mainBox))+1]
		dataname<-tclvalue(dataname)
		stage2df<-tclvalue(mergedtext)
		
	#stage 2 details
		PSU <- as.numeric(tkcurselection(PSUBox))+1
		strata <- as.numeric(tkcurselection(strataBox))+1
		dem<-tclvalue(demlabel2text) #domain or " " if none
		inclus1<-tclvalue(inclus1labeltext)
		inclus2<-tclvalue(inclus2labeltext)
		onestage<-tclvalue(strataValue) #0 is two stage, 1 is one stage
		createID<-tclvalue(IDValue) #0 is don't create, 1 is create numbers	 	
		if (createID==1) IDn<-as.numeric(tclvalue(idlength))

	#Put in checks
		check.fn<-function(v, nbtab){
			mes<-paste("Please select a ", v, " variable", sep="")
 			tkmessageBox(message=mes)
			tk2notetab.select(nb, nbtab)		
			}

		if (mergeyesno==1){
			if (PSUselect=="NOT FOUND") {check.fn("PSU dataset", "Merge Datasets")
				return()}
			if (length(maindata)==0) {check.fn("MAIN dataset(s)", "Merge Datasets")
				return()}
			}
		if (mergeyesno==2 & stage2df=="NOT FOUND") {check.fn("MERGED dataset", "Merge Datasets")
				return()}
		if (length(PSU)==0) {check.fn("PSU", "Stage 2 Details")
				return()}
		if (length(strata)==0 & onestage==0) {check.fn("STRATA", "Stage 2 Details")
				return()}
		if (dem=="NOT FOUND") {check.fn("STAGE 2 STRATIFICATION (domain)", "Stage 2 Details")
				return()}
		if (inclus1=="NOT FOUND") {check.fn("FIRST STAGE INCLUSION PROBABILITY", "Stage 2 Details")
				return()}
		if (inclus2=="NOT FOUND") {check.fn("SECOND STAGE INCLUSION PROBABILITY", "Stage 2 Details")
				return()}
		if (onestage=="1"){
			tkmessageBox(message="You have chosen a one-stage sampling design. This option has not been prepared in Rcmdr.")
			tk2notetab.select(nb,"Stage 2 Details")
			return()
			}

# create running window #

		initializeDialog(window=running)
		tkconfigure(running, cursor="watch")
		labeltext<- tclVar("Merging datasets... Please Wait")
		runlabel<-tklabel(running, text=tclvalue(labeltext))
		tkconfigure(runlabel, textvariable=labeltext)
		tkgrid(runlabel)

# merge datasets with more than one main frame
		if (mergeyesno==1) {
			PSU <- matchedvar[PSU]
			if (length(maindata)>1){
				main.dataset<-NULL
				for (i in 1:length(maindata)) {
					main.dataset<-rbind(main.dataset, get(maindata[i]))
					}	
				main.dataset<<-main.dataset
				command<-paste(dataname, "<-merge(",PSUselect,", main.dataset)", sep="")
				PSUy<-main.dataset[,match(PSU, names(main.dataset))]

# merge datasets with one main frame
			} else {
				command<-paste(dataname, "<-merge(",PSUselect,",", maindata,")", sep="")
				PSUy<-get(maindata)[,match(PSU, names(get(maindata)))]
				}

# Check for PSU mismatches
			PSUx<-get(PSUselect)[,match(PSU, names(get(PSUselect)))]
			bad1<-is.element(PSUx, PSUy)
			if (is.element("FALSE", bad1)) {
				closeDialog(running)
				tkmessageBox(message="Not all of the PSUs in the selected PSU dataset were located in the main dataset (s). Please check them!")
				tk2notetab.select(nb, "Strata Details")
				return()
				}
		
# Merge datasets
			closeDialog(top)
			doItAndPrint(command)
			PSUnew<-match(PSU, names(get(dataname)))
			stage2df<-dataname
		} else {
			closeDialog(top)
			PSUnew<-PSU
		}

#select new variable numbers in new merged dataset
		stratanew<-match(allvariables[strata],names(get(stage2df)))
		demnew<-ifelse(dem==" ", "na", match(dem, names(get(stage2df))))
		inclus1new<-match(inclus1,names(get(stage2df)))
		inclus2new<-match(inclus2,names(get(stage2df)))

#Add in Report		
		if (reportyesno==1) {
			df1names<-names(get(stage2df))
			cat("Documentation for EHES sampling - Stage 2 Sampling\n\nDate:",date(),"\nR Version:", paste(getRversion()),"\nRcmdr Version:", packageDescription("Rcmdr")$Version,"\nRcmdrPlugin.EHESsampling Version: ", packageDescription("RcmdrPlugin.EHESsampling")$Version, "\nWorking Directory:", getwd(),
			"\n\nDatasets...\nThe main dataset containing Unit per LINE and Sample Sizes was called:", stage2df,
			"\n\nVariable Details...\nThe Primary Sampling Unit variable was:", df1names[PSUnew],
			"\nThe Stratification variable was:", df1names[stratanew], 
			ifelse(tclvalue(demlabel2text)!=" ", "\nThe Second Stage Stratification variable was:", "No SECOND STAGE STRATIFICATION variable was used"),
			if (tclvalue(demlabel2text)!=" ") df1names[demnew],
			"\nThe First Stage Inclusion Probability variable was:", df1names[inclus1new],
			"\nThe Second Stage Inclusion Probability variable was:", df1names[inclus2new],
			"\n", file=reportname)
			}

#sampling
		tclvalue(labeltext)<-"Sampling... Please Wait"
		doItAndPrint(paste("SampleUnits<-stage2Sample.fn(stage2df=",stage2df,", PSU=",PSUnew,", strata=",stratanew,", dem=", demnew, ", inclus1=", inclus1new, ", inclus2=", inclus2new, ")", sep=""))
		if (createID==1) SampleUnits$ID_Numbers<-Create.control(IDn, nrow(SampleUnits))
		closeDialog(running)
		View(SampleUnits)
		}

	OKCancelHelp(window=run2,helpSubject="sample")
	tkgrid(buttonsFrame, columnspan="2", sticky="w")	
	dialogSuffix(window=run2, rows=3, columns=2, focus=buttonsFrame)
	}
	onOK<-function(){}
	OKCancelHelp(helpSubject="sample")
	dialogSuffix(focus=next.bn)
}
