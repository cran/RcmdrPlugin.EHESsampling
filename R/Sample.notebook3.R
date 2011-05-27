

Sample.datasets<-function(){
	initializeDialog(title="Data sets")
	variables<-listDataSets()

# PSU Box
	PSUBox<-variableListBox2(top, variableList=variables, title="Select the PRIMARY SAMPLING UNIT data set:")

# StrataBox with check button
	strataBox<-variableListBox2(top, variableList=variables, title="Select the Stratification data set:")
	stratayesno<-tclVar("0")
	strataCB<-tkcheckbutton(top, text="No stratification dataset",command=function()disable.fn2(stratayesno, strataBox))
	tkconfigure(strataCB, variable=stratayesno)

#forward function
	onOK<-function(){
		df1<-getSelection(PSUBox)
		stratayesno <-tclvalue(stratayesno) #0 is yes stratadataset, 1 is create dataset
		if (stratayesno==0) {df2<-getSelection(strataBox)} else {df2<-getSelection(PSUBox)}

		if (length(df1)==0) {check.fn2("PSU DATASET") 
			return()}

		if (stratayesno==0 & length(df2)==0) {check.fn2("STRATIFICATION DATASET") 
			return()}

		tkdestroy(top)
		if (Sys.info()[["sysname"]] == "Linux" |Sys.info()[["sysname"]] == "Unix") {
			PSU.options(df1=df1, df2=df2, stratayesno=stratayesno)} else {
			ALL.options(df1=df1, df2=df2, stratayesno=stratayesno)}
	}

#Grid
	tkgrid(getFrame(PSUBox), sticky="w")
	tkblank(top)
	tkgrid(getFrame(strataBox), sticky="w")
	tkgrid(strataCB, sticky="w")
	tkblank(top)

#help and buttons
	OKCancelHelp2(window=top, onHelp=dataonHelp)
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	dialogSuffix(rows=3, window=top, focus=buttonsFrame)
}



# Notebook for calculating sample sizes (Windows version)

ALL.options<-function(df1, df2, stratayesno){
	one<-samplecheck<-StrataSampleSize<-PSUSampleSize<-top<-NULL

	initializeDialog(title="Sample Sizes")
	nb<-tk2notebook(top, tabs=c("PSU Details", "Strata Details", "Cost/Variance Details"))
	tkpack(nb, fill="both", expand=2)
	tkconfigure(nb, width=700)
	tkgrid(nb,columnsp=2)

	next.bn<-tk2button(top, text="Next >>>", width=10, command=function()forward())
	back.bn<-tk2button(top, text="<<< Back", width=10,command=function()backward())
	tkgrid(back.bn, next.bn)
	
forward<-function(){
	if (tk2notetab.text(nb)=="PSU Details") tk2notetab.select(nb,"Strata Details")
	else if (tk2notetab.text(nb)=="Strata Details") tk2notetab.select(nb,"Cost/Variance Details")
	else if (tk2notetab.text(nb)=="Cost/Variance Details") review.fn()
	}

backward<-function(){
	if (tk2notetab.text(nb)=="PSU Details") tk2notetab.select(nb,"PSU Details")
	else if (tk2notetab.text(nb)=="Strata Details") tk2notetab.select(nb,"PSU Details")
	else if (tk2notetab.text(nb)=="Cost/Variance Details") tk2notetab.select(nb,"Strata Details")
	}


### PSU DETAILS tab ###
	variables<-names(get(df1))
	variables2<-names(get(df2))
	PSU.b<-tk2notetab(nb, "PSU Details")
	tkgrid(tklabel(PSU.b,text=""))
	

#PSU List Box
	
	PSUframe.b<-tkframe(PSU.b)
	PSUlabel.b<-tklabel(PSU.b, text="The PSU variable is:", fg="blue")
	PSUscr.b <- tkscrollbar(PSUframe.b, repeatinterval=5,command=function(...)tkyview(PSUBox.b,...))
	PSUBox.b<-tklistbox(PSUframe.b, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(PSUscr.b,...),
		background="white")
	tkgrid(PSUlabel.b, sticky="w")
	tkgrid.configure(PSUBox.b,PSUscr.b)

	PSUtext<-"The PSU variable should contain a unique identifying number or name \nfor each Primary Sampling Unit. The identifyer should be identical in both the \nPSU dataset and STRATA datset (if using a separate dataset)."
	infoPSU<-info.bn(PSU.b, PSUtext)	
	tkgrid(PSUframe.b, infoPSU, sticky="w")

#Size list box
	sizeframe.b<-tkframe(PSU.b)
	sizescr.b <- tkscrollbar(sizeframe.b, repeatinterval=5,command=function(...)tkyview(sizeBox.b,...))
	sizelabel<-tklabel(PSU.b,text="The SIZE variable is:", fg="blue")
	sizeBox.b<-tklistbox(sizeframe.b, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(sizescr.b,...),background="white")
	tkgrid.configure(sizeBox.b, sizescr.b)
	
	Sizetext<-"The SIZE variable should list the size of each PSU. This may be in terms of the \npopulation size or the number of addresses/dwellings for each Primary Sampling Unit.\n"
	infoSize<-info.bn(PSU.b, Sizetext)
	tkgrid(tklabel(PSU.b, text=""))
	tkgrid(sizelabel, sticky="w")
	tkgrid(sizeframe.b, infoSize, sticky="w")

#Strata List Box
	strataframe.b<-tkframe(PSU.b)
	stratascr.b<- tkscrollbar(strataframe.b, repeatinterval=5,command=function(...)tkyview(strataBox.b,...))
	strataBox.b<-tklistbox(strataframe.b, height=4, selectmode="single", export="FALSE",yscrollcommand=function(...)tkset(stratascr.b,...),background="white")
	stratalabel.b<-tklabel(PSU.b,text="The STRATA variable is:", fg="blue")
	tkgrid.configure(strataBox.b,stratascr.b)
	Stratatext<-"The STRATA variable should contain a unique identifying number or name \nfor each STRATUM. The identifyer should be identical in both the \nPSU dataset and STRATA datset (if using a separate dataset). If you are sampling every PSU,
		this is called a one stage sample and the dataset does not need a strata variable.\n In the case of a one stage sample, select the one stage sample check box."
	infoStrata<-info.bn(PSU.b, Stratatext)

	tkgrid(tklabel(PSU.b, text=""))
	tkgrid(stratalabel.b, sticky="w")
	tkgrid(strataframe.b, infoStrata, sticky="w")	
	tkgrid(tklabel(PSU.b, text=""))

#Demographics
	demoValue<-tclVar("0")
	demoCB<-tkcheckbutton(PSU.b, text="No second stage stratification Variables",command=function(){disable.fn(demoValue,demoBox,demolabel)})
	tkconfigure(demoCB, variable=demoValue)
	demolabel<-tklabel(PSU.b,text="The stage 2 stratification variables are: [select all] ", fg="blue")
	demoframe<-tkframe(PSU.b)
	demoscr <- tkscrollbar(demoframe, repeatinterval=5,command=function(...)tkyview(demoBox,...))
	demoBox<-tklistbox(demoframe, height=8, selectmode="multiple", export="FALSE", yscrollcommand=function(...)tkset(demoscr,...),background="white")
	tkgrid.configure(demoBox,demoscr, sticky="nsw")
	Demotext<-"The Stage 2 stratification variables should list the size of each domain in each of the \nPrimary Sampling Units. For example, information on sex distributions may be available for \neach PSUs and the dataset would contain 2 columns, one for the number of females in each\n PSU and one for the number of males in each PSU."
	infoDemo<-info.bn(PSU.b, Demotext)

	tkgrid(demolabel, sticky="w", columnspan=2)
	tkgrid(demoframe, infoDemo, sticky="w")
	tkgrid(demoCB, sticky="w")
	tkgrid(tklabel(PSU.b, text=""))


###### Strata Details Tab ########
	Strata.d<-tk2notetab(nb, "Strata Details")

#Strata List Box
	strataframe.d<-tkframe(Strata.d)
	stratascr.d<- tkscrollbar(strataframe.d, repeatinterval=5,command=function(...)tkyview(strataBox.d,...))
	strataBox.d<-tklistbox(strataframe.d, height=4, selectmode="single", export="FALSE",yscrollcommand=function(...)tkset(stratascr.d,...),background="white")
	stratalabel.d<-tklabel(Strata.d,text="The STRATA variable is:", fg="blue")
	tkgrid.configure(strataBox.d,stratascr.d)
	Stratatext<-"The STRATA variable should contain a unique identifying number or name \nfor each STRATUM. The identifier should be identical in both the PSU \ndataset and STRATA datset (if using a separate dataset). If you are sampling \nevery PSU, this is called a one stage sample and the dataset does not need \na strata variable. In the case of a one stage sample, select the one stage \nsample check box."
	infoStrata.d<-info.bn(Strata.d, Stratatext)
	tkgrid(tklabel(Strata.d, text=""))
	tkgrid(stratalabel.d, infoStrata.d, sticky="w")
	tkgrid(strataframe.d, sticky="w")	
	tkgrid(tklabel(Strata.d, text=""))


# m variable box
	mkvarframe<-tkframe(Strata.d,relief="groove", borderwidth=1)
	mkscr <- tkscrollbar(mkvarframe, repeatinterval=5,command=function(...)tkyview(mkBox,...))
	mkvarlabel<-tklabel(mkvarframe,text="The stage 1 sample size variable is:", fg="blue")
	mkBox<-tklistbox(mkvarframe, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(mkscr,...),background="white")
	tkgrid(mkvarlabel, sticky="w")
	tkgrid.configure(mkBox, mkscr, sticky="we")

# m question buttons
	mkframe<-tkframe(Strata.d)
	mkqlabel<-tklabel(mkframe, text="Does the dataset contain a STAGE 1 SAMPLE SIZE variable? (m)", fg="blue")
	rb1<-tkradiobutton(mkframe)
	rb2<-tkradiobutton(mkframe)
	mkValue<-tclVar(1)
	tkconfigure(rb1, variable=mkValue, value=0, text="yes", command=function()mk.variable())
	tkconfigure(rb2, variable=mkValue, value=1, text="no - I want to sample an optimal number of PSUs", command=function()mk.variable())
	tkgrid(mkqlabel)
	tkgrid(rb1, sticky="nw")
	tkgrid(rb2, sticky="nw")
	mktext<-"The STAGE 1 SAMPLE SIZE variable is a list of the number of PRIMARY SAMPLING UNITS to be sampled in each stratum.\n"
	infomk<-info.bn(Strata.d, mktext)	
	tkgrid(mkframe, mkvarframe, infomk, sticky="nw", columnspan=3)
	tkgrid(tklabel(Strata.d, text=""))

#strata size var window
	ssvarframe<-tkframe(Strata.d, relief="groove", borderwidth=1)
	ssscr <- tkscrollbar(ssvarframe, repeatinterval=5,command=function(...)tkyview(ssBox,...))
	ssvarlabel<-tklabel(ssvarframe,text="The STRATA SAMPLE SIZES variable is:", fg="blue")
	ssBox<-tklistbox(ssvarframe, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(ssscr,...),background="white")
	tkgrid(ssvarlabel, sticky="w")
	tkgrid.configure(ssBox,ssscr, sticky="we")
	sstext<-"The STRATA SAMPLE SIZES variable is a list of the number of INDIVIDUAL Units (SSUs) to be sampled in each stratum.\n"
	infoss<-info.bn(Strata.d, sstext)

# First radio buttons
	ssframe<-tkframe(Strata.d)
	sslabel<-tklabel(ssframe, text="Does the dataset contain a STRATA SAMPLE SIZES variable?", fg="blue")
	rbss1<-tkradiobutton(ssframe)
	rbss2<-tkradiobutton(ssframe)
	ssValue<-tclVar(1)
	tkconfigure(rbss1, variable=ssValue, value=0, text="yes", command=function()ssdisable())
	tkconfigure(rbss2, variable=ssValue, value=1, text="no", command=function()ssdisable())
	tkgrid(sslabel, sticky="w")
	tkgrid(rbss1, sticky="nw")
	tkgrid(rbss2, sticky="nw")
	tkgrid(ssframe, ssvarframe, infoss, sticky="nw", columnspan=3)

# Strat Size field variable window
	ssboxframe<-tkframe(Strata.d)
	sscalcframe<-tkframe(ssboxframe)
	sstotal<-tclVar(4000)
	sstotallabel<-tklabel(sscalcframe, text="The total sample size is approximately:", fg="blue")
	ssField<-tkentry(sscalcframe, width="10", textvariable=sstotal)
	totaltext<-"The total sample size is the desired number of individuals/addresses to select. This \nis only approximate as if a minimum stratum sample size is choosen, the total sample \nsize will be inflated. Probability proportional to size is recommended however a minimum \nsize may be desirable if estimates are required for each stratum" 
	infototal<-info.bn(ssboxframe, totaltext)	
	tkgrid(sstotallabel, ssField, infototal, columnspan=2)

#ss Sample distribution among strata
	ppslabel<-tklabel(ssboxframe, text="How do you want the sample distributed among the STRATA?", fg="blue")
	ssradioframe<-tkframe(ssboxframe)	
	rbpps1<-tkradiobutton(ssradioframe)
	rbpps2<-tkradiobutton(ssradioframe)
	rbpps3<-tkradiobutton(ssradioframe)
	rbpps4<-tkradiobutton(ssradioframe)
	ppsValue<-tclVar(1)
	tkconfigure(rbpps1, variable=ppsValue, value=1, text="", command=function()psdisable())
	tkconfigure(rbpps2, variable=ppsValue, value=2, text="", command=function()psdisable())
	tkconfigure(rbpps3, variable=ppsValue, value=3, text="", command=function()psdisable())
	tkconfigure(rbpps4, variable=ppsValue, value=4, text="", command=function()psdisable())
	tkgrid(rbpps1,sticky="nw")
	tkgrid(rbpps2, sticky="nw")	
	tkgrid(rbpps3, sticky="nw")	
	tkgrid(rbpps4, sticky="nw")	

	ppsframe<-tkframe(ssboxframe)
	pps1label<-tklabel(ppsframe,text="Proportional to Size (recommended)")
	pps2label<-tklabel(ppsframe,text="Proportional to Size with a minimum in each stratum of")
	pps2<-tclVar(1000)
	pps2Field<-tkentry(ppsframe, width="5", textvariable=pps2)
	pps3label<-tklabel(ppsframe,text="Fixed Sample Size per stratum")
	pps4label<-tklabel(ppsframe,text="Neymans Allocation")
	tkgrid(pps1label, sticky="w")
	tkgrid(pps2label, pps2Field, sticky="w")	
	tkgrid(pps3label, sticky="w")
	tkgrid(pps4label, sticky="w")

# Grid the strata ss options
	tkgrid(sscalcframe, columnspan=2, sticky="w")
	tkgrid(tklabel(ssboxframe, text=""))
	tkgrid(ppslabel, sticky="w", columnspan=3)
	tkgrid(ssradioframe, ppsframe, sticky="w")
	tkgrid(ssboxframe, sticky="w",columnspan=3)


### Cost/Variance Tab tab ###
	Opt<-tk2notetab(nb, "Cost/Variance Details")
	tkgrid(tklabel(Opt, text=""))

#Cost variables
	PSUcostframe<-tkframe(Opt)
	costscr <- tkscrollbar(PSUcostframe, repeatinterval=5,command=function(...)tkyview(PSUcostBox,...))
	PSUcostlabel<-tklabel(PSUcostframe,text="The PSU COST variable is:", fg="blue")
	PSUcostBox<-tklistbox(PSUcostframe, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(costscr,...),background="white")
	Parcostframe<-tkframe(Opt)
	Parscr <- tkscrollbar(Parcostframe, repeatinterval=5,command=function(...)tkyview(ParcostBox,...))
	Parcostlabel<-tklabel(Parcostframe,text="The PARTICIPANT COST variable is:", fg="blue")
	ParcostBox<-tklistbox(Parcostframe, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(Parscr,...),background="white")
	Costtext<-"Cost variables can be used to adjust sample sizes to minimise the \noverall cost of sampling. The participant cost refers to the cost associated \nwith asking an individual participant to participate in the survey. The \nPSU cost refers to the cost associated with setting up a Primary Sampling\n Unit for sampling."
	infoCost<-info.bn(Opt, Costtext)

	tkgrid(PSUcostlabel, sticky="w")
	tkgrid.configure(PSUcostBox, costscr, sticky="nsew")
	tkgrid(Parcostlabel, sticky="w")
	tkgrid.configure(ParcostBox, Parscr, sticky="nsew")
	tkgrid(PSUcostframe, Parcostframe, infoCost, sticky="w")
	
#Cost check box
	costValue<-tclVar("0")
	costCB<-tkcheckbutton(Opt,text="No cost information is available",command=function()costdisable())
	tkconfigure(costCB, variable=costValue)
	tkgrid(costCB, sticky="w", columnspan=2)
	tkgrid(tklabel(Opt, text=""))

#PSU Variables frame
	PSUVframe<-tkframe(Opt)
	PSUVscr <- tkscrollbar(PSUVframe, repeatinterval=5,command=function(...)tkyview(PSUVBox,...))
	PSUVlabel<-tklabel(PSUVframe,text="The VARIANCE variable is:", fg="blue")
	PSUVBox<-tklistbox(PSUVframe, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(PSUVscr,...),background="white")
	tkgrid(PSUVlabel, sticky="w")
	tkgrid.configure(PSUVBox, PSUVscr, sticky="we")

	PSUmeanframe<-tkframe(Opt)
	PSUmscr <- tkscrollbar(PSUmeanframe, repeatinterval=5,command=function(...)tkyview(PSUmeanBox,...))
	PSUmeanlabel<-tklabel(PSUmeanframe,text="The MEAN variable is:", fg="blue")
	PSUmeanBox<-tklistbox(PSUmeanframe, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(PSUmscr,...),background="white")
	tkgrid(PSUmeanlabel, sticky="w")
	Varitext<-"The mean and variance of a key variable can be used to adjust \nsample sizes to minimise the overall variance. Only one variable can \nbe used for the optimisation, however the sample size process may \nbe repeated for other variables" 
	infoVari<-info.bn(Opt, Varitext)
	tkgrid.configure(PSUmeanBox, PSUmscr, sticky="we")
	tkgrid(PSUmeanframe, PSUVframe, infoVari, sticky="w")

#Variable check box
	optimalValue<-tclVar("0")
	optimalCB<-tkcheckbutton(Opt,text="No variable information is available",command=function()variabledisable())
	tkconfigure(optimalCB, variable=optimalValue)
	tkgrid(optimalCB, sticky="w", columnspan=2)
	tkgrid(tklabel(Opt, text=""))

#Put in variables Variables
	boxes1<-list(PSUBox.b, sizeBox.b, strataBox.b, demoBox, PSUVBox, PSUmeanBox)
	for (j in 1:length(boxes1)) for (i in 1:length(variables)) tkinsert(boxes1[[j]], "end", variables[i])

	boxes2<-list(strataBox.d, mkBox, ssBox, PSUcostBox, ParcostBox)
	for (j in 1:length(boxes2)) for (i in 1:length(variables2)) tkinsert(boxes2[[j]], "end", variables2[i])


# autoSelection between PSU and strata Datasets

StrataVarSelect<-function(){
	if (length(as.numeric(tkcurselection(strataBox.b)))>0) {
		strata.variable<-variables[as.numeric(tkcurselection(strataBox.b))+1]
		strata.selection<-ifelse(is.na(match(strata.variable, variables2)), NA, match(strata.variable,variables2)-1)
		tkselection.set(strataBox.d, strata.selection)
		}
	}

	tkbind(strataBox.b, "<ButtonRelease-1>", function()StrataVarSelect())

### DISABLING ###
	costdisable<-function(){
		b<-list(ParcostBox, Parcostlabel, PSUcostBox, PSUcostlabel)
		if (tclvalue(costValue)==1) setstate2(b, "disabled")
		if (tclvalue(costValue)==0) setstate2(b, "normal")
		}

	variabledisable<-function(){
		a<-list(PSUVBox, PSUVlabel, PSUmeanBox, PSUmeanlabel)
		if (tclvalue(optimalValue)==0) setstate2(a, "normal") 
		if (tclvalue(optimalValue)==1) setstate2(a, "disabled")
		}

	psdisable<-function(){
		if (tclvalue(ppsValue)==1) tkconfigure(pps2Field, state="disabled")
		if (tclvalue(ppsValue)==2) tkconfigure(pps2Field, state="normal")
		if (tclvalue(ppsValue)==3) tkconfigure(pps2Field, state="disabled")
		}

	ssdisable<-function(){
		disable.fn(ssValue, ssvarlabel, ssBox)
		w<-list(sstotallabel, ssField, rbpps1, rbpps2, rbpps3, ppslabel, pps1label, pps2label, 
			pps3label, pps2Field)
		if (tclvalue(ssValue)==1) {setstate2(w, "normal")
			psdisable() }
		if (tclvalue(ssValue)==0) setstate2(w, "disabled")		
		}

	mk.variable<-function(){
		disable.fn(mkValue, mkBox, mkvarlabel)
		if (tclvalue(mkValue)==1) {
			tclvalue(costValue)<-0
			tclvalue(optimalValue)<-0
			costdisable()
			variabledisable()
			tkconfigure(costCB, state="disabled")
			tkconfigure(optimalCB, state="disabled")
			}	
		if (tclvalue(mkValue)==0) {	
			tkconfigure(costCB, state="normal")
			tkconfigure(optimalCB, state="normal")
			}
		}

	v<-list(strataBox.d, stratalabel.d)
	if (stratayesno=="1") setstate2(v, "disabled")
		

### Initial disabling ###
	ssdisable()
	mk.variable()
	variabledisable()	

### ON OK FUNCTION ###
review.fn<-function(){
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
		
		#From PSU details
	 	PSU1 <- as.numeric(tkcurselection(PSUBox.b))+1
	 	strata1 <- as.numeric(tkcurselection(strataBox.b))+1
	 	size1 <- as.numeric(tkcurselection(sizeBox.b))+1
		demo1 <- as.numeric(tkcurselection(demoBox))+1
		demoyesno<-tclvalue(demoValue) # 0 is using demographics, 1 is not using

		#from strata details
		strata2<-as.numeric(tkcurselection(strataBox.d))+1
		mkvar<-as.numeric(tkcurselection(mkBox))+1
		mkvalue<-tclvalue(mkValue) #0 is strataset contains mk value, 1 is optim
		ssyesno<-tclvalue(ssValue) # 1 for no sss variable, 0 for sss variable
		ssvar<-as.numeric(tkcurselection(ssBox))+1
		sstotal<-as.numeric(tclvalue(sstotal)) #total sample size
		ppstype<-tclvalue(ppsValue) #1 for PPS, 2 for PPS with min, 3 for fixed, 4 for neymans
		nminimum<-as.numeric(tclvalue(pps2))

		# from opt sampling
		costyesno<-tclvalue(costValue) #0 is there is cost information, 1 is no information
		optyesno<-tclvalue(optimalValue)  #0 is there are variable information, 1 is no information 
		Parcost <- as.numeric(tkcurselection(ParcostBox))+1
		PSUcost <- as.numeric(tkcurselection(PSUcostBox))+1
		PSUV<-as.numeric(tkcurselection(PSUVBox))+1
		PSUmean<-as.numeric(tkcurselection(PSUmeanBox))+1

	### CHECKS ###
		check.fn<-function(v, nbtab){
			mes<-paste("Please select a ", v, " variable", sep="")
 			tkmessageBox(message=mes)
			tk2notetab.select(nb, nbtab)		
		}

		if (length(PSU1)==0) {check.fn("PSU", "PSU Details") 
			return()}
		if (length(strata1)==0) {check.fn("STRATA", "PSU Details") 
			return()}
		if (length(size1)==0) {check.fn("SIZE", "PSU Details") 
			return()}
		if (length(demo1)==0 & demoyesno==0) {check.fn("DEMOGRAPHIC", "Strata Details") 
			return()}
		if (length(strata2)==0 & stratayesno==0) {check.fn("STRATIFICATION", "Strata Details") 
			return()}
		if (length(mkvar)==0 & mkvalue==0) {check.fn("STAGE 1 SAMPLE SIZE", "Strata Details") 
			return()}
		if (length(ssvar)==0 & ssyesno==0) {check.fn("STRATA SAMPLE SIZE", "Strata Details") 
			return()}
		if (length(PSUV)==0 & optyesno==0) {check.fn("PSU VARIANCE", "Optimal Sampling") 
			return()}
		if (length(PSUV)==0 & ppstype==4) {check.fn("PSU VARIANCE", "Optimal Sampling") 
			return()}
		if (length(PSUmean)==0 & optyesno==0) {check.fn("PSU MEAN", "Optimal Sampling") 
			return()}
		if (length(Parcost)==0 & mkvalue==1) {check.fn("PARTICIPANT COST", "Optimal Sampling") 
			return()}
		if (length(PSUcost)==0 & mkvalue==1) {check.fn("PSU COST", "Optimal Sampling") 
			return()}
	
		#check for strata name mismatches between datasets
		if (stratayesno==0){
			bad<-is.element(get(df1)[,strata1], get(df2)[,strata2])
			bad2<-is.element(get(df2)[,strata2], get(df1)[,strata1])
			if (is.element("FALSE", bad)|is.element("FALSE", bad2)) {
				tkmessageBox(message="Not all of the strata names match between the two datasets provided. Please check them!")
				tk2notetab.select(nb, "Strata Details")
				return()
				}
			}

	### Print out function ###	
		closeDialog()
		command<-paste("SampleSizes(df1=",df1, ", df2=",df2,", PSU1=",PSU1,", strata1=",strata1,", size1=",size1,", demo1=",list(demo1),", demoyesno=", demoyesno, 
		", strata2=",strata2,", stratayesno=", stratayesno,
		", mkvar=", mkvar,", mkvalue=", mkvalue,", ssyesno=", ssyesno, ", ssvar=", ssvar, ", sstotal=", sstotal, ", ppstype=", ppstype,
		", nminimum=", nminimum, ", Parcost=", Parcost, ", PSUcost=",PSUcost,
		", PSUV=",PSUV,", PSUmean=", PSUmean, ", optyesno=", optyesno, ", costyesno=", costyesno, ")\n",sep="")
		samplesizedatasets<-justDoIt(command)

       ### Add in report ###
	if (reportyesno==1) {
		
		if (costyesno==0) costtotal<-round(sum(samplesizedatasets$StrataSS$ST1_COST))
		df1names<-names(get(df1))
		df2names<-names(get(df2))
		
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
		if (ssyesno==1 & ppstype==3) "\nThe strata sample sizes were selected to be a fixed size ",
		if (ssyesno==1 & ppstype==4) "\nThe strata sample sizes were selected to distributed using Neymans Allocation",
		if (mkvalue==1 & optyesno==0) c("\nOptimal sample sizes were calculated using the PSU dataset mean and variance variables: ", df1names[PSUmean], df1names[PSUV]),
		if (costyesno==0) c("\nThe cost was calculated using the variables: ", df2names[PSUcost], df2names[Parcost]),
		"\n\nAdditional Details...\nThe total sample size was: ", sstotal,
		if (costyesno==0) c("\nThe total cost came to: ", costtotal),
		if (optyesno==0 & demoyesno==1) c("\nThe weighted mean for the key variable is estimated as: ", signif(T.Var,digits=4), "\nThe overall variance for the key variable is estimated as: ", signif(TotalVar, digits=4)),
		if (optyesno==0 & demoyesno==1) c("\nThe Coefficient of Vaiation (as a percentage) for the key variable is estimated as: ", signif(COEF, digits=4), "\nThe Design effect is estimated as: ", signif(Deff, digits=4)),
		"\n", file=reportname)
		}

		doItAndPrint(paste("samplesizedatasets<-", command, sep=""))
		doItAndPrint("StrataSampleSize<-samplesizedatasets$StrataSS")
		doItAndPrint("PSUSampleSize<-samplesizedatasets$PSUSS\n")
		activeDataSet("PSUSampleSize")
		View(samplesizedatasets$StrataSS, title="StrataSampleSize")
		tkfocus(CommanderWindow())
		}
	OKCancelHelp(helpSubject="sample", window=samplecheck)
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	dialogSuffix(rows=3, window=samplecheck, focus=buttonsFrame)
	}
	onOK<-function(){}
	OKCancelHelp(helpSubject="sample", window=top)
	dialogSuffix(focus=next.bn)
}