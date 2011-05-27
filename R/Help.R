dataonHelp<-function(){
	initializeDialog(title="Stage 1 Sampling Help", window=topp)
	topp<-tkframe(topp, borderwidth=20)
	fontHeading <- tkfont.create(family="times",size=12,weight="bold",slant="italic")
	tkgrid(tklabel(topp, text="Primary Sampling Unit Dataset", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The Primary Sampling Unit (PSU) dataset should contain a list of all the PSUs and \ntheir sample sizes and inclusion probabilities. If second stage stratifications \n(domains) are used, the dataset should have one line for each domain in each PSU.", justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(tklabel(topp, text="Stratification Dataset", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The Stratification dataset should contain a summary list of the Strata and \ntheir sizes. The dataset should have one line per strata.", justify="left"), sticky="w")
	tkblank(topp)

	tkgrid(topp)
	tkgrab.set(topp)
        tkfocus(topp)
	}

PSUonHelp<-function(){
	initializeDialog(title="Stage 1 Sampling Help", window=topp)
	topp<-tkframe(topp, borderwidth=20)
	fontHeading <- tkfont.create(family="times",size=12,weight="bold",slant="italic")
	
	tkgrid(tklabel(topp, text="Primary Sampling Unit Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The PSU variable should contain a unique identifying number or name \nfor each Primary Sampling Unit.", justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(tklabel(topp, text="Stratification Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The STRATA variable should contain a unique identifying number or name \nfor each STRATUM.", justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(tklabel(topp, text="Size Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The SIZE variable should list the size of each PSU. This may be in terms of the \npopulation size or the number of addresses/dwellings for each Primary Sampling Unit.\n", justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(tklabel(topp, text="Second Stage Stratification Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The Second Stage Stratification variables (domains) should contain \ninformation on the size of the domain within each PSU. We recommend \nusing age groups and sex as secondary stratification if available.", justify="left"), sticky="w")

	tkgrid(topp)
	tkgrab.set(topp)
        tkfocus(topp)
	}

strataonHelp<-function(){
	initializeDialog(title="Stage 1 Sampling Help", window=topp)
	topp<-tkframe(topp, borderwidth=20)
	fontHeading <- tkfont.create(family="times",size=12,weight="bold",slant="italic")
	
	tkgrid(tklabel(topp, text="Stratification Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The STRATA variable should contain a unique identifying number or name \nfor each STRATUM. This should be identical in both PSU and strata datasets.", justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(tklabel(topp, text="Stage 1 Sample Size Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The STAGE 1 SAMPLE SIZE variable is a list of the number of PRIMARY SAMPLING UNITS to be sampled in each stratum.\n", justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(tklabel(topp, text="Strata Sample Sizes Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The STRATA SAMPLE SIZES variable is a list of the number of INDIVIDUAL Units (SSUs) to be sampled in each stratum.\n", justify="left"), sticky="w")
	
	tkgrid(topp)
	tkgrab.set(topp)
        tkfocus(topp)
	}

costonHelp<-function(){
	initializeDialog(title="Stage 1 Sampling Help", window=topp)
	topp<-tkframe(topp, borderwidth=20)
	fontHeading <- tkfont.create(family="times",size=12,weight="bold",slant="italic")
	tkgrid(tklabel(topp, text="Cost Variables", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="Cost variables can be used to adjust sample sizes to minimise the \noverall cost of sampling. The participant cost refers to the cost associated \nwith asking an individual participant to participate in the survey. The \nPSU cost refers to the cost associated with setting up a Primary Sampling\n Unit for sampling.", justify="left"), sticky="w")
	tkgrid(topp)
	tkgrid(tklabel(topp, text="Mean and Variance Variables", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The mean and variance of a key variable can be used to adjust \nsample sizes to minimise the overall variance. Only one variable can \nbe used for the optimisation, however the sample size process may \nbe repeated for other variables", justify="left"), sticky="w")
	
	tkgrid(topp)
	tkgrab.set(topp)
        tkfocus(topp)
	}
	

reportonHelp<-function(){
	initializeDialog(title="Write a report", window=topp)
	topp<-tkframe(topp, borderwidth=20)
	fontHeading <- tkfont.create(family="times",size=12,weight="bold",slant="italic")
	tkgrid(tklabel(topp, text="Ensure the box is check to create a summary report of the choices made \nduring the sampling process. The report will be saved in the working \ndirectory", justify="left"), sticky="w")

	tkgrid(topp)
	tkgrab.set(topp)
        tkfocus(topp)
	}

Stage1onHelp<-function(){
	initializeDialog(title="Stage 1 Sampling Help", window=topp)
	topp<-tkframe(topp, borderwidth=20)
	fontHeading <- tkfont.create(family="times",size=12,weight="bold",slant="italic")
	tkgrid(tklabel(topp, text="Primary Sampling Unit Dataset", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The Primary Sampling Unit (PSU) dataset should contain a list of all the PSUs and \ntheir sample sizes and inclusion probabilities. If second stage stratifications \n(domains) are used, the dataset should have one line for each domain in each PSU.", justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(tklabel(topp, text="Primary Sampling Unit Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The PSU variable should contain a unique identifying number or name \nfor each Primary Sampling Unit.", justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(tklabel(topp, text="Stratification Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The STRATA variable should contain a unique identifying number or name \nfor each STRATUM.", justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(tklabel(topp, text="First Stage Inclusion Probabilbilty Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The first stage inclusion probability is how likely each PSU is to being \nselected for sampling in the first stage. This can be calculated with \nthe sample sizes option in the RcmdrPluing.EHES package.", justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(topp)
	tkgrab.set(topp)
        tkfocus(topp)
	}

Stage2onHelp<-function(){
	initializeDialog(title="Stage 2 Sampling Help", window=topp)
	topp<-tkframe(topp, borderwidth=20)
	fontHeading <- tkfont.create(family="times",size=12,weight="bold",slant="italic")
	tkgrid(tklabel(topp, text="Primary Sampling Unit Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The PSU variable should contain a unique identifying number or name \nfor each Primary Sampling Unit.", justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(tklabel(topp, text="Stratification Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The STRATA variable should contain a unique identifying number or name \nfor each STRATUM. If you are sampling every PSU, this is called a \none stage sample and the dataset does not need a strata variable.\nIn the case of a one stage sample, select the one stage sample \ncheck box.", justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(tklabel(topp, text="Stage 2 Stratification Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The Stage 2 Stratification Variable is for sex-age (domain) groups in sampling individuals.", justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(tklabel(topp, text="Second Stage Inclusion Probabilbilty Variable", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="The second stage inclusion probability is how likely each individual person \nor address is to be selected in the sample. This can be calculated with the sample \nsizes option in the RcmdrPluing.EHES package.",justify="left"), sticky="w")
	tkblank(topp)
	tkgrid(tklabel(topp, text="Create ID numbers", font=fontHeading), sticky="w")
	tkgrid(tklabel(topp, text="Choose this option to create an ID number for all the selected participants \nin the survey. The ID number is encoded with a control digit using the dihedral \ngroup of order 10 addition and can be checked later using the \nCHECK ENCODED NUMBERS option.",justify="left"), sticky="w")
	tkgrid(topp)
	tkgrab.set(topp)
        tkfocus(topp)
	}

