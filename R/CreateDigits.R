CreateDigits<-function(){
	initializeDialog(title="Create Encoded Numbers")

#Select a name form the new variable
	nameframe<-tkframe(top)
	dataname<-tclVar("Encoded_Number")
	namelabel<-tklabel(nameframe, text="Enter variable name for the new encoded numbers", fg="blue")
	nameField<-tkentry(nameframe, width="20", textvariable=dataname)
	nameinfo<-"If the numbers are to be attached to an existing data set, a new column will be created with \nthe complete encoded numbers including the control digit. The name entered as the variable \nname, will be the column heading for the new variable. If there is no data set, a vector of the \nspecified number if numbers will eb created with the vector name as that specified. "
	name.bn<-info.bn(top, nameinfo)
	tkgrid(namelabel, nameField)	
	tkgrid(nameframe, name.bn, sticky="w", columnspan=2)

### THe dataset to attach the numbers to...
	datalabel<-tklabel(top, text="Select the dataset to add the numbers to:", fg="blue")
	dataFrame<-tkframe(top)
	dataBox<-tklistbox(dataFrame, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(datascr,...),background="white")
	datascr <- tkscrollbar(dataFrame, repeatinterval=5,command=function(...)tkyview(dataBox,...))
	tkgrid.configure(dataBox, datascr, sticky="nsew")
	variables<-listDataSets()
	for (i in 1:length(variables)) tkinsert(dataBox, "end", variables[i])
	view.but<-tk2button(top, text="dataset preview", command=function()DataPreview(dataBox))
	datainfo<-"Select the data set from the list which encoded numbers will be added to. The data set should \ncontain one line for each individual that requires a number. If numbers will not be attached \nto a datset, check the box below that says: There is no dataset. If no data set is selected, \nchoose how many encoded numbers to create in the field."
	data.bn<-info.bn(top, datainfo)
	tkgrid(tklabel(top, text=""))
	tkgrid(datalabel, sticky="w")
	tkgrid(dataFrame, view.but, data.bn, sticky="w")


### Check box with no data set option
	dataValue<-tclVar("0")
	dataCB<-tkcheckbutton(top, text="There is no dataset",command=function()disabledata())
	tkconfigure(dataCB, variable=dataValue)
	tkgrid(dataCB, sticky="w")

### The number of numbers to create ###
	nframe<-tkframe(top)
	nlabel<-tklabel(nframe, text="Enter how many encoded numbers to create:", fg="blue") 
	nvar<-tclVar("50")
	nField<-tkentry(nframe, width="8", textvariable=nvar)
	tkgrid(nlabel, nField)
	tkgrid(nframe, sticky="e")
	tkgrid(tklabel(top, text=""))

### The number of digits ###
	xframe<-tkframe(top)
	xlabel<-tklabel(xframe, text="Enter how many digits to use:", fg="blue") 
	xvar<-tclVar("8")
	xField<-tkentry(xframe, width="3", textvariable=xvar)
	tkgrid(xlabel, xField)
	digitinfo<-"Enter how many digits each number should contain, the more digits there are, the more secure \nthe number but it will have a higher chance of typing errors. It is not recommended to use more \nthan 12 digits for EHES."
	digit.bn<-info.bn(top, digitinfo)
	tkgrid(xframe, digit.bn, sticky="nw")
	tkgrid(tklabel(top, text=""))

###Disable
disabledata<-function() {
	if (tclvalue(dataValue)==0) {
		tkconfigure(nlabel, state="disabled")
		tkconfigure(nField, state="disabled")
		tkconfigure(dataBox, state="normal")
		tkconfigure(datalabel, state="normal")
		}
	if (tclvalue(dataValue)==1) {
		tkconfigure(nlabel, state="normal")
		tkconfigure(nField, state="normal")
		tkconfigure(dataBox, state="disabled")
		tkconfigure(datalabel, state="disabled")
		}
	}
disabledata()

### On Ok button and function ###
	onOK<-function(){
		dn<-trim.blanks(tclvalue(dataname))
		ds<-variables[as.numeric(tkcurselection(dataBox))+1]
		x<-as.numeric(tclvalue(xvar))
		datayesno<-tclvalue(dataValue)
		n<-ifelse(datayesno==0, nrow(get(ds)), as.numeric(tclvalue(nvar)))
		
		if (length(ds)==0 & datayesno==0) {
			tkmessageBox(message="Please select a dataset")
			return()
			}

		if (x<3) {
			tkmessageBox(message="The minimum number of digits to use is 3.")
			tclvalue(xvar)<-3
			return()
			}

		if ((10^(x-1))<n) {
			tkmessageBox(message="The number of digits selected is not enough to provide unique numbers with this function. \nPlease increase the number of digits or decrease how many numbers to create.")
			return()
			}

		closeDialog()
		if (datayesno==0) doItAndPrint(paste(ds, "$", dn, "<-Create.control(x=", x, ", n=", n, ")", sep=""))
		if (datayesno==1) doItAndPrint(paste(dn, "<-Create.control(x=", x, ", n=", n, ")", sep=""))		
	}
	OKCancelHelp(window=top)
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	dialogSuffix(rows=3, columns=2, focus=buttonsFrame)
}




