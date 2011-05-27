ControlDigit<-function(){
	initializeDialog(title="Calculate a control digit")

#Select a name form the new variable
	nameframe<-tkframe(top)
	dataname<-tclVar("Encoded_Number")
	namelabel<-tklabel(nameframe, text="Enter variable name for the new encoded numbers", fg="blue")
	nameField<-tkentry(nameframe, width="20", textvariable=dataname)
	nameinfo<-"A new column will be created with the complete encoded numbers including the control digit. The name entered as the variable name, will be the column heading for the new variable."
	name.bn<-info.bn(top, nameinfo)
	tkgrid(namelabel, nameField)	
	tkgrid(nameframe, name.bn, sticky="w", columnspan=2)

#Dataset box
	dataFrame<-tkframe(top)
	tkgrid(tklabel(dataFrame, text="Select the dataset:", fg="blue"), sticky="w")
	dataBox<-tklistbox(dataFrame, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(datascr,...),background="white")
	datascr <- tkscrollbar(dataFrame, repeatinterval=5,command=function(...)tkyview(dataBox,...))
	datainfo<-"Select the dataset which contains the list of numbers that will have a control digit added \nto them. Then select the variable from the list which corresponds to the numbers for which \na control digit will be added."
	data.bn<-info.bn(top, datainfo)
	tkgrid.configure(dataBox, datascr, sticky="nsw")
	variables<-listDataSets()
	for (i in 1:length(variables)) tkinsert(dataBox, "end", variables[i])
	tkgrid(tklabel(top, text=""))

#Variable Box
	varFrame<-tkframe(top)
	tkgrid(tklabel(varFrame, text="Select the variable to encode:", fg="blue"), sticky="w", columnspan=3)
	varBox<-tklistbox(varFrame, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(varscr,...),background="white")
	varscr <- tkscrollbar(varFrame, repeatinterval=5,command=function(...)tkyview(varBox,...))
	tkgrid.configure(varBox, varscr, sticky="nsw")
	tkgrid(dataFrame, varFrame, data.bn, sticky="new")
	tkgrid(tklabel(top, text=""))

#insert Variables function
	getvars<-function(){
		if (length(as.numeric(tkcurselection(dataBox)))==0|length(variables)==0) {
			variables2<-"NA"} else {
			variables2<-names(get(variables[as.numeric(tkcurselection(dataBox))+1]))
			}
		tkdelete(varBox,0,"end")
		for (i in 1:length(variables2)) tkinsert(varBox, "end", variables2[i])
	}
	tkbind(dataBox, "<ButtonRelease-1>", function()getvars())

#On Ok button and function
	onOK<-function(){
		if (length(variables[as.numeric(tkcurselection(dataBox))+1])==0) {
				tkmessageBox(message="Please select a dataset")
				return()
				}
		if (length(variables[as.numeric(tkcurselection(varBox))+1])==0) {
				tkmessageBox(message="Please select a variable")
				return()
				}

		dataset1<-variables[as.numeric(tkcurselection(dataBox))+1]
		x<-as.numeric(tkcurselection(varBox))+1
		bad<-is.na(as.double(get(dataset1)[,x]))
		if (is.element(TRUE, bad)) {
			tkmessageBox(message="The variable selected contains letters. This option is not available \nyet. Please try a different variable.")
			return()
			}
		closeDialog()
		initializeDialog(title="Processing Control Digit")
		tkgrid(tklabel(top, text="Control digits are being added. Please wait."))
		doItAndPrint(paste(dataset1, "$", tclvalue(dataname), "<-Add.control(dataset=", dataset1, ", x=",x, ")", sep=""))
		closeDialog()	
	}

	OKCancelHelp(window=top)
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	dialogSuffix(rows=3, columns=2, focus=buttonsFrame)
}
