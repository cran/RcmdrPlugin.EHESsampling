Check<-function() {
	initializeDialog(title="Check Numbers")

#Dataset box
	dataFrame<-tkframe(top)
	tkgrid(tklabel(dataFrame, text="Select the dataset:", fg="blue"), sticky="w")
	dataBox<-tklistbox(dataFrame, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(datascr,...),background="white")
	datascr <- tkscrollbar(dataFrame, repeatinterval=5,command=function(...)tkyview(dataBox,...))
	tkgrid.configure(dataBox, datascr, sticky="nsw")
	variables<-listDataSets()
	for (i in 1:length(variables)) tkinsert(dataBox, "end", variables[i])
	datainfo<-"Select the data set from the list which contains the list of encoded numbers to check. \nThen select the variable of encoded numbers."
	data.bn<-info.bn(top, datainfo)
	tkgrid(tklabel(top, text=""))

#Variable Box
	varFrame<-tkframe(top)
	tkgrid(tklabel(varFrame, text="Select the variable to check:", fg="blue"), sticky="w", columnspan=3)
	varBox<-tklistbox(varFrame, height=4, selectmode="single", export="FALSE", yscrollcommand=function(...)tkset(varscr,...),background="white")
	varscr <- tkscrollbar(varFrame, repeatinterval=5,command=function(...)tkyview(varBox,...))
	tkgrid.configure(varBox, varscr, sticky="nsw")

#Preview button
	prev.bn<-tk2button(top, text="Variable Preview", command=function()VarPreview())	
	VarPreview<-function(){
		ds<-variables[as.numeric(tkcurselection(dataBox))+1]
		var<-as.numeric(tkcurselection(varBox))+1
		if (length(ds)==0 | length(var)==0) {tkbell()} else {
			initializeDialog(title="")
			datanames<-names(get(ds))[var]
			data<-as.matrix(get(ds)[c(1:11),var])
			data<-rbind(datanames, data)
			for (i in (0:10))
				for (j in (0:0))
					.Tcl(paste("set tclarray(", i, ",", j, ") ", data[i+1, j+1], sep=""))	
			table1<-tk2table(top, variable="tclarray", background="white", rows=11, cols=1, colwidth = "20")
			tkpack(table1)
			tkbind(top,"<Destroy>",function()tkfocus(CommanderWindow()))
			onOK<-function(){}
			dialogSuffix(focus=top)
			}
		}

	tkgrid(dataFrame, varFrame, prev.bn, data.bn, sticky="ew", columnspan=3)
	tkgrid(tklabel(top, text=""))

### The number of digits ###
	zframe<-tkframe(top)
	zlabel<-tklabel(zframe, text="Enter how many digits the numbers contains:", fg="blue") 
	zvar<-tclVar("8")
	zField<-tkentry(zframe, width="3", textvariable=zvar)
	zinfo<-"Enter the number of digits, each number should contain. \nThe function will check that all numbers contain this number of digits."
	z.bn<-info.bn(top, zinfo)
	tkgrid(zlabel, zField)
	tkgrid(zframe, z.bn, sticky="nw", columnspan=5)
	tkgrid(tklabel(top, text=""))

### insert Variables function ###
	getvars<-function(){
		if (length(as.numeric(tkcurselection(dataBox)))==0|length(variables)==0) {
			variables2<-"NA"} else {
			variables2<-names(get(variables[as.numeric(tkcurselection(dataBox))+1]))
			}
		tkdelete(varBox,0,"end")
		for (i in 1:length(variables2)) tkinsert(varBox, "end", variables2[i])
	}
	tkbind(dataBox, "<ButtonRelease-1>", function()getvars())

### On Ok button and function ###
	onOK<-function(){
		ds<-variables[as.numeric(tkcurselection(dataBox))+1]
		x<-as.numeric(tkcurselection(varBox))+1
		z<-as.numeric(tclvalue(zvar))
		if (length(ds)==0) {
			tkmessageBox(message="Please select a dataset")
			return()
			}
		if (length(x)==0) {
			tkmessageBox(message="Please select a variable")
			return()
			}
		closeDialog()
		doItAndPrint(paste("Problem_Numbers<-ListCheck(dataset=", ds, ", x=", x, ", z=", z, ")", sep=""))
		if (length(Problem_Numbers)>0) {
			tkmessageBox(message="Incorrect numbers have been detected. The dataset Problem_Numbers contains those that are incorrect")
			View(Problem_Numbers)
		} else {
			tkmessageBox(message="No incorrect numbers were been detected.")
			Problem_Numbers<-NULL
		}
	}

	OKCancelHelp(window=top)
	tkgrid(buttonsFrame, columnspan=5, sticky="w")
	dialogSuffix(rows=3, columns=2, focus=buttonsFrame)
}
