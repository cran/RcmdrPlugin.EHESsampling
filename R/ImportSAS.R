importSAS<-function(){
	require(Hmisc)
	initializeDialog(title="Import SAS dataset")

   ### Name for the dataset ###
	dataname<-tclVar("Dataset")
	nameField<-tkentry(top, width="20", textvariable=dataname)
	tkgrid(labelRcmdr(top, text="Enter a name for the imported dataset:", fg="blue"), nameField, sticky="w")
	tkgrid(tklabel(top, text=""))

   ### Locate the file to open ###
	fileframe<-tkframe(top)
	filevar<-tclVar("")
	filefield<-tkentry(fileframe, width="40", textvariable=filevar)
	tkconfigure(filefield, textvariable=filevar)
	file.bn<-ttkbutton(fileframe, text="Browse", command=function()getfile())
	tkgrid(labelRcmdr(fileframe, text="Enter the file to import:", fg="blue"), filefield, file.bn)
	tkgrid(fileframe, sticky="w", columnspan=3)
	tkgrid(tklabel(top, text=""))

   ### Format file ###
	formatframe<-tkframe(top)
	formatlabel<-tklabel(formatframe, text="Enter the format file:", fg="blue")
	formatvar<-tclVar("")
	formatfield<-tkentry(formatframe, width="42", textvariable=formatvar)
	tkconfigure(formatfield, textvariable=formatvar)
	format.bn<-ttkbutton(formatframe, text="Browse", command=function()getformat())
	tkgrid(formatlabel, formatfield, format.bn)
	tkgrid(formatframe, sticky="w", columnspan=3)

  ### check box ###
	formatValue<-tclVar("1")
	formatCB<-tkcheckbutton(top, text="There is no format file",command=function()disable())
	tkconfigure(formatCB, variable=formatValue)
	tkgrid(formatCB, sticky="w")
	tkgrid(tklabel(top, text=""))

   ### Locate SAS on the computer ###
	SASframe<-tkframe(top)
	if (!exists("SASlocation")) SASlocation<-""
	SASvar<-tclVar(SASlocation)
	SASfield<-tkentry(SASframe, width="30", textvariable=SASvar)
	SAS.bn<-ttkbutton(SASframe, text="Browse", command=function()getSAS())
	tkgrid(tklabel(SASframe, text="Enter the path to the program SAS:", fg="blue"), SASfield, SAS.bn, sticky="w")
	tkgrid(tklabel(SASframe, text=""))
	tkgrid(SASframe, sticky="new", columnspan=3)

   ### Place file name in field
	getfile<-function(){
		file<-tclvalue(tkgetOpenFile(title="Select SAS File", filetypes='{"SAS datasets" {".sas7bdat"}} {"All Files" {"*"}}'))
		tclvalue(filevar)<-file
		}

   ### Place format file in field
	getformat<-function(){
		file<-tclvalue(tkgetOpenFile(title="Select SAS Format File", filetypes='{"SAS format" {".sas7bcat" ".sc" ".sc2" ".sct"}} {"All Files" {"*"}}'))
		tclvalue(formatvar)<-file
		}

   ### Place SAS location in field
	getSAS<-function(){
		path<-tclvalue(tkchooseDirectory(title="Please Choose the directory where the program SAS is located"))
		tclvalue(SASvar)<-path
		}

   ### Disabling ###
	disable<-function(){
		if (tclvalue(formatValue)==0){
			tkconfigure(formatlabel, state="normal")
			tkconfigure(formatfield, state="normal")}
		if (tclvalue(formatValue)==1){
			tkconfigure(formatlabel, state="disabled")
			tkconfigure(formatfield, state="disabled")}
		}
	disable()	

   ### Split up file name into location and name
	getname<-function(x){
		n<-unlist(strsplit(x, "/"))
		n2<-n[length(n)]
		n3<-unlist(strsplit(n2, "\\."))[1]
		return(n3)
		}

	getlibrary<-function(x) {
		n<-unlist(strsplit(x, "/"))
		n2<-paste(n[1:length(n)-1], sep="", collapse="/")
		return(n2)
		}

   ### On OK ###
   	onOK<-function(){
		closeDialog()
		ds1<-trim.blanks(tclvalue(dataname))
		n<-getname(tclvalue(filevar))
		lib<-getlibrary(tclvalue(filevar))
		f<-tclvalue(formatvar)
		formatyesno<-tclvalue(formatValue)
		v<-sessionInfo()$other$Hmisc$Version
		if (v<="3.7-0") {
			tkmessageBox(message="Warning: You are using an old version of Hmisc that might not work properly")
			}
		SASlocation<-tclvalue(SASvar)
		doItAndPrint(paste("SASlocation<-\"", tclvalue(SASvar), "\"", sep=""))
		command<- if (formatyesno=="1"){
			paste(ds1, "<-sas.get(libraryName = \"", lib, "\", member = \"", n, "\", sasprog = \"", SASlocation, "/sas.exe\", formats=FALSE)", sep="")
			} else {
			paste(ds1, "<-sas.get(libraryName = \"", lib, "\", member = \"", n, "\", format.library = \"", f, "\", sasprog = \"", SASlocation, "/sas.exe\")", sep="")
			}
		doItAndPrint(command)
		activeDataSet(ds1)
        	tkfocus(CommanderWindow())
		}

   ### final bits ###
	OKCancelHelp(helpSubject="sas.get")
	tkgrid(buttonsFrame, columnspan=3, sticky="w")
	dialogSuffix(rows=4, columns=2, focus=buttonsFrame)
	}
