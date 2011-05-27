.packageName<-"RcmdrPlugin.EHESsampling"

#Internal functions for EHES plugin

.First.lib <- function(libname, pkgname){ 
	if (!interactive()) return()
	Rcmdr<-options()$Rcmdr
	plugins <- Rcmdr$plugins
	if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
		Rcmdr$plugins <- c(plugins, pkgname)
		options(Rcmdr=Rcmdr)
		closeCommander(ask=FALSE, ask.save=TRUE)
		Commander()
		}
	}

OpenSampleSizes<-function(){
	if (length(listDataSets())==0) tkmessageBox(message="Please import the PSU and Stratification datasets \nbefore trying to calculate sample sizes")
	Sample.datasets()
	}

OpenSampling1<-function()Stage1.notebook()


OpenSampling2<-function()Stage2.notebook()


mk.calc<-function(x,y,z) {
	mk.opt<-x/y
	mk<-ifelse(mk.opt<2,2,mk.opt)
	mk<-ifelse(z==1,1,mk) 
	mk<-ifelse(z<mk,floor(mk),mk)
	mk<-special.round(mk)
	return(mk)
	}


special.round <-function (x){
	uk<-x-floor(x)
	diff<-sum(x)-floor(sum(x))
	number<-ifelse(diff<0.5,floor(sum(uk)),ceiling(sum(uk)))
	order.x<-order(-uk)
	x<-x[order(-uk)]
	x<-c(ceiling(x[if(number==0) 0 else seq(number)]), 
	floor(x[(number+1):length(x)])) 
	return(x[order(order.x)])
	}


DataPreview<- function(Box){
	variables<-listDataSets()
	ds<-variables[as.numeric(tkcurselection(Box))+1]
	if (length(ds)==0) {tkbell()} else {
		initializeDialog(title="Dataset Preview")
		colnum<-min(6,ncol(get(ds)))
		datanames<-names(get(ds))[1:colnum]
		data<-as.matrix(get(ds)[c(1:5),c(1:colnum)])
		data<-rbind(datanames,data)
		data[grep("\ ", data)]<-paste("\"", data[grep("\ ", data)], "\"", sep="")  #new to adjust for blank spaces in data
		for (i in (0:5))
			for (j in (0:(colnum-1)))
				.Tcl(paste("set tclarray(", i, ",", j, ") ", data[i+1, j+1], sep=""))	
		table1<-tk2table(top, variable="tclarray", background="white", rows=6, cols=colnum, colwidth = "15", titlerows = "1")
		tkpack(table1)
	
		tkbind(top,"<Destroy>",function()tkfocus(CommanderWindow()))
		tkgrab.set(top)
		tkfocus(top)
		}
	}
		

#Change from a character string of numbers to a vector of real digits
digits<-function(x){
	return(as.real(c(strsplit(as.character(x),""))[[1]]))
	}

#make from vector of digits to single character number
undigits<-function(x){
	return(paste(x, collapse = ""))
	}

#get the transformation of a number function including if longer than 8 digits
#control is true when the control digit is included in the number and false when its not

Perm.fn <- function(x, control=TRUE){
	y<-rev(seq(length(x)))
	if (!control) {y<-y+1}
	if (max(y)>8) {
		y<-y %% 8
		y<-ifelse(y==0, 8, y)
		}
	return(digi$Perm[cbind(y,x+1)])
	}


### Add the transformated numbers function ###
Add.fn<-function(x){	
	n<-x[1]
	for (i in 1:(length(x)-1)){
		n<-digi$Add[n+1,x[i+1]+1]
		}
	return(n)
	}

### Checking Numbers ###
Check.number<-function(x){
	return(Add.fn(Perm.fn(digits(x))))
	}

ListCheck<-function(dataset, x, z){
	data(digi)
	m<-sapply(dataset[,x], Check.number)	
	n<-sapply(dataset[,x], nchar)
	y<-rbind(dataset[which(m!="0"),], dataset[which(n!=z),])
	if (nrow(y)>0) {
		y<-unique(y)
		y<-y[order(y[,x]),]
		row.names(y)<-NULL		
		return(y)
		} else return()
	}


### Add control digits to a list of numbers
Add.control<-function(dataset, x){
	data(digi)
	lmax<-max(sapply(dataset[,x], function(y)length(digits(y))))
	z<-as.double(dataset[,x])
	l<-sapply(z, function(y)length(digits(y)))
	z[which(l<lmax)]<-sprintf(paste("%0", lmax, "d", sep=""), z[which(l<lmax)])
	z<-sapply(z, function(x){
	m<-Add.fn(Perm.fn(digits(x), control=FALSE))
	z<-undigits(c(as.character(x), digi$Inv[m+1]))
	return(z)})
	return(z)
	}


###Create a list of encoded numbers ###
Create.control<-function(x, n){
	data(digi)
	z<-sprintf(paste("%0", x-1, "d", sep=""), seq(1:n))
	z<-sapply(z, function(x){
	m<-Add.fn(Perm.fn(digits(x), control=FALSE))
	z<-undigits(c(as.character(x), digi$Inv[m+1]))
	return(z)})
	names(z)<-NULL
	return(z)
	}


# General Disabling function
disable.fn<-function(x,y,z){ 
	if (tclvalue(x)=="1") {
		tkconfigure(y, state="disabled")
		tkconfigure(z, state="disabled")
		}
	if (tclvalue(x)=="0") {
		tkconfigure(y, state="normal")
		tkconfigure(z, state="normal")
		}
	}

disable.fn2<-function(x,y) {
  if (tclvalue(x)=="1") setstate(y, state="disabled")
  if (tclvalue(x)=="0") setstate(y, state="normal")
                }

disable.fn3<-function(x,y,z) {
  if (tclvalue(x)=="0") {
		tkconfigure(y, state="disabled")
		tkconfigure(z, state="disabled")
		}
	if (tclvalue(x)=="1") {
		tkconfigure(y, state="normal")
		tkconfigure(z, state="normal")
		}
	}
                

#################
variableListBox2 <- function (parentWindow, variableList = Variables(), bg = "white", 
    selectmode = "single", export = "FALSE", initialSelection = NULL, 
    listHeight = getRcmdr("variable.list.height"), title) 
{
    if (length(variableList) == 1 && is.null(initialSelection)) 
        initialSelection <- 0
    frame <- tkframe(parentWindow)
    minmax <- getRcmdr("variable.list.width")
    listbox <- tklistbox(frame, height = listHeight, 
        selectmode = selectmode, background = bg, exportselection = export, 
        width = min(max(minmax[1], nchar(variableList)), minmax[2]))
    label <- tklabel(frame, text = title, fg = "blue")
    scrollbar <- ttkscrollbar(frame, command = function(...) tkyview(listbox, 
        ...))
    tkconfigure(listbox, yscrollcommand = function(...) tkset(scrollbar, 
        ...))
    for (var in variableList) tkinsert(listbox, "end", var)
    if (is.numeric(initialSelection)) 
        for (sel in initialSelection) tkselection.set(listbox, 
            sel)
    firstChar <- tolower(substr(variableList, 1, 1))
    len <- length(variableList)
    onLetter <- function(letter) {
        letter <- tolower(letter)
        current <- 1 + round(as.numeric(unlist(strsplit(tclvalue(tkyview(listbox)), 
            " "))[1]) * len)
        mat <- match(letter, firstChar[-(1:current)])
        if (is.na(mat)) 
            return()
        tkyview.scroll(listbox, mat, "units")
    }
    onA <- function() onLetter("a")
    onB <- function() onLetter("b")
    onC <- function() onLetter("c")
    onD <- function() onLetter("d")
    onE <- function() onLetter("e")
    onF <- function() onLetter("f")
    onG <- function() onLetter("g")
    onH <- function() onLetter("h")
    onI <- function() onLetter("i")
    onJ <- function() onLetter("j")
    onK <- function() onLetter("k")
    onL <- function() onLetter("l")
    onM <- function() onLetter("m")
    onN <- function() onLetter("n")
    onO <- function() onLetter("o")
    onP <- function() onLetter("p")
    onQ <- function() onLetter("q")
    onR <- function() onLetter("r")
    onS <- function() onLetter("s")
    onT <- function() onLetter("t")
    onU <- function() onLetter("u")
    onV <- function() onLetter("v")
    onW <- function() onLetter("w")
    onX <- function() onLetter("x")
    onY <- function() onLetter("y")
    onZ <- function() onLetter("z")
    for (letter in c(letters, LETTERS)) {
        tkbind(listbox, paste("<", letter, ">", sep = ""), get(paste("on", 
            toupper(letter), sep = "")))
    }
    onClick <- function() tkfocus(listbox)
    toggleSelection <- function() {
        active <- tclvalue(tkindex(listbox, "active"))
        selected <- tclvalue(tkcurselection(listbox))
        if (selected == active) 
            tkselection.clear(listbox, "active")
        else tkselection.set(listbox, "active")
    }
    tkbind(listbox, "<ButtonPress-1>", onClick)
    if (selectmode == "single") 
        tkbind(listbox, "<Control-ButtonPress-1>", toggleSelection)
    if (title!="")  tkgrid(label, columnspan = 2, sticky = "w")
    tkgrid(listbox, scrollbar, sticky = "nw")
    tkgrid.configure(scrollbar, sticky = "wns")
    tkgrid.configure(listbox, sticky = "ew")
    result <- list(frame = frame, listbox = listbox,  label=label, scrollbar = scrollbar, 
        selectmode = selectmode, varlist = variableList)
    class(result) <- "listbox"
    result
}


########################
info.bn<-function(frame, text1){
	tkbutton(frame, text="Help", width=10, command=function(){
	top<-tktoplevel()
	tkwm.title(top, "Information")
	tkgrid(tklabel(top, text=text1, justify="left"))
	tkgrab.set(top)
	tkfocus(top)})
	}

#################################
setstate<-function(boxie, state){
	tkconfigure(boxie[[2]], state=state)
	tkconfigure(boxie[[3]], state=state)
	}

##################################
setstate2<-function(x, y) for (i in 1:length(x)) tkconfigure(x[[i]], state=y)

################################
radioButtons2 <- defmacro(window=top, name, buttons, values=NULL, initialValue=..values[1], labels, 
	title="", title.color="blue", right.buttons=FALSE, click.command=function(){},
	expr={
		..values <- if (is.null(values)) buttons else values
		..frame <- paste(name, "Frame", sep="")
		assign(..frame, tkframe(window))
		..variable <- paste(name, "Variable", sep="")
		assign(..variable, tclVar(initialValue))
		if(title != ""){
			..title <- paste(name, "Title", sep="")
			assign(..title, labelRcmdr(eval(parse(text=..frame)), text=title, foreground=title.color))
			tkgrid(get(..title), columnspan=2, sticky="w")
		}
		for (i in 1:length(buttons)) {
			..button <- paste(buttons[i], "Button", sep="")
			..buttonlabel<-paste(buttons[i], "Label", sep="")
			assign(..buttonlabel, labelRcmdr(eval(parse(text=..frame)), text=labels[i], justify="left"))
			assign(..button,
				tkradiobutton(eval(parse(text=..frame)), variable=eval(parse(text=..variable)), value=..values[i]))
			if (right.buttons) tkgrid(eval(parse(text=..buttonlabel)), eval(parse(text=..button)), sticky="w")
			else  tkgrid(eval(parse(text=..button)), eval(parse(text=..buttonlabel)), sticky="w")
			tkconfigure(eval(parse(text=..button)), command=click.command)
		}
	}
) 

##########################
tkblank<-function(window) tkgrid(tklabel(window, text=""))

#########################
check.fn<-function(v, nbtab){
		mes<-paste("Please select a ", v, " variable", sep="")
 		tkmessageBox(message=mes)
		tk2notetab.select(nb, nbtab)		
	}
#########################
check.fn2<-function(v){
	mes<-paste("Please select a ", v, " variable", sep="")
 	tkmessageBox(message=mes)
	return()	
	}

#######################
OKCancelHelp2<-defmacro(window = top, onHelp, expr = {
    buttonsFrame <- tkframe(window, borderwidth = 5)
    OKbutton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("OK"), 
        foreground = "darkgreen", width = "12", command = onOK, 
        default = "active", borderwidth = 3)
    onCancel <- function() {
        if (GrabFocus()) tkgrab.release(window)
        tkdestroy(window)
        tkfocus(CommanderWindow())
    }
    cancelButton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Cancel"), 
        foreground = "red", width = "12", command = onCancel, 
        borderwidth = 3)
    helpButton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Help"), 
            width = "12", command = onHelp, borderwidth = 3)
    tkgrid(OKbutton, labelRcmdr(buttonsFrame, text = "  "), cancelButton, 
        labelRcmdr(buttonsFrame, text = "            "), helpButton, sticky = "w")
})
