UPmaxentropy1=function(pik) 
{
if(is.data.frame(pik)) 
    if(ncol(pik)>1) stop("pik is not a vector") else pik=unlist(pik)
else if(is.matrix(pik))
       if(ncol(pik)>1) stop("pik is not a vector") else pik=pik[1]
else  if(is.list(pik))
   if(length(pik)>1) stop("pik is not a vector") else pik=unlist(pik)
    n = sum(pik)
    n=as_int1(pik)
    if (n >= 2) {
        pik2 = pik[pik != 1]
        n = sum(pik2)
        n=as_int1(pik2)
        piktilde = UPMEpiktildefrompik1(pik2)
        w = piktilde/(1 - piktilde)
        q = UPMEqfromw(w, n)
        s2 = UPMEsfromq(q)
        s = rep(0, times = length(pik))
        s[pik == 1] = 1
        s[pik != 1][s2 == 1] = 1
    }
    if (n == 0) 
        s = rep(0, times = length(pik))
    if (n == 1) 
        s = as.vector(rmultinom(1, 1, pik))
    s
}


UPMEpiktildefrompik1=function (pik, eps = 1e-06) 
{
    n = sum(pik)
    n=as_int1(pik)
    pikt = pik
    arr = 1
    while (arr > eps) {
        w = (pikt)/(1 - pikt)
        q = UPMEqfromw(w, n)
        pikt1 = pikt + pik - UPMEpikfromq(q)
        arr = sum(abs(pikt - pikt1))
        pikt = pikt1
    }
    pikt
}


as_int1=function(pik) 
{x=sum(pik)
 if (!is.integer(x)) {
        xo = round(x)
        if (any(x > .Machine$integer.max)) 
            stop("the input has entries too large to be integer")
        if (!identical(TRUE, (ax <- all.equal(xo, x,tolerance=.Machine$double.eps)))) 
            warning("the sum of pik is not integer")
        else x=xo
    }
    x
}


