
#Siland is a R package to estimate spatial infleunce of landscape variables.
#Copyright (C) 2017  Martin O. <olivier.martin@inra.fr>
#		     Carpentier F.

# Siland is free software; you can redistribute it and/or
#modify it under the terms of the GU General Public License
#as published by the Free Software Foundation; either version 2
#of the License, or (at your option) any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with this program; if not, write to the Free Software
#Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


print.siland<-function(x,...){
  cat("Model: ")
  print(as.formula(x$formula),showEnv=F)
  cat("\n")

  cat("Landscape variables: ")
  cat(colnames(x$landcontri))
  cat("\n \n")

  cat("Coefficients:\n")
  print(round(x$coefficients,3))
  if(x$modelType=="LMM" ||x$modelType=="GLMM")
    {
    cat("\nRandom effects:\n")
    if(x$modelType=="LMM"||x$modelType=="GLMM")
      print(x$rand.StdDev)
    }

  cat("\nstandard error: ")
  cat(x$sd.error)
  cat("\n")
  cat(paste("AIC: ",round(x$AIC,2), "\t", "AIC (no landscape): ",round(x$AIC0,2),sep=""))
  cat("\n")
  cat("(No landscape effect) p-value: ")
  if(x$pval0 < 1e-16) cat("<1e-16") else cat(x$pval0)
  cat("\n")


  if(sum(x$paramSIF<(3*x$wd)))
  {
    warning("\nIt is recommended that wd is three times smaller than the estimated SIF mean distance. A new estimation with smaller wd should be more appropriate (see argument wd in Fsiland).")
  }


  #cat("\t(No landscape effect) p-value: ")
  #if(x$pval0==0) cat("<1e-16") else cat(x$pval0)
  #cat("\n")
}




summary.siland<-function(object,...)
{
  #data farame summary

  x<-object
  summaryx=summary(x$result)
  summaryx$call=x$formula

  cat("SIF parameters:\n")
  print(round(x$paramSIF,4))
  cat("\n")
  cat("-- Tests are given conditionally to the best SIF parameters --" )
  cat("\n")
  print(summaryx)


  if(sum(x$paramSIF<(3*x$wd)))
  {
    warning("\nIt is recommended that wd is three times smaller than the estimated SIF mean distance. A new estimation with smaller wd should be more appropriate (see argument wd in Fsiland).")
  }


}












fdispE=function(x,dmoy){
  #compute density for exponential sif
  alpha=dmoy/2
  (1/(2*pi*alpha^2))*exp(-x/alpha)
}

fdispG=function(x,dmoy){
  #compute density for gaussian sif
  alpha=dmoy/gamma(3/2)
  (1/(alpha^2*pi))*exp(-(x/alpha)^2)
}

fdispU=function(x,dmoy){
  #compute density for uniform sif
  alpha=3*dmoy/2
  w=x
  s=(x<=alpha)
  w[s]=1/(pi*alpha^2)
  w[!s]=0
  return(w)

}

fdispRE=function(r,dmoy){
  #compute radius density for exponential fis function
  alpha=dmoy/2
  (r/(alpha^2))*exp(-r/alpha)
}

fdispRG=function(r,dmoy){
  #compute radius density for gaussian fis function
  alpha=dmoy/gamma(3/2)
  (2/alpha^2)*r*exp(-r^2/alpha^2)
}

fdispRU=function(r,dmoy){
  #compute radius density for uniform fis function
  alpha=dmoy*3/2
  w=rep(0,length(r))
  s=r<=alpha
  w[s]=1/(pi*alpha^2)
  return(2*pi*r*w)
}


quantileE=function(q=0.9,dm,l=3000)
{
  #Find quantile for radius distribution for exponential fis
  vv=seq(0,4*dm,length=l)
  pas=vv[2]-vv[1]
  cc=0
  for(i in 1:length(vv))
  {
    tmp=fdispRE(vv[i],dm)*pas
    cc=cc+tmp
    if(cc>q)
    {
      resq=vv[i]
      return(resq)
    }
  }
}

quantileG=function(q=0.9,dm,l=3000)
{
  #Find quantile for radius distribution for gaussian fis
  vv=seq(0,4*dm,length=l)
  pas=vv[2]-vv[1]
  cc=0
  for(i in 1:length(vv))
  {
    tmp=fdispRG(vv[i],dm)*pas
    cc=cc+tmp
    if(cc>q)
    {
      resq=vv[i]
      return(resq)
    }
  }
}

quantileU=function(dm,q=0.9,l=3000)
{
  #Find quantile for radius distribution for uniform fis
  vv=seq(0,5*dm,length=l)
  pas=vv[2]-vv[1]
  cc=0
  for(i in 1:length(vv))
  {
    tmp=fdispRU(vv[i],dm)*pas
    cc=cc+tmp
    #print(cc)
    if(cc>q)
    {
      resq=vv[i]
      return(resq)
    }
  }
}



  parens <- function(x) paste0("(",x,")")


leg.col <- function(colr, niv){
#add bar color scale for plotcontri
  n <- length(colr)
  bx <- par("usr")
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  #box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
  #            bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 30)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n

  xx <- rep(box.cx, each = 2)

  par(xpd = TRUE)
  for(i in 1:n){

    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = colr[i], border = colr[i])

  }
  par(new = TRUE)
  plot(0, 0, type = "n",
       ylim = c(min(niv), max(niv)),
       yaxt = "n", ylab = "",
       xaxt= "n", xlab = "",
       frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line = 0.3)
  #axis(side = 4, las = 2, tick = FALSE, line = .25)
}


