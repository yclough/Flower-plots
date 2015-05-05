rotate<-function(xy,angle){
	m<-matrix(c(cos(-angle*pi/180),-sin(-angle*pi/180),sin(-angle*pi/180),cos(-angle*pi/180)),nrow=2,ncol=2,byrow=T)
	xy2<-m%*%xy
	return(as.numeric(xy2))
}

flallg<-function(
	values.orig = c(1,0.5,0.3,0.2,0.1),
	vlabels = c("Naturalness","Biodiversity","Soil Fertility","Microclimatic\nstability","next one"),
	angle.start = 0,
	lab.offset = 0.7,
	crad = 1,
	plot.xlim = c(-1.5,1.5),
	plot.ylim = c(-1.5,1.5),
	x.cent = 0,
	y.cent = 0,
	col = "blue"
	){

	require(plotrix)

	plot(NA,axes=F,xlab="",ylab="",xlim=plot.xlim,ylim=plot.ylim,asp=1)
		# note that asp=1 is critical to keep draw.circle() from overriding the coordinate system
	d=draw.circle(x.cent,y.cent,radius=crad,nv=100,border=NULL,col=NA,lty=1,lwd=2)
	if(length(col)!=length(values.orig)){petalcol=rep(col,length.out=length(values.orig))}
	if(length(col)==length(values.orig)){petalcol=col}
	values=numeric(length(values.orig))
	nrays<-length(values)
	angle.diff=360/nrays

	for(i in 1:(nrays)){
		
		# coordinates for petal components (incl. grey line extremities, circle centre,
		# polygon corners, and label positions) are first built and then rotated.
		
		#line coordinates
		ytemp0<-y.cent+crad
		xtemp0<-0
		
		#circle centre coordinates
		r=values.orig[i]/5
		values[i]=values.orig[i]-r
		angle<-(angle.start+(i-1)*angle.diff)
		xtemp1<-0
		ytemp1<-y.cent+values[i]

		#triangle coordinates
		#v^2=r^2+adj^2
		adj<-sqrt((values[i]^2)-r^2)
		#sin(ang)=r/values[i]
		ang<-asin(r/values[i])*180/pi
		ang2<-180-90-ang
		ang3<-180-90-ang2
		xtemp2<-xtemp1-cos(ang3*pi/180)*r
		ytemp2<-ytemp1-sin(ang3*pi/180)*r
		xtemp3<-xtemp1+cos(ang3*pi/180)*r
		ytemp3<-ytemp1-sin(ang3*pi/180)*r

		#label coords
		ytemp4<-y.cent+1+lab.offset
		xtemp4<-0

		#rotate all coords
		xy0<-rotate(c(xtemp0,ytemp0),angle)
		xy1<-rotate(c(xtemp1,ytemp1),angle)
		xy2<-rotate(c(xtemp2,ytemp2),angle)
		xy3<-rotate(c(xtemp3,ytemp3),angle)
		xy4<-rotate(c(xtemp4,ytemp4),angle)

		#draw the components
		lines(x=c(0,xy0[1]),y=c(0,xy0[2]),col="grey")
		draw.circle(xy1[1],xy1[2],radius=r,nv=100,border=NA,col=petalcol[i],lty=1,lwd=1)
		polygon(x=c(0,xy2[1],xy3[1]),y=c(0,xy2[2],xy3[2]),border=petalcol[i],col=petalcol[i])
		text(x=xy4[1],y=xy4[2],label=vlabels[i],xpd=T)
	}
}

# run it using defaults
flallg()
