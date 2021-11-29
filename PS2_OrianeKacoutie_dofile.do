
* Q1 If you did not use the data for PS1, you can use jtrain1 data. 
************************************************
************************************************
**** Generate Data **************************
************************************************
************************************************


***Construct  a  placebo  treatment  Ti  by  choosing  some  rule  for  so  that  Ti  =  1  when  Xi  > x*  for some x*
*Estimate the model by regression discontinuity in several different ways (i.e. kernel regression, local linear regression, using polynomials etc.)
use "C:\Users\Oriane\Downloads\jtrain1.dta"

gen T=0
replace T=1 if lemploy >3.5
drop if lemploy <0
drop if lemploy <1
gen x=lemploy
gen y =lsales
graph twoway (lfit lsales lemploy)(scatter lsales lemploy)
drop if missing(x)
drop if missing(y)
sort x
reg lsales T employ
predict yhat
gen x0=yhat if T==0
gen x1=yhat if T==1
scatter y x0 x1 x, c(. 1 1)s(x i i)


*Now different types of non-parametric method, Kernel estimators to execute Sharp RD*
** Try 3 different Bandwidths **

gen h1=0.25
gen h2=0.5
gen h3=0.75

**Uniform Kernel Denisity Function**

**For bandwidth of 0.25**
gen kern11u=0
gen kern11l=0
replace kern11u=1 if ((x>3.5)&(x<3.5+h1))
replace kern11l=1 if ((x<3.5)&(x>3.5-h1))
reg y [pweight=kern11u]
predict b011u
reg y [pweight=kern11l]
predict b011l
gen est11=b011u-b011l

**For bandwidth of 0.5
gen kern12u=0
gen kern12l=0
replace kern12u=1 if ((x>3.5)&(x<3.5+h2))
replace kern12l=1 if ((x<3.5)&(x>3.5-h2))
reg y [pweight=kern12u]
predict b012u
reg y [pweight=kern12l]
predict b012l
gen est12=b012u-b012l

**For bandwidth of 0.75
gen kern13u=0
gen kern13l=0
replace kern13u=1 if ((x>3.5)&(x<3.5+h3))
replace kern13l=1 if ((x<3.5)&(x>3.5-h3))
reg y [pweight=kern13u]
predict b013u
reg y [pweight=kern13l]
predict b013l
gen est13=b013u-b013l

**Epanechikov Kernel Regression Function**

**For bandwidth of 0.25
gen kern21u=0
gen kern21l=0
gen dx1=(x-3.5)/h1
replace kern21u=(1-dx1*dx1) if ((x>3.5)&(x<3.5+h1))
replace kern21l=(1-dx1*dx1) if ((x<3.5)&(x>3.5-h1))
reg y [pweight=kern21u]
predict b021u
reg y [pweight=kern21l]
predict b021l
gen est21=b021u-b021l

** For bandwidth of 0.5
gen kern22u=0
gen kern22l=0
gen dx2=(x-3.5)/h2
replace kern22u=(1-dx2*dx2) if ((x>3.5)&(x<3.5+h2))
replace kern22l=(1-dx2*dx2) if ((x<3.5)&(x>3.5-h2))
reg y [pweight=kern22u]
predict b022u
reg y [pweight=kern22l]
predict b022l
gen est22=b022u-b022l

** For bandwidth of 0.75
gen kern23u=0
gen kern23l=0
gen dx3=(x-3.5)/h3
replace kern23u=(1-dx3*dx3) if ((x>3.5)&(x<3.5+h3))
replace kern23l=(1-dx3*dx3) if ((x<3.5)&(x>3.5-h3))
reg y [pweight=kern23u]
predict b023u
reg y [pweight=kern23l]
predict b023l
gen est23=b023u-b023l
sum est11 est12 est13 est21 est22 est23

**Local Linear Regression with different Kernels and bandwidths**
reg y x [pweight=kern11u]
predict b031u
reg y x [pweight=kern11l]
predict b031l
gen est31=b031u-b031l if _n==359
reg y x [pweight=kern12u]
predict b032u
reg y x [pweight=kern12l]
predict b032l
gen est32=b032u-b032l if _n==359
reg y x [pweight=kern13u]
predict b033u
reg y x [pweight=kern13l]
predict b033l
gen est33=b033u-b033l if _n==359
reg y x [pweight=kern21u]
predict b041u
reg y x [pweight=kern21l]
predict b041l
gen est41=b041u-b041l if _n==359
reg y x [pweight=kern22u]
predict b042u
reg y x [pweight=kern22l]
predict b042l
gen est42=b042u-b042l if _n==359
reg y x [pweight=kern23u]
predict b043u
reg y x [pweight=kern23l]
predict b043l
gen est43=b043u-b043l if _n==359
sum est31 est32 est33 est41 est42 est43

** Parametic Approach: Polynomial Regression Approach**
gen x2=x*x
gen x3=x*x2
gen x4=x*x3
gen x5=x*x4
gen x6=x*x5
gen x7=x*x6
gen x8=x*x7
gen gt5=0
replace gt5=1 if x>3.5
gen xgt=(x-3.5)*gt5
gen x2gt=xgt*xgt
gen x3gt=x2gt*xgt
gen x4gt=x3gt*xgt
gen x5gt=x4gt*xgt
gen x6gt=x5gt*xgt
gen x7gt=x6gt*xgt
gen x8gt=x7gt*xgt

**2 different model specifications
* polynomial regression plus treatment dummies

reg y T x
reg y T x x2 x3
reg y T x x2 x3 x4
reg y T x x2 x3 x4 x5 x6
reg y T x x2 x3 x4 x5 x6 x7 x8

* polynomial regression (different fn forms on the right hand side and the left hand>side)
reg y T x xgt
reg y T x x2 x3 x4 xgt x2gt x3gt x4gt
reg y T x x2 x3 x4 x5 x6 xgt x2gt x3gt x4gt x5gt x6gt
reg y T x x2 x3 x4 x5 x6 x7 x8 xgt x2gt x3gt x4gt x5gt x6gt x7gt x8gt


*********************************************************
*********************Q2**********************************
*********************************************************
**Construct a dummy data set.Estimate the treatment effect (α) by RDD in several ways and compare the results.

clear
**Generate Data

pause on
set obs 1001
gen x=uniform()
gen T=0
replace T=1 if x>0.5
gen g=T+3*log(x+1)+sin(x*12)/3
gen y=g+invnorm(uniform())/5
sort x
replace x=0.5 if _n==1001
replace g=. if _n==1001
replace y=. if _n==1001
gen g0=g if T==0
gen g1=g if T==1
scatter y g0 g1 x, c(. l l) s(x i i)


******* Try 3 different Bandwidths *************

gen h1=0.1
gen h2=0.05
gen h3=0.01

************************************************
******* Kernel Regression with Uniform *********

gen kern11u=0
gen kern11l=0
replace kern11u=1 if ((x>0.5)&(x<0.5+h1))
replace kern11l=1 if ((x<0.5)&(x>0.5-h1))
reg y [pweight=kern11u]
predict b011u
reg y [pweight=kern11l]
predict b011l
gen est11=b011u-b011l
gen kern12u=0
gen kern12l=0
replace kern12u=1 if ((x>0.5)&(x<0.5+h2))
replace kern12l=1 if ((x<0.5)&(x>0.5-h2))
reg y [pweight=kern12u]
predict b012u
reg y [pweight=kern12l]
predict b012l
gen est12=b012u-b012l
gen kern13u=0
gen kern13l=0
replace kern13u=1 if ((x>0.5)&(x<0.5+h3))
replace kern13l=1 if ((x<0.5)&(x>0.5-h3))
reg y [pweight=kern13u]
predict b013u
reg y [pweight=kern13l]
predict b013l
gen est13=b013u-b013l

************************************************
******* Kernel Regression with Epanechnikov ****
gen kern21u=0
gen kern21l=0
gen dx1=(x-0.5)/h1
replace kern21u=(1-dx1*dx1) if ((x>0.5)&(x<0.5+h1))
replace kern21l=(1-dx1*dx1) if ((x<0.5)&(x>0.5-h1))
reg y [pweight=kern21u]
predict b021u
reg y [pweight=kern21l]
predict b021l
gen est21=b021u-b021l
gen kern22u=0
gen kern22l=0
gen dx2=(x-0.5)/h2
replace kern22u=(1-dx2*dx2) if ((x>0.5)&(x<0.5+h2))
replace kern22l=(1-dx2*dx2) if ((x<0.5)&(x>0.5-h2))
reg y [pweight=kern22u]
predict b022u
reg y [pweight=kern22l]
predict b022l
gen est22=b022u-b022l
gen kern23u=0
gen kern23l=0
gen dx3=(x-0.5)/h3
replace kern23u=(1-dx3*dx3) if ((x>0.5)&(x<0.5+h3))
replace kern23l=(1-dx3*dx3) if ((x<0.5)&(x>0.5-h3))
reg y [pweight=kern23u]
predict b023u
reg y [pweight=kern23l]
predict b023l
gen est23=b023u-b023l
sum est11 est12 est13 est21 est22 est23


************************************************
******* Local Linear Regression with ***********
******* different Kernels and bandwidths *******
reg y x [pweight=kern11u]
predict b031u
reg y x [pweight=kern11l]
predict b031l
gen est31=b031u-b031l if _n==1001
reg y x [pweight=kern12u]
predict b032u
reg y x [pweight=kern12l]
predict b032l
gen est32=b032u-b032l if _n==1001
reg y x [pweight=kern13u]
predict b033u
reg y x [pweight=kern13l]
predict b033l
gen est33=b033u-b033l if _n==1001
reg y x [pweight=kern21u]
predict b041u
reg y x [pweight=kern21l]
predict b041l
gen est41=b041u-b041l if _n==1001
reg y x [pweight=kern22u]
predict b042u
reg y x [pweight=kern22l]
predict b042l
gen est42=b042u-b042l if _n==1001
reg y x [pweight=kern23u]
predict b043u
reg y x [pweight=kern23l]
predict b043l
gen est43=b043u-b043l if _n==1001
sum est31 est32 est33 est41 est42 est43

************************************************
******* Polynomial Regression Approach *********
gen x2=x*x
gen x3=x*x2
gen x4=x*x3
gen x5=x*x4
gen x6=x*x5
gen x7=x*x6
gen x8=x*x7
gen gt5=0
replace gt5=1 if x>0.5
gen xgt=(x-0.5)*gt5
gen x2gt=xgt*xgt
gen x3gt=x2gt*xgt
gen x4gt=x3gt*xgt
gen x5gt=x4gt*xgt
gen x6gt=x5gt*xgt
gen x7gt=x6gt*xgt
gen x8gt=x7gt*xgt

***** 2 different model specifications
***** polynomial regression plus treatment dummies
reg y T x
reg y T x x2 x3
reg y T x x2 x3 x4
reg y T x x2 x3 x4 x5 x6
reg y T x x2 x3 x4 x5 x6 x7 x8

* polynomial regression (different fn forms on the right hand side and the left hand> side)
reg y T x xgt
reg y T x x2 x3 x4 xgt x2gt x3gt x4gt
reg y T x x2 x3 x4 x5 x6 xgt x2gt x3gt x4gt x5gt x6gt
reg y T x x2 x3 x4 x5 x6 x7 x8 xgt x2gt x3gt x4gt x5gt x6gt x7gt x8gt

*********************************************************
*********************Q3**********************************
**Modify  the  simulated  model  and  the  estimation  method  for  a  fuzzy  design rather  than a sharp design

clear

net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
set obs 1001
gen x=uniform()
gen T=0
replace T=1 if (runiform()<0.5)+(runiform()<0.65)*(x>=0.5)
gen g=T+3*log(x+1)+sin(x*12)/3
gen y=g+invnorm(uniform())/5
rdrobust y x, c(0.5) fuzzy(T) kernel(uniform)
rdplot y x, c(0.5) fuzzy(T) kernel(uniform)
rdrobust y x, c(0.5) fuzzy(T) kernel(epanechnikov)
rdplot y x, c(0.5) fuzzy(T) kernel(epanechnikov)
rdrobust y x, c(0.5) fuzzy(T)
rdplot y x, c(0.5) fuzzy(T)

***** Alteratively, Fuzzy RD is IV***
clear
set obs 2000
gen x=uniform()
gen T=0
replace T=1 if x>0.5
gen D=rbinomial(1,0.65)
replace D=0 if x<0.5
gen g=D+3*log(x+1)+cos(x*12)/3
gen y=g+invnorm(uniform())/5
sort x
gen g0=g if x<0.5
scatter y g0 x, c(. l l) s(x i i)
gen h1=0.1
gen h2=0.05
gen h3=0.01
drop if ((x<0.5-h1)|(x>0.5+h1))
ivregress 2sls y x (D = T)
drop if ((x<0.5-h2)|(x>0.5+h2))
ivregress 2sls y x (D = T)
drop if ((x<0.5-h3)|(x>0.5+h3))
ivregress 2sls y x (D = T)

clear
set obs 1001
gen x=uniform()
gen T=0
replace T=1 if x>0.5
gen D=rbinomial(1,0.65)
replace D=0 if x<0.5
gen g=D+3*log(x+1)+cos(x*12)/3
gen y=g+invnorm(uniform())/5
sort x
gen g0=g if x<0.5
scatter y g0 x, c(. l l) s(x i i)
gen h1=0.1
gen h2=0.05
gen h3=0.01
drop if ((x<0.5-h1)|(x>0.5+h1))
ivregress 2sls y x (D = T)
drop if ((x<0.5-h2)|(x>0.5+h2))
ivregress 2sls y x (D = T)
drop if ((x<0.5-h3)|(x>0.5+h3))
ivregress 2sls y x (D = T)

