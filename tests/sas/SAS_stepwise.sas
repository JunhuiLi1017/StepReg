/*stepwise linear regression*/
proc glmselect data=work.dat;
model mpg=cyl disp hp drat wt qsec vs am gear carb/selection=forward select=SL sle=0.15 noint;
run;

proc glmselect data=work.dat;
model mpg=vs qsec cyl disp hp drat wt am gear carb/selection=stepwise include=2 select=SL sle=0.15 sls=0.15;
run;

/*stepwise logistic regression*/
proc logistic data=work.dat2;
model remiss=cell smear infil li blast temp/selection=score include=1 noint;
run;

/*stepwise cox regression*/
proc phreg data=work.dat1;
model time*status1(0)=age inst sex ph_ecog ph_karno pat_karno meal_cal wt_loss/selection=forward sle=0.15;
run;