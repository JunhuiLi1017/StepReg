proc logistic data=dat2 outest=betas covout;
model remiss(event='1')=cell smear infil li blast temp/ selection=stepwise sle=0.3 sls=0.35 details lackfit;
output out=pred p=phat lower=lcl upper=ucl predprob=(individual crossvalidate);
ods output Association=Association;
run;