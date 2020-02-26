// Create a file to run the TMB version of sprat
#include <TMB.hpp>
#include <iostream>


template <class Type>
vector<Type> cumsum(vector<Type> x) {
  int n = x.size();
  vector<Type> ans(n);
  ans[0] = x[0];
  for (int i = 1; i < n; i++) ans[i] = x[i] + ans[i-1];
  return ans;
}


template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data input
  DATA_ARRAY(wage_ssb); // Weight in the beginning of the year
  DATA_ARRAY(wage_catch); // Weight in catch
  DATA_ARRAY(wage_survey); // Weight in survey
  DATA_ARRAY(wage_mid); // Weight in the middle of the year
// //
// // // Age
  DATA_INTEGER(nage); // Plus group
  DATA_INTEGER(sum_zero); // should rec dev's sum so zero?
  DATA_VECTOR(age); // ages
  DATA_INTEGER(tEnd);
  DATA_INTEGER(year_sel);
  DATA_INTEGER(selYear);
  DATA_SCALAR(logQ);
  DATA_VECTOR(b); // bias adjustment factor
  DATA_VECTOR(years);
  DATA_VECTOR(flag_sel);
// // Selectivity
  DATA_INTEGER(Smin);
  DATA_INTEGER(Smin_survey);
  DATA_INTEGER(Smax);
  DATA_INTEGER(Smax_survey);
// // // Survey
  DATA_VECTOR(survey); // Acoustic survey
  DATA_VECTOR(survey_x); // Flag if survey occured
  DATA_VECTOR(survey_err);
  DATA_VECTOR(ss_survey); // Age comp sample size
  DATA_VECTOR(flag_survey); // Were ages sampled this year
  DATA_ARRAY(age_survey); // Age compositions
  DATA_INTEGER(age_maxage); // Last age included in age comps
  DATA_SCALAR(smul); // Multiplier for survey selectivity
 // Catches
  DATA_VECTOR(Catchobs); // Total catch
  DATA_VECTOR(ss_catch); // age comp sample size
  DATA_VECTOR(flag_catch); // Years age was sampled
  DATA_ARRAY(age_catch); // Age comps

  DATA_SCALAR(logSDcatch); // Error on catch
  DATA_SCALAR(logSDR); // Can it be estimated as a fixed effect?
  DATA_SCALAR(sigma_psel); // selectivity SD

//
//   // Mortality
  DATA_VECTOR(Msel); // How mortality scales with age
  DATA_VECTOR(Matsel); // Maturity ogive
  // Priors
  DATA_SCALAR(Bprior);
  DATA_SCALAR(Aprior)
//   // Time parameters
  // Parameter integers
  PARAMETER(logRinit); // Recruitment at
  PARAMETER(logh); // Steepness
  PARAMETER(logMinit); // Natural mortality
  PARAMETER(logSDsurv); // Survey uncertainty
  //PARAMETER(logSDR);

  PARAMETER(logphi_catch);
  //PARAMETER(logphi_survey);
  DATA_SCALAR(logphi_survey);
  PARAMETER_VECTOR(psel_fish);
  PARAMETER_VECTOR(psel_surv);
  PARAMETER_VECTOR(initN);
  PARAMETER_VECTOR(Rin); // Time varying stuff
  PARAMETER_ARRAY(PSEL); // Time varying selectivity
  PARAMETER_VECTOR(F0);

  // Transform out of log space
  Type SDsurv = exp(logSDsurv);
  Type SDcatch = exp(logSDcatch);
  Type SDR = exp(logSDR);
  Type Rinit = exp(logRinit);
  Type h = exp(logh);
  Type Minit = exp(logMinit);
  Type q = exp(logQ);
  Type phi_survey = exp(logphi_survey);
  Type phi_catch = exp(logphi_catch);

//
// //  Minor calculations
  vector<Type> M = Minit*Msel; // Natural mortality
  vector<Type> logF(tEnd);
  vector<Type> logR(tEnd);
//
  // Vectors for saving stuff
  vector<Type>R(tEnd);
  array<Type> CatchAge(nage,tEnd);
  array<Type> CatchNAge(nage,tEnd);
//


for(int j=0;j<(tEnd-1);j++){
      logR(j)=Rin(j);
}
logR(tEnd-1) = 0;

// selectivity
// survey
vector<Type>surveyselc(nage);
Type pmax = sum(psel_surv);
Type ptmp = 0;

for(int j=0;j<nage;j++){ // Fix the survey selectivity
  if (age(j) < Smin_survey){
    surveyselc(j) = 0;
  }
   if (age(j) == Smin_survey){
    ptmp = 0;
    surveyselc(j) = exp(ptmp-pmax);
  }
   if ((age(j) > Smin_survey) & (age(j) <= Smax_survey)){
     ptmp = psel_surv(j-Smin_survey-1)+ptmp;
     surveyselc(j) = exp(ptmp-pmax);
   }
   if(age(j) > (Smax_survey)){
    surveyselc(j) = surveyselc(Smax_survey);
  }
}
// Fishing mortality
vector<Type> catchselec(nage);
Type pmax_catch = max(cumsum(psel_fish));
Type ptmp_catch = 0;
//
for(int time=0;time<tEnd;time++){ // Start time loop
    for (int j=0;j<nage;j++){
      if (age(j) < Smin){
          catchselec(j) = 0;
      }
        if (age(j) == Smin){
          ptmp_catch = 0;
          catchselec(j) = exp(ptmp_catch-pmax_catch);
      }
        if ((age(j) > Smin) & (age(j) <= Smax)){
          ptmp_catch = psel_fish(j-Smin-1)+ptmp_catch;
          catchselec(j) = exp(ptmp_catch-pmax_catch);
      }
        if(age(j) > (Smax_survey)){
          catchselec(j) = catchselec(Smax);
      }
    }
  }


vector<Type> Nzero(nage); // Numbers with no fishing
//vector<Type>Meq = cumsum(M);
//
Nzero(0) = Rinit;
for(int i=1;i<(nage-1);i++){
    Nzero(i) = Rinit * exp(-(M(i)*age(i)));
  }
//
// Nzero(nage-1) = Rinit*exp(-(M(nage-2)*age(nage-2)))/(Type(1.0)-exp(-M(nage-1)));//*exp(initN(nage-2)); // Plus group

Nzero(nage-1) = (Rinit*exp(-(M(nage-2)*age(nage-1))))/(Type(1.0)-exp(-M(nage-1)));

array<Type> SSBage(nage);
array<Type> Catchinit(nage);
array<Type>selectivity_save(nage,tEnd);
Type SSBzero = 0;
vector<Type> Zzero = M;

for(int i=0;i<nage;i++){ // Loop over ages
    SSBzero += Matsel(i)*Nzero(i);
  }
// Run the initial distribution
 REPORT(SSBzero);

// Type SSBinit = 0;
//
// // for(int i=0;i<(nage);i++){ // Loop over other ages
// //     Ninit(i,0) = Nzero(i);
// //   }
// Ninit(0) = Rinit;
// for(int i=1;i<(nage-1);i++){
//     Ninit(i) = Rinit * exp(-(M(i)*age(i)))*exp(-0.5*0*SDR*SDR+initN(i-1));
//   }
// //
// Ninit(nage-1) = Rinit*exp(-(M(nage-1)*age(nage-1)))/(Type(1.0)-exp(-M(nage-1)))*exp(-0.5*0*SDR*SDR+initN(nage-2));
//
// for(int i=0;i<(nage);i++){ // Loop over other ages
//   SSBinit += Ninit(i)*Matsel(i)*0.5;
// }

// Plus group
vector<Type>Catch(tEnd);
vector<Type>CatchN(tEnd);
array<Type> N_beg(nage,tEnd+1);
array<Type> N_mid(nage,tEnd+1);
N_beg.setZero();
N_mid.setZero();
// }
// // Run the model over time
array<Type> SSB(tEnd);
array<Type>Surveyobs(tEnd); // Survey observed Surveyobs
array<Type>Surveyobs_tot(tEnd); // Total Surveyobs over age 2
array<Type>age_survey_est(age_maxage,tEnd);
array<Type>age_catch_est(age_maxage,tEnd);
array<Type>Zsave(nage,tEnd);
//
age_survey_est.setZero();
age_catch_est.setZero();
Catch.setZero();
CatchN.setZero();
vector<Type> Myear = M*Msel; // Natural mortality (if we want to change it later)
//
vector<Type> Fyear(tEnd);
vector<Type> Freal(nage);
vector<Type> Z(nage);
vector<Type>pmax_catch_save(tEnd);
vector<Type>psel_fish_zero = psel_fish;
vector<Type>Catchsave(tEnd);
//vector<Type>Fpope(tEnd);

// vector<Type>Bmid(nage); // Biomass at the middle of the year
// // vector<Type>test(3);
// // test = cumsum(test);
// REPORT(test)
//array<Type> PSEL_save(5,)

for(int time=0;time<(tEnd);time++){ // Start time loop

    Type Ntot_survey = 0;
    pmax_catch_save(time) = pmax_catch;
    // Take care of selectivity
    REPORT(flag_sel)
    REPORT(PSEL.cols())

    if (flag_sel(time) == 1){

           for(int i=0;i<psel_fish.size();i++){
           psel_fish(i) = psel_fish_zero(i)+PSEL(i,time-selYear+1)*sigma_psel; // 27 is the number of years selectivity is calculated PSEL.cols()-1/ time-selYear-
           }

           pmax_catch = max(cumsum((psel_fish)));
           pmax_catch_save(time) = pmax_catch;

           for(int j=0;j<(nage);j++){ // Fix the Catch selectivity
               if (age(j) == Smin){
                 ptmp_catch = 0;
                 catchselec(j) = exp(ptmp_catch-pmax_catch);
             }
               if ((age(j) > Smin) & (age(j) <= Smax)){
                 ptmp_catch = psel_fish(j-Smin-1)+ptmp_catch;
                 catchselec(j) = exp(ptmp_catch-pmax_catch);
             }
               if(age(j) > (Smax_survey)){
                 catchselec(j) = catchselec(Smax);
             }
           }
         }


       Catch(time) = 0;

       Fyear(time) = F0(time);



    if (time == 0){
      for(int i=1;i<(nage-1);i++){
        N_beg(i,time) = Rinit * exp(-0.5*0*SDR*SDR+initN(i-1))*exp(-Myear(i)*age(i));
      }
        N_beg(nage-1, time) = Rinit * exp(-0.5*0*SDR*SDR+initN(nage-2)) * exp(-Myear(nage-1) * age(nage-1)) / (1 - exp(-Myear(nage-1)));

    }

    for(int i=0;i<nage;i++){ // Loop over other ages
         SSB(time) += N_beg(i,time)*wage_ssb(i,time); // hat
         //SSB(time) += N_beg(i,time)*Matsel(i); // hat

      }

    for(int i=0;i<(nage);i++){ // Loop over other ages
          Freal(i) = Fyear(time)*catchselec(i);
          Z(i) = Freal(i)+Myear(i);
          selectivity_save(i,time) = catchselec(i);
          Zsave(i,time) = Z(i);


     }

    R(time) = (4*h*Rinit*SSB(time)/(SSBzero*(1-h)+ SSB(time)*(5*h-1)))*exp(-0.5*b(time)*SDR*SDR+logR(time));
    N_beg(0,time) = R(time); // First one is recruits
    //

    //Type smul = Type(0.58);

    for(int i=0;i<(nage-1);i++){ // Loop over other ages
    N_mid(i,time) = N_beg(i,time)*exp(-Z(i)*smul);
    N_beg(i+1,time+1) = N_beg(i,time)*exp(-Z(i));
    }
    // // Plus group
    N_mid(nage-1, time) = N_beg(nage-2,time)*exp(-Z(nage-2)*0.5)+N_beg(nage-1,time)*exp(-Z(nage-1)*smul);
    N_beg(nage-1, time+1) = N_beg(nage-2,time)*exp(-Z(nage-2))+N_beg(nage-1,time)*exp(-Z(nage-1));

    Catch(time) = 0;

    for(int i=0;i<nage;i++){ // Loop over other ages
        CatchAge(i,time)= (Freal(i)/(Z(i)))*(1-exp(-Z(i)))*N_beg(i,time)*wage_catch(i,time);// Calculate the catch in kg
        CatchNAge(i,time)= (Freal(i)/(Z(i)))*(1-exp(-Z(i)))*N_beg(i,time);// Calculate the catch in kg
        Catch(time) += CatchAge(i,time);
        CatchN(time) += CatchNAge(i,time);

        Surveyobs(time) += surveyselc(i)*wage_survey(i,time)*N_mid(i,time)*q;
        Ntot_survey += surveyselc(i)*N_mid(i,time); // To use with age comps
      }


      if(flag_survey(time) == 1){ // Flag if  there was a measurement that year

        for(int i=0;i<(nage-1);i++){ // Loop over other ages
          if(i < age_maxage){
          age_survey_est(i,time) = (surveyselc(i+1)*N_mid(i+1,time))/Ntot_survey;
          }else{
          age_survey_est(age_maxage-1,time) += (surveyselc(i+1)*N_mid(i+1,time))/Ntot_survey;
          }
        }
      }  //Recruitment

    if(flag_catch(time) == 1){ // Flag if  there was a measurement that year

    for(int i=0;i<(nage-1);i++){ // Loop over ages for catch comp
      if(i<age_maxage){
        age_catch_est(i,time) = (CatchNAge(i+1,time)/CatchN(time)); // Catch comp (1 bc the data starts at age = 1)
      }else{
        age_catch_est(age_maxage-1,time) += (CatchNAge(i+1,time)/CatchN(time));
       }
     }
   }
 }


// // // Make the observation model
// using namespace density;
Type ans_survey=0.0;
////Save the observation model estimates
for(int time=1;time<tEnd;time++){ // Survey Surveyobs

      if(survey_x(time) == 2){
        ans_survey += -dnorm(log(Surveyobs(time)), log(survey(time)), SDsurv+survey_err(time), TRUE);
    }
  }

Type ans_catch = 0.0;
for(int time=0;time<tEnd;time++){ // Total Catches
        ans_catch += -dnorm(log(Catch(time)+1e-6), log(Catchobs(time)+1e-6), SDcatch, TRUE);
}




REPORT(ans_catch)
////Likelihood function for age composition in survey
//
Type ans_survcomp = 0.0;
Type ans_catchcomp = 0.0;


vector<Type>sum1(tEnd);
vector<Type>sum2(tEnd);

sum1.setZero();
sum2.setZero();


for(int time=1;time<tEnd;time++){ // Loop over available years
        if(flag_survey(time) == 1){ // Flag if  there was a measurement that year
        for(int i=1;i<age_maxage;i++){ // Loop over other ages (first one is empty for survey)
          sum1(time) += lgamma(ss_survey(time)*age_survey(i,time)+1);
          sum2(time) += lgamma(ss_survey(time)*age_survey(i,time) + phi_survey*ss_survey(time)*age_survey_est(i,time)) - lgamma(phi_survey*ss_survey(time)*age_survey_est(i,time));
        }
        ans_survcomp += lgamma(ss_survey(time)+1)-sum1(time)+lgamma(phi_survey*ss_survey(time))-lgamma(ss_survey(time)+phi_survey*ss_survey(time))+sum2(time);

      }

}


vector<Type>sum3(tEnd);
vector<Type>sum4(tEnd);
//
sum3.setZero();
sum4.setZero();

for(int time=1;time<tEnd;time++){ // Loop over available years
  if(Catch(time)>0){

        if(flag_catch(time) == 1){ // Flag if  there was a measurement that year
        for(int i=0;i<age_maxage;i++){ // Loop over other ages (first one is empty for survey)
          sum3(time) += lgamma(ss_catch(time)*age_catch(i,time)+1);
          sum4(time) += lgamma(ss_catch(time)*age_catch(i,time) + phi_catch*ss_catch(time)*age_catch_est(i,time)) - lgamma(phi_catch*ss_catch(time)*age_catch_est(i,time));
        }
        ans_catchcomp += lgamma(ss_catch(time)+1)-sum3(time)+lgamma(phi_catch*ss_catch(time))-lgamma(ss_catch(time)+phi_catch*ss_catch(time))+sum4(time);
      }
        }
}


Type ans_SDR = 0.0;

 for(int time=0;time<(tEnd-1);time++){ // Start time loop
   ans_SDR += Type(0.5)*(logR(time)*logR(time))/(SDR*SDR)+b(time)*log(SDR*SDR);
 }





// Error for Selectivity
Type ans_psel = 0.0;
//
for(int time=0;time<year_sel;time++){ // Start time loop
  for(int i=0;i<psel_fish.size();i++){ // Start time loop
        ans_psel += Type(0.5)*(PSEL(i,time)*PSEL(i,time))/(sigma_psel*sigma_psel);
      }
}

// Priors on h and M
Type ans_priors = 0.0;

for(int time=0;time<(nage-1);time++){ // Start time loop
  ans_priors += Type(0.5)*(initN(time)*initN(time))/(SDR*SDR);
}

// ans_priors += -dnorm(logh,log(Type(0.777)),Type(0.113),TRUE);

// Prior on h
ans_priors += -dbeta(h,Bprior,Aprior,TRUE);

if(sum_zero == 1){
  ans_priors += ((Type(0.0)-sum(logR))*(Type(0.0)-sum(logR)))/Type(0.01);
}

// ans_priors += -dnorm(logMinit, log(Type(0.2)), Type(0.1), TRUE);
ans_priors += 0.5*pow(logMinit-log(Type(0.2)),2)/Type(0.01);


vector<Type>ans_tot(7);
ans_tot(0) = ans_SDR;
ans_tot(1) = ans_psel;
ans_tot(2) = ans_catch;
ans_tot(3) = ans_survey;
ans_tot(4) = ans_survcomp;
ans_tot(5) = ans_catchcomp;
ans_tot(6) = ans_priors;

Type ans = ans_SDR+ans_psel+ans_catch+ans_survey-ans_survcomp-ans_catchcomp+ans_priors;
//


// Later Fix F in the likelihood and age comp in catch
// Type ans = 0.0;
// Report calculations
ADREPORT(SSB)
//ADREPORT(N)
ADREPORT(Catch)
ADREPORT(logF)
ADREPORT(R)
ADREPORT(Surveyobs)
ADREPORT(Fyear)
ADREPORT(surveyselc)
ADREPORT(catchselec)
ADREPORT(age_catch)
ADREPORT(age_catch_est)
ADREPORT(age_survey)
ADREPORT(age_survey_est)
ADREPORT(ans_tot)
ADREPORT(SSBzero)

REPORT(SSB)
REPORT(Fyear)
REPORT(Catch)
REPORT(R)
REPORT(Nzero)
REPORT(ans_tot)
REPORT(Zsave)
REPORT(age_survey_est)
REPORT(age_catch_est)
REPORT(CatchN)
REPORT(selectivity_save)
REPORT(surveyselc)
REPORT(N_beg)
REPORT(N_mid)
REPORT(Surveyobs)

  return ans;
}
