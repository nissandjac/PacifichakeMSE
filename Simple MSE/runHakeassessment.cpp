// Create a file to run the TMB version of sprat
#include <TMB.hpp>
#include <iostream>

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
  DATA_VECTOR(age); // ages
  DATA_INTEGER(tEnd);
  DATA_INTEGER(year_sel);
  DATA_INTEGER(selYear);
  DATA_SCALAR(logQ);
  DATA_SCALAR(logphi_survey);
// // Selectivity
  DATA_INTEGER(Smin);
  DATA_INTEGER(Smin_survey);
  DATA_INTEGER(Smax);
  DATA_INTEGER(Smax_survey);
// // // Survey
  DATA_VECTOR(survey); // Acoustic survey
  DATA_VECTOR(survey_x); // Flag if survey occured
  DATA_VECTOR(ss_survey); // Age comp sample size
  DATA_VECTOR(flag_survey); // Were ages sampled this year
  DATA_ARRAY(age_survey); // Age compositions
  DATA_INTEGER(age_maxage); // Last age included in age comps
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
//   // Time parameters
  // Parameter integers
  PARAMETER(logRinit); // Recruitment at
  // //PARAMETER(rho);
  PARAMETER(logh); // Steepness
  PARAMETER(logMinit); // Natural mortality
  PARAMETER(logSDsurv); // Survey uncertainty
  //PARAMETER(logFinit);
  PARAMETER(logphi_catch);

  PARAMETER(logSDF);
  PARAMETER_VECTOR(psel_fish);
  PARAMETER_VECTOR(psel_surv);
  PARAMETER_VECTOR(initN);
  //
  PARAMETER_VECTOR(Rin); // Time varying stuff
  PARAMETER_VECTOR(F0);
  PARAMETER_ARRAY(PSEL); // Time varying selectivity

  // Transform out of log space
  Type SDsurv = exp(logSDsurv);
  Type SDcatch = exp(logSDcatch);
  Type SDR = exp(logSDR);
  Type SDF = exp(logSDF);
  Type Rinit = exp(logRinit);
  Type h = exp(logh);
  Type Minit = exp(logMinit);
  Type q = exp(logQ);
  //Type Finit = exp(logFinit);
  Type phi_survey = exp(logphi_survey);
  Type phi_catch = exp(logphi_catch);
//
// //  Minor calculations
  vector<Type> M = Minit*Msel; // Natural mortality
//   // Random effects (F and M)
  vector<Type> logF(tEnd);
  vector<Type> logR(tEnd);
//
  // Vectors for saving stuff
  vector<Type>R(tEnd);
  vector<Type>Catch(tEnd);
  array<Type> N(nage,tEnd);
  array<Type> CatchAge(nage,tEnd);
//

// Recruitment as a random effect
// logF(0) = 0;
// logR(0) = 0;
//logSel(0) = 0;

for(int j=0;j<(tEnd);j++){
      logR(j)=Rin(j);
}

for(int j=0;j<(tEnd);j++){
      logF(j)=F0(j);
}
REPORT(logF)
// selectivity
// survey
vector<Type>surveyselc(nage);
Type pmax = max(cumsum(psel_surv));
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

REPORT(catchselec)
REPORT(surveyselc)

vector<Type> Ninit(nage);
vector<Type> Nzero(nage); // Numbers with no fishing
vector<Type>Fzero = logF(0)*catchselec;
vector<Type>Meq = cumsum(M);
//
Nzero(0) = Rinit;
for(int i=1;i<(nage-1);i++){
    Ninit(i) = Rinit*exp(initN(i-1)) * exp(-(Meq(i-1)));
    Nzero(i) = Rinit * exp(-(Meq(i-1)));
  }
//
Ninit(nage-1) = Rinit*exp(initN(nage-2))*exp(-(Meq(nage-2)))/(Type(1.0)-exp(-M(nage-1))); // Plus group
Nzero(nage-1) = Rinit*exp(-(Meq(nage-2)))/(Type(1.0)-exp(-M(nage-1)));//*exp(initN(nage-2)); // Plus group

array<Type> SSBage(nage);
array<Type> Catchinit(nage);
array<Type>selectivity_save(nage,tEnd);
Type SSBinit = 0;
Type SSBzero = 0;
vector<Type> Zzero = Fzero+M;

for(int i=0;i<nage;i++){ // Loop over ages
    SSBage(i) = Matsel(i)*Ninit(i); // Assumes no fishing prior to dataset
    SSBinit += SSBage(i)*0.5;
    SSBzero += Matsel(i)*Nzero(i)*0.5;
    N(i,0) = Ninit(i);
    Catchinit(i)= (Fzero(i)/(Zzero(i)))*(1-exp(-Zzero(i)))*Ninit(i)*wage_catch(i,0);// Calculate the catch in kg
    Catch(0) += Catchinit(i);
    selectivity_save(i,0) = catchselec(i);
  }

Ninit(0) = (4*h*Rinit*SSBinit/(SSBzero*(1-h)+ SSBinit*(5*h-1)))*exp(logR(0));
N(0,0) = Ninit(0)*exp(logR(0));
R(0) = Ninit(0)*exp(logR(0));

// Save the initial conditions
// // Run the model over time
array<Type> SSB(tEnd);
array<Type>Biomass(tEnd); // Survey observed biomass
array<Type>Biomass_tot(tEnd); // Total biomass over age 2
array<Type>age_survey_est(age_maxage,tEnd);
array<Type>age_catch_est(age_maxage,tEnd);
//
vector<Type> Myear = M*Msel; // Natural mortality (if we want to change it later)
//
SSB(0) = SSBinit;
vector<Type> Fyear(tEnd);
vector<Type> Freal(nage);
vector<Type> Z(nage);
vector<Type>pmax_catch_save(tEnd);
vector<Type>psel_fish_zero = psel_fish;
pmax_catch_save(0) = pmax_catch;
Fyear(0) = logF(0);

for(int time=1;time<tEnd;time++){ // Start time loop
//     // Biomass from survey (add 0.5 to M and F later bc it's in the middle of the season)
    Fyear(time) = logF(time); // Random walk later
    Type Ntot_survey = 0;
    pmax_catch_save(time) = pmax_catch;
    // Take care of selectivity
    if (time >= (selYear-1)){
           for(int i=0;i<psel_fish.size();i++){
           psel_fish(i) = psel_fish_zero(i)+PSEL(i,time-selYear+1);
           }
           pmax_catch = max(cumsum(psel_fish));
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

    for(int i=0;i<(nage);i++){ // Loop over other ages
    Freal(i) = Fyear(time)*catchselec(i);
    Z(i) = Freal(i)+Myear(i);
    Biomass(time) += surveyselc(i)*wage_survey(i,time)*N(i,time-1)*q;
    Ntot_survey += surveyselc(i)*N(i,time-1); // To use with age comps
    selectivity_save(i,time) = catchselec(i);
    }

    for(int i=0;i<(nage-1);i++){ // Loop over other ages
      if(i < age_maxage){
      age_survey_est(i,time) = ((surveyselc(i+1)*N(i+1,time-1))/Ntot_survey);
      }else{
      age_survey_est(age_maxage-1,time) += ((surveyselc(i+1)*N(i+1,time-1))/Ntot_survey);
      }
    }
    // Recruitment
    R(time) = (4*h*Rinit*SSB(time-1)/(SSBzero*(1-h)+ SSB(time-1)*(5*h-1)))*exp(logR(time));

    N(0,time) = R(time); // First one is recruits
    //
    for(int i=1;i<(nage-1);i++){ // Loop over other ages
    N(i,time) = N(i-1, time-1)*exp(-Z(i-1));
    }
    // Plus group
    N(nage-1, time) = N(nage-2, time-1)*exp(-Z(nage-2))+N(nage-1,time-1)*exp(-Z(nage-1));
    //
    Catch(time) = 0;
    for(int i=0;i<nage;i++){ // Loop over other ages
    SSB(time) += N(i,time)*Matsel(i)*0.5; // Change zero to time later
    CatchAge(i,time)= (Freal(i)/(Z(i)))*(1-exp(-Z(i)))*N(i,time)*wage_catch(i,time);// Calculate the catch in kg
    Catch(time) += CatchAge(i,time);
    }
    // //
    for(int i=0;i<(nage-1);i++){ // Loop over ages for catch comp
      if(i<age_maxage){
        age_catch_est(i,time) = (CatchAge(i+1,time)/Catch(time)); // Catch comp (1 bc the data starts at age = 1)
      }else{
        age_catch_est(age_maxage-1,time) += (CatchAge(i+1,time)/Catch(time));
       }
     }
 }

REPORT(N)
REPORT(SSB)
REPORT(Catch)
REPORT(Biomass)
REPORT(R)
REPORT(Fyear)
REPORT(age_catch_est)
REPORT(age_survey_est)
REPORT(age_catch)
REPORT(age_survey)
REPORT(CatchAge)
REPORT(selectivity_save)
REPORT(pmax_catch_save)
// Make the observation model
using namespace density;
Type ans=0.0;
////Save the observation model estimates
for(int time=1;time<tEnd;time++){ // Survey biomass

      if(survey_x(time) == 2){
        ans += -dnorm(log(Biomass(time)), log(survey(time)), SDsurv, TRUE);
    }
  }

for(int time=0;time<tEnd;time++){ // Total Catches
        ans += -dnorm(log(Catch(time)+1), log(Catchobs(time)+1), SDcatch, TRUE);
}
////Likelihood function for age composition in survey
//
Type alpha =0;
for(int time=1;time<tEnd;time++){ // Loop over available years

        if(flag_survey(time) == 1){ // Flag if  there was a measurement that year

        for(int i=1;i<age_maxage;i++){ // Loop over other ages (first one is empty for survey)
          alpha = phi_survey*ss_survey(time);
          ans -=   lgamma(alpha)-lgamma(ss_survey(time)+alpha)+  //lgamma(ss_survey(time)+1)-lgamma(ss_survey(time)*age_survey(i,time)+1)+
                    lgamma(ss_survey(time)*age_survey(i,time)+alpha*age_survey_est(i,time))-
                    lgamma(alpha*age_survey(i,time));
        }
      }
}
//
//
Type beta =0;
for(int time=1;time<tEnd;time++){ // Loop over available years

        if(flag_catch(time) == 1){ // Flag if  there was a measurement that year

        for(int i=0;i<age_maxage;i++){ // Loop over other ages (first one is empty for survey)
          beta = phi_catch*ss_catch(time);
          if(age_catch(i,time)> 0){ // Ignore zero entries
          ans -=lgamma(beta)-lgamma(ss_catch(time)+beta)+
            lgamma(ss_catch(time)*age_catch(i,time)+beta*age_catch_est(i,time))-lgamma(beta*age_catch(i,time));//lgamma(ss_catch(time)+1)-lgamma(ss_catch(time)*age_catch(i,time)+1)+

        }
      }
    }
}


// REPORT(age_catch_est)
// REPORT(age_catch)
//
// // //Type Rmean = logR.mean();
// // //Error for recruitment deviations
// //
// for(int time=1;time<(tEnd);time++){ // Start time loop
//         ans += -dnorm(logR(time), Type(0.0), SDR, TRUE);
//
// }
 for(int time=1;time<(tEnd);time++){ // Start time loop
   ans+= Type(0.5)*(logR(time)*logR(time))/(SDR*SDR);
 }
// //
//
for(int time=1;time<(tEnd);time++){ // Start time loop
        ans += -dnorm(logF(time), logF(time-1), SDF, TRUE);
}

// Error for Selectivity
for(int time=0;time<year_sel;time++){ // Start time loop
  for(int i=0;i<psel_fish.size();i++){ // Start time loop
        ans += Type(0.5)*(PSEL(i,time)*PSEL(i,time))/(sigma_psel);
      }
}
REPORT(ans)

// Later Fix F in the likelihood and age comp in catch
// Type ans = 0.0;
// Report calculations
ADREPORT(SSB)
ADREPORT(N)
ADREPORT(Catch)
ADREPORT(logF)
ADREPORT(R)
ADREPORT(Biomass)
ADREPORT(Ninit)
ADREPORT(Fyear)
ADREPORT(surveyselc)
ADREPORT(catchselec)
  // Type ans = (a*a-1);
  return ans;
}
