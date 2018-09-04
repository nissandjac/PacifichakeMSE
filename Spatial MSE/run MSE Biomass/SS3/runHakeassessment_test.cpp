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
  DATA_VECTOR(b); // bias adjustment factor
  DATA_SCALAR(logphi_survey);
  DATA_VECTOR(years);
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
  PARAMETER_VECTOR(Rin); // Time varying stuff
  PARAMETER_VECTOR(F0);
  PARAMETER_ARRAY(PSEL); // Time varying selectivity
//  PARAMETER(a);

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
  array<Type> CatchAge(nage,tEnd);
//


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

vector<Type> Nzero(nage); // Numbers with no fishing
vector<Type>Meq = cumsum(M);
//
Nzero(0) = Rinit;
for(int i=1;i<(nage-1);i++){
    Nzero(i) = Rinit * exp(-(Meq(i-1)));
  }
//
Nzero(nage-1) = Rinit*exp(-(Meq(nage-2)))/(Type(1.0)-exp(-M(nage-1)));//*exp(initN(nage-2)); // Plus group

array<Type> SSBage(nage);
array<Type> Catchinit(nage);
array<Type>selectivity_save(nage,tEnd);
Type SSBzero = 0;
vector<Type> Zzero = M;

for(int i=0;i<nage;i++){ // Loop over ages
    SSBzero += Matsel(i)*Nzero(i)*0.5;
  }
// Run the initial distribution
array<Type>Ninit(nage,nage);
array<Type>SSBinit(nage);

for(int i=0;i<(nage);i++){ // Loop over other ages
    Ninit(i,0) = Nzero(i);
  }
  SSBinit(0) = SSBzero;


for(int time=1;time<nage;time++){ // Start time loop
      Ninit(0,time) = (4*h*Rinit*SSBinit(time-1)/(SSBzero*(1-h)+ SSBinit(time-1)*(5*h-1)))*exp(-0.5*0*SDR*SDR+initN(time-1)); // First one is recruits

      for(int i=1;i<(nage-1);i++){ // Loop over other ages
      Ninit(i,time) = Ninit(i-1, time-1)*exp(-Zzero(i-1));
      }

      Ninit(nage-1, time) = Ninit(nage-2, time-1)*exp(-Zzero(nage-2))+Ninit(nage-1,time-1)*exp(-Zzero(nage-1)); // Plus group

      for(int i=0;i<(nage-1);i++){ // Loop over other ages
      SSBinit(time) += Ninit(i,time)*Matsel(i)*0.5;
      }

}
REPORT(Ninit)
REPORT(SSBinit)
// Plus group
vector<Type>Catch(tEnd);
array<Type> N(nage,tEnd);

// // Save the initial conditions
// for(int i=1;i<(nage-1);i++){ // Loop over other ages
//   N(i,0) = Ninit(i,nage);
//   Catchinit(i)= (Fzero(i)/(Zzero(i)))*(1-exp(-Zzero(i)))*Ninit(i)*wage_catch(i,0);// Calculate the catch in kg
//   Catch(0) += Catchinit(i);
//   selectivity_save(i,0) = catchselec(i);
//
// }
// // Run the model over time
array<Type> SSB(tEnd);
array<Type>Biomass(tEnd); // Survey observed biomass
array<Type>Biomass_tot(tEnd); // Total biomass over age 2
array<Type>age_survey_est(age_maxage,tEnd);
array<Type>age_catch_est(age_maxage,tEnd);
//
vector<Type> Myear = M*Msel; // Natural mortality (if we want to change it later)
//
vector<Type> Fyear(tEnd);
vector<Type> Freal(nage);
vector<Type> Z(nage);
vector<Type>pmax_catch_save(tEnd);
vector<Type>psel_fish_zero = psel_fish;

Type SSBtmp = 0;
vector<Type>Ntmp(nage);
//
for(int time=0;time<tEnd;time++){ // Start time loop
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
  if (time == 0){
      for(int j=0;j<(nage);j++){ // Fix the Catch selectivity
      Ntmp(j) = Ninit(j,nage-1);
    }
      SSBtmp = SSBinit(nage-1);
  }else{
    for(int j=0;j<(nage);j++){ // Fix the Catch selectivity
     Ntmp(j) = N(j,time-1);
  }
      SSBtmp = SSB(time-1);
}
  REPORT(SSBtmp)
//
//
    for(int i=0;i<(nage);i++){ // Loop over other ages
    Freal(i) = Fyear(time)*catchselec(i);
    Z(i) = Freal(i)+Myear(i);
    Biomass(time) += surveyselc(i)*wage_survey(i,time)*Ntmp(i)*q;
    Ntot_survey += surveyselc(i)*Ntmp(i); // To use with age comps
    selectivity_save(i,time) = catchselec(i);
    }

    for(int i=0;i<(nage-1);i++){ // Loop over other ages
      if(i < age_maxage){
      age_survey_est(i,time) = ((surveyselc(i+1)*Ntmp(i+1))/Ntot_survey);
      }else{
      age_survey_est(age_maxage-1,time) += ((surveyselc(i+1)*Ntmp(i+1))/Ntot_survey);
      }
    }
    //Recruitment
    R(time) = (4*h*Rinit*SSBtmp/(SSBzero*(1-h)+ SSBtmp*(5*h-1)))*exp(-0.5*b(time)*SDR*SDR+logR(time));

    N(0,time) = R(time); // First one is recruits
    //
    for(int i=1;i<(nage-2);i++){ // Loop over other ages
    N(i,time) = Ntmp(i-1)*exp(-Z(i-1));
    }
    // Plus group
    N(nage-1, time) = Ntmp(nage-2)*exp(-Z(nage-2))+Ntmp(nage-1)*exp(-Z(nage-1));
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
// //
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
// // // Make the observation model
// using namespace density;
Type ans_survey=0.0;
////Save the observation model estimates
for(int time=1;time<tEnd;time++){ // Survey biomass

      if(survey_x(time) == 2){
        ans_survey += -dnorm(log(Biomass(time)), log(survey(time)), SDsurv+survey_err(time), TRUE);
    }
  }

Type ans_catch = 0.0;
for(int time=0;time<tEnd;time++){ // Total Catches
        ans_catch += -dnorm(log(Catch(time)+1), log(Catchobs(time)+1), SDcatch, TRUE);
}
////Likelihood function for age composition in survey
//
Type alpha =0;
Type ans_survcomp = 0.0;

for(int time=1;time<tEnd;time++){ // Loop over available years

        if(flag_survey(time) == 1){ // Flag if  there was a measurement that year

        for(int i=1;i<age_maxage;i++){ // Loop over other ages (first one is empty for survey)
          alpha = phi_survey*ss_survey(time);
          ans_survcomp -=   lgamma(alpha)-lgamma(ss_survey(time)+alpha)+  //lgamma(ss_survey(time)+1)-lgamma(ss_survey(time)*age_survey(i,time)+1)+
                    lgamma(ss_survey(time)*age_survey(i,time)+alpha*age_survey_est(i,time))-
                    lgamma(alpha*age_survey(i,time));
        }
      }
}
//
//
Type beta =0;
Type ans_catchcomp = 0.0;

for(int time=1;time<tEnd;time++){ // Loop over available years

        if(flag_catch(time) == 1){ // Flag if  there was a measurement that year

        for(int i=0;i<age_maxage;i++){ // Loop over other ages (first one is empty for survey)
          beta = phi_catch*ss_catch(time);
          if(age_catch(i,time)> 0){ // Ignore zero entries
          ans_catchcomp -=lgamma(beta)-lgamma(ss_catch(time)+beta)+
            lgamma(ss_catch(time)*age_catch(i,time)+beta*age_catch_est(i,time))-lgamma(beta*age_catch(i,time));//lgamma(ss_catch(time)+1)-lgamma(ss_catch(time)*age_catch(i,time)+1)+

        }
      }
    }
}
REPORT(beta)
REPORT(ss_catch)
REPORT(age_catch)
REPORT(phi_catch)

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
Type ans_SDR = 0.0;

 for(int time=1;time<(tEnd);time++){ // Start time loop
   ans_SDR += Type(0.5)*(logR(time)*logR(time))/(SDR*SDR);
 }
// //
//
Type ans_F = 0.0;

for(int time=1;time<(tEnd);time++){ // Start time loop
        ans_F += -dnorm(logF(time), logF(time-1), SDF, TRUE); // Ensures smoothing on F
}

// Error for Selectivity
Type ans_psel = 0.0;

for(int time=0;time<year_sel;time++){ // Start time loop
  for(int i=0;i<psel_fish.size();i++){ // Start time loop
        ans_psel += Type(0.5)*(PSEL(i,time)*PSEL(i,time))/(sigma_psel);
      }
}
Type ans = ans_F+ans_SDR+ans_psel+ans_catch+ans_survey+ans_survcomp+ans_catchcomp;
//
REPORT(ans)
REPORT(ans_SDR)
REPORT(ans_psel)
REPORT(ans_catch)
REPORT(ans_survey)
REPORT(ans_survcomp)
REPORT(ans_catchcomp)
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
  return ans;
}
