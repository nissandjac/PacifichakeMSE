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
vector<Type> Zzero = Fzero+M;

for(int i=0;i<nage;i++){ // Loop over ages
    SSBzero += Matsel(i)*Nzero(i)*0.5;
  }
// Run the initial distribution
array<Type>Ninit(nage,nage);
array<Type>SSBinit(nage);

for(int time=0;time<nage;time++){ // Start time loop
  if(time == 0){
    for(int i=1;i<(nage-1);i++){ // Loop over other ages

        Ninit(i,time) = Nzero(i);
        SSBinit(time) += Ninit(i,time)*Matsel(i)*0.5;
      }
    }else{
      Ninit(0,time) = (4*h*Rinit*SSBinit(time-1)/(SSBzero*(1-h)+ SSBinit(time-1)*(5*h-1)))*exp(-0.5*0*SDR*SDR+initN(time-1)); // First one is recruits
      for(int i=1;i<(nage-2);i++){ // Loop over other ages
      Ninit(i,time) = Ninit(i-1, time-1)*exp(-Zzero(i-1));
      }
      Ninit(nage-1, time) = Ninit(nage-2, time-1)*exp(-Zzero(nage-2))+Ninit(nage-1,time-1)*exp(-Zzero(nage-1));

      for(int i=0;i<(nage-1);i++){ // Loop over other ages
      SSBinit(time) += Ninit(time,i)*Matsel(i)*0.5;
      }

    }
}
REPORT(Ninit)
REPORT(SSBinit)
