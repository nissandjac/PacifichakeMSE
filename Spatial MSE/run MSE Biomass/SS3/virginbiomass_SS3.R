/*  SS_Label_FUNCTION 30 Do_Equil_Calc */
  FUNCTION void Do_Equil_Calc()
{
  int t_base;
  int bio_t_base;
  int bio_t;
  dvariable N_mid;
  dvariable N_beg;
  dvariable Fishery_Survival;
  dvariable crashtemp;
  dvariable crashtemp1;
  dvar_matrix Survivors(1,pop,1,gmorph);
  dvar_matrix Survivors2(1,pop,1,gmorph);
  
  t_base=styr+(eq_yr-styr)*nseas-1;
  bio_t_base=styr+(bio_yr-styr)*nseas-1;
  GenTime.initialize(); Equ_penalty.initialize();
  cumF.initialize(); maxF.initialize();
  SPB_equil_pop_gp.initialize();
  equ_mat_bio=0.0;
  equ_mat_num=0.0;
  equ_catch_fleet.initialize();
  equ_numbers.initialize();
  equ_catage.initialize();
  equ_F_std=0.0;
  totbio=0.0;
  smrybio=0.0;
  smryage=0.0;
  smrynum=0.0;
  # first seed the recruits; seems redundant
  for (g=1;g<=gmorph;g++)
  {
    if(use_morph(g)>0)
    {
      settle=settle_g(g);
      for (p=1;p<=pop;p++)
      {
        equ_numbers(Settle_seas(settle),p,g,Settle_age(settle)) = equ_Recr*recr_dist(GP(g),settle,p)*platoon_distr(GP2(g))*
          mfexp(natM(Settle_seas(settle),GP3(g),Settle_age(settle))*Settle_timing_seas(settle));
      }
    }
  }
  
  for (a=0;a<=3*nages;a++)     # go to 3x nages to approximate the infinite tail, then add the infinite tail
  {
    if(a<=nages) {a1=a;} else {a1=nages;}    # because selex and biology max out at nages
    
    for (s=1;s<=nseas;s++)
    {
      t=t_base+s;
      bio_t=bio_t_base+s;
      
      {
            N_beg=equ_numbers(s,p,g,a);
            if(F_Method==1)   ## Pope's approx
            {
            N_mid = N_beg*surv1(s,GP3(g),a1);     # numbers at middle of season
            Nsurvive=N_mid;                            # initial number of fishery survivors
            if(Fishon==1)
            {                       #  remove catch this round
            # check to see if total harves would exceed max_harvest_rate
            crashtemp=0.;  harvest_rate=1.0;
            for (f=1;f<=Nfleet;f++)
            if (fleet_area(f)==p && Hrate(f,t)>0.)
            {
            crashtemp+=Hrate(f,t)*deadfish(s,g,f,a1);
            }
            
            if(crashtemp>0.20)                  # only worry about this if the exploit rate is at all high
            {
            join1=1./(1.+mfexp(40.0*(crashtemp-max_harvest_rate)));  # steep joiner logistic curve at limit
            upselex=1./(1.+mfexp(Equ_F_joiner*(crashtemp-0.2)));          #  value of a shallow logistic curve that goes through the limit
            harvest_rate = join1 + (1.-join1)*upselex/(crashtemp);      # ratio by which all Hrates will be adjusted
            }
            
            for (f=1;f<=Nfleet;f++)
            if (fleet_area(f)==p && Hrate(f,t)>0. && fleet_type(f)<=2)
            {
            temp=N_mid*Hrate(f,t)*harvest_rate;     # numbers that would be caught if fully selected
            Nsurvive-=temp*deadfish(s,g,f,a1);       #  survival from fishery kill
            equ_catch_fleet(2,s,f) += temp*deadfish_B(s,g,f,a1);
            equ_catch_fleet(5,s,f) += temp*deadfish(s,g,f,a1);
            equ_catch_fleet(3,s,f) += temp*sel_al_2(s,g,f,a1);      # retained fishery kill in biomass
            
            equ_catch_fleet(1,s,f)+=temp*sel_al_1(s,g,f,a1);      #  total fishery encounter in biomass
            equ_catch_fleet(4,s,f)+=temp*sel_al_3(s,g,f,a1);    # total fishery encounter in numbers
            equ_catch_fleet(6,s,f)+=temp*sel_al_4(s,g,f,a1);      # retained fishery kill in numbers
            equ_catage(s,f,g,a1)+=temp*deadfish(s,g,f,a1);      #  dead catch numbers per recruit  (later accumulate N in a1)
            }
            }   # end removing catch
            
            Nsurvive *= surv1(s,GP3(g),a1);  # decay to end of season
            
            if(a<=a1)
            {
            equ_Z(s,p,g,a1) = -(log((Nsurvive+1.0e-13)/(N_beg+1.0e-10)))/seasdur(s);
            Fishery_Survival = equ_Z(s,p,g,a1)-natM(s,GP3(g),a1);
            if(a>=Smry_Age)
            {
            cumF(g)+=Fishery_Survival*seasdur(s);
            if(Fishery_Survival>maxF(g)) maxF(g)=Fishery_Survival;
            }
            }
            
            }   # end Pope's approx

              
            }  #  end F method
            Survivors(p,g)=Nsurvive;
          }
          else
          {
            equ_Z(s,p,g,a1)=natM(s,GP3(g),a1);
          }
      }  # end pop
  }  # end morph
      
      
      }  # end do migration
      
      for (g=1;g<=gmorph;g++)
        if(use_morph(g)>0)
        {
          for (p=1;p<=pop;p++)
          {
            if(s==nseas)  # into next age at season 1
            {
              if(a==3*nages)
              {
                # end of the cohort
              }
              else if(a==(3*nages-1))           # do infinite tail; note that it uses Z from nseas as if it applies annually
              {
                if(F_Method==1)
                {
                  equ_numbers(1,p,g,a+1) = Survivors(p,g)/(1.-exp(-equ_Z(nseas,p,g,nages)));
                }
                else
                {
                  equ_numbers(1,p,g,a+1) = Survivors(p,g)/(1.-exp(-equ_Z(nseas,p,g,nages)));
                }
              }
              else
              {
                equ_numbers(1,p,g,a+1) = Survivors(p,g);
              }
            }
            else
            {
              equ_numbers(s+1,p,g,a) = Survivors(p,g);  # same age, next season
            }
            
          }
        }
  }  # end season
    }  # end age
  
  # now calc contribution to catch and ssb
  for (g=1;g<=gmorph;g++)
    if(use_morph(g)>0)
    {
      gg=sx(g);
      for (s=1;s<=nseas;s++)
        for (p=1;p<=pop;p++)
        {
          t=t_base+s;
          bio_t=bio_t_base+s;
          equ_numbers(s,p,g,nages)+=sum(equ_numbers(s,p,g)(nages+1,3*nages));
          if(Fishon==1)
          {
            if(F_Method>=2)
            {
              Zrate2(p,g)=elem_div( (1.-mfexp(-seasdur(s)*equ_Z(s,p,g))), equ_Z(s,p,g));
              if(s<Bseas(g)) Zrate2(p,g,0)=0.0;
              for (f=1;f<=Nfleet;f++)
                if (fleet_area(f)==p && fleet_type(f)<=2)
                  if(Hrate(f,t)>0.0)
                  {
                    equ_catch_fleet(2,s,f)+=Hrate(f,t)*elem_prod(equ_numbers(s,p,g)(0,nages),deadfish_B(s,g,f))*Zrate2(p,g);      # dead catch bio
                    equ_catch_fleet(5,s,f)+=Hrate(f,t)*elem_prod(equ_numbers(s,p,g)(0,nages),deadfish(s,g,f))*Zrate2(p,g);      # deadfish catch numbers
                    equ_catch_fleet(3,s,f)+=Hrate(f,t)*elem_prod(equ_numbers(s,p,g)(0,nages),sel_al_2(s,g,f))*Zrate2(p,g);      # retained catch bio
                    equ_catage(s,f,g)=elem_prod(elem_prod(equ_numbers(s,p,g)(0,nages),deadfish(s,g,f)) , Zrate2(p,g));
                    equ_catch_fleet(1,s,f)+=Hrate(f,t)*elem_prod(equ_numbers(s,p,g)(0,nages),sel_al_1(s,g,f))*Zrate2(p,g);      # encountered catch bio
                    equ_catch_fleet(4,s,f)+=Hrate(f,t)*elem_prod(equ_numbers(s,p,g)(0,nages),sel_al_3(s,g,f))*Zrate2(p,g);      # encountered catch bio
                    equ_catch_fleet(6,s,f)+=Hrate(f,t)*elem_prod(equ_numbers(s,p,g)(0,nages),sel_al_4(s,g,f))*Zrate2(p,g);      # retained catch numbers
                  }
            }
            else  # F_method=1
            {
              # already done in the age loop
            }
          }
          
          if(s==1)
          {
            totbio += equ_numbers(s,p,g)(0,nages)*Wt_Age_beg(s,g)(0,nages);
            smrybio += equ_numbers(s,p,g)(Smry_Age,nages)*Wt_Age_beg(s,g)(Smry_Age,nages);
            smrynum += sum(equ_numbers(s,p,g)(Smry_Age,nages));
            smryage += equ_numbers(s,p,g)(Smry_Age,nages) * r_ages(Smry_Age,nages);
          }
          #  SPAWN-RECR:   calc generation time, etc.
          if(s==spawn_seas)
          {
            if(gg==1)  # compute equilibrium spawning biomass for females
            {
              tempvec_a=elem_prod(equ_numbers(s,p,g)(0,nages),mfexp(-spawn_time_seas*equ_Z(s,p,g)(0,nages)));
              SPB_equil_pop_gp(p,GP4(g))+=tempvec_a*fec(g);
              equ_mat_bio+=elem_prod(equ_numbers(s,p,g)(0,nages),mfexp(-spawn_time_seas*equ_Z(s,p,g)(0,nages)))*make_mature_bio(GP4(g));
              equ_mat_num+=elem_prod(equ_numbers(s,p,g)(0,nages),mfexp(-spawn_time_seas*equ_Z(s,p,g)(0,nages)))*make_mature_numbers(GP4(g));
              GenTime+=tempvec_a*elem_prod(fec(g),r_ages);
            }
            else if(Hermaphro_Option>0 && gg==2)
            {
              tempvec_a=elem_prod(equ_numbers(s,p,g)(0,nages),mfexp(-spawn_time_seas*equ_Z(s,p,g)(0,nages)));
              MaleSPB_equil_pop_gp(p,GP4(g))+=tempvec_a*Wt_Age_beg(s,g)(0,nages);
            }
          }
        }
    }
  
  YPR_dead =   sum(equ_catch_fleet(2));    # dead yield per recruit
  YPR_N_dead = sum(equ_catch_fleet(5));    # dead numbers per recruit
  YPR_enc =    sum(equ_catch_fleet(1));    #  encountered yield per recruit
  YPR_ret =    sum(equ_catch_fleet(3));    # retained yield per recruit
  
  if(Fishon==1)
  {
    if(F_reporting<=1)
    {
      equ_F_std=YPR_dead/smrybio;
    }
    else if(F_reporting==2)
    {
      equ_F_std=YPR_N_dead/smrynum;
    }
    else if(F_reporting==3)
    {
      if(F_Method==1)
      {
        for (s=1;s<=nseas;s++)
        {
          t=t_base+s;
          for (f=1;f<=Nfleet;f++)
          {
            equ_F_std+=Hrate(f,t);
          }
        }
      }
      else
      {
        for (s=1;s<=nseas;s++)
        {
          t=t_base+s;
          for (f=1;f<=Nfleet;f++)
          {
            equ_F_std+=Hrate(f,t)*seasdur(s);
          }
        }
      }
    }
    else if(F_reporting==4)
    {
      temp1=0.0;
      temp2=0.0;
      for (g=1;g<=gmorph;g++)
        if(use_morph(g)>0)
        {
          for (p=1;p<=pop;p++)
          {
            for (a=F_reporting_ages(1);a<=F_reporting_ages(2);a++)   #  should not let a go higher than nages-2 because of accumulator
            {
              if(nseas==1)
              {
                temp1+=equ_numbers(1,p,g,a+1);
                temp2+=equ_numbers(1,p,g,a)*mfexp(-seasdur(1)*natM(1,GP3(g),a));
              }
              else
              {
                temp1+=equ_numbers(1,p,g,a+1);
                temp3=equ_numbers(1,p,g,a);  #  numbers at begin of year
                for (int kkk=1;kkk<=nseas;kkk++) {temp3*=mfexp(-seasdur(kkk)*natM(kkk,GP3(g),a));}
                temp2+=temp3;
              }
            }
          }
        }
      equ_F_std = log(temp2)-log(temp1);
    }
  }
  SPB_equil=sum(SPB_equil_pop_gp);
  GenTime/=SPB_equil;
  smryage /= smrynum;
  cumF/=(r_ages(nages)-r_ages(Smry_Age)+1.);
  if(Hermaphro_maleSPB==1) SPB_equil+=sum(MaleSPB_equil_pop_gp);
  
  }  #  end equil calcs

