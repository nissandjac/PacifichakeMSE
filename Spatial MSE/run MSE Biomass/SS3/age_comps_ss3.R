// 
  
  
  
NLL-= gammln(A) -
        gammln(ninput_t(t)+A) + 
             sum(gammln(ninput_t(t)*extract_row(pobs_ta,t) + 
                 A*extract_row(pexp_ta,t))) - 
                     sum(lgamma(A*extract_row(pexp_ta,t))) 

temp = gammln(dirichlet_Parm) - 
          gammln(nsamp_a(f,i)+dirichlet_Parm)+
           
  
  EQ3<-  sum(gammln(nsamp_a(f,i)*obs_a(f,i)+
               dirichlet_Parm*exp_a(f,i)))-
                sum(gammln(dirichlet_Parm*exp_a(f,i)))



NLL -= 
  gammln(A) - 
    gammln(ninput_t(t)+A) + 
      sum(gammln(ninput_t(t)*extract_row(pobs_ta,t) + A*extract_row(pexp_ta,t))) - 
        sum(lgamma(A*extract_row(pexp_ta,t))) \
