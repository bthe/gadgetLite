#include <TMB.hpp>
//#include <fenv.h> // Extra line needed 
 
template<class Type> 
Type objective_function<Type>::operator() () { 

//  feenableexcept(FE_INVALID | FE_OVERFLOW | FE_DIVBYZERO | FE_UNDERFLOW);	
//  feraiseexcept(FE_INVALID | FE_OVERFLOW | FE_DIVBYZERO | FE_UNDERFLOW);    
	 
  DATA_ARRAY(SI);            //survey indices from the spring survey 
  DATA_ARRAY(ldistSI);       //length distributions from the survey 
  DATA_ARRAY(ldistComm);     //length distributions from the commercial fleet 
  DATA_ARRAY(aldistSI);      //age-length distributions from the survey 
  DATA_ARRAY(aldistComm);    //age-length distributions from the commerical fleet 
  DATA_ARRAY(fleetCatches); //catches 
 
  DATA_VECTOR(SIlgroups);     //length group intervals for the survey idx  
//  DATA_VECTOR(matpar);        //maturity parameters
  DATA_INTEGER(firstyear);    //first year of the simulation 
  DATA_INTEGER(lastyear);     //last year of the simulation 
  DATA_INTEGER(maxlgr);       //maximum length group growth 
  DATA_INTEGER(minage);       //minimum age of the stock 
  DATA_INTEGER(maxage);       //maximum age of the stock 
  DATA_INTEGER(minlength);    //maximum length of the stock 
  DATA_INTEGER(maxlength);    //maximum length of the stock 
  DATA_INTEGER(rfunc);        //recruitment function 0='fixed effect',1='Ricker'
  DATA_VECTOR(M);             //natural mortality 
  DATA_VECTOR(initSigma);     //variation in initial length at age  
  DATA_VECTOR(wa);            //weight length relationship 
  DATA_VECTOR(compW);         //likelihood component weights 
  DATA_VECTOR(fleetMort);     //fleet mortality
   
  PARAMETER_VECTOR(recruits); //number of minage year olds 
  PARAMETER(recl);               //average length at recruitment 
  PARAMETER(recsd);              //std.dev length at recruitment 
  PARAMETER_VECTOR(initial);     //initial number at age at year 0 
  PARAMETER(aComm);              //commerical suitability params 
  PARAMETER(bComm); 
  PARAMETER(aSurv);              //survey suitability params 
  PARAMETER(bSurv); 
  PARAMETER(k);                  //growth k 
  PARAMETER(linf);               //growth linf 
  PARAMETER(beta);               //growth dispersion parameter 
  PARAMETER_VECTOR(SIa);         //alpha in log(Idx) = a + log(N) 
  PARAMETER(meanrec);            //average num recruits
  PARAMETER(log_sigma);            //var num recruits
  PARAMETER(rickmu);
  PARAMETER(ricklambda);
  PARAMETER_VECTOR(lik_sigma);
  
  
  Type lik_idx, lik_sldist, lik_saldist, 
    lik_cldist, lik_caldist,lik_rec;     // likelihood component scores 
  double numyears = lastyear - firstyear + 1; 
 
  vector<Type> len(maxlength); 
  vector<Type> wl(maxlength); 
   
  // follow the stock status through abundance by length and age at time 
  array<Type> stkArr(maxage,maxlength,numyears,4); 
  // catches by length and age at time 
  array<Type> survArr(maxage,maxlength,numyears,4);   
  array<Type> commArr(maxage,maxlength,numyears,4);   
  array<Type> survArrALP(maxage,maxlength,numyears,4);   
  array<Type> commArrALP(maxage,maxlength,numyears,4);   
  array<Type> survArrLP(maxlength,numyears,4);   
  array<Type> commArrLP(maxlength,numyears,4);   
  array<Type> predidx(SIlgroups.size(),numyears);  
  array<Type> tmpGrowth(maxage,maxlength);
  // Growth 
  matrix<Type> G(maxlength,maxlength); 
 
  
  // selection 
  vector<Type> suitSurv(maxlength);   
  vector<Type> suitComm(maxlength); 
  vector<Type> dmu(maxlength); 
  vector<Type> alpha(maxlength); 

  // mean length at age
  vector<Type> mu(maxage); 
  Type kk = exp(k);
  Type bbeta = exp(beta);
  
  // length and weight calculated
  for(int l = minlength+1; l < maxlength+1;l++){ 
    len(l-1) = Type(l); 
    wl(l-1) = exp(log(wa(0))+log(len(l-1))*wa(1)); 
  } 
  
  for(int l = 0; l < minlength; l++){
    len(l) = 0;
    wl(l) = 0;
  }
  
  // suitability functions (needs updating with different types of suitability)
  for(int l = minlength; l < maxlength;l++){ 
      suitSurv(l) = 1/(1+exp(-(aSurv+bSurv*len(l)))); 
      suitComm(l) = 1/(1+exp(-(aComm+bComm*len(l)))); 
  } 
   
 
  // Calculate average growth by lgroup 
  for(int i=minlength; i < maxlength; i++){ 
    dmu(i-1) = linf*(1-len(i)/linf)*(1-exp(-kk/4)); 
    alpha(i-1) = (bbeta*dmu(i-1)/(len(i) - len(i-1)))/(maxlgr-dmu(i-1)/(len(i) - len(i-1))); 
  } 
  // biggest lengthgroup doesn't grow 
  dmu(maxlength-1) = 0; 
  alpha(maxlength-1) = 0; 
  
  // growthprob i -> j 
  for(int i=minlength; i<maxlength-1; i++){ 
    for(int x=0; x < std::min(maxlgr+1,maxlength-i); x++){ 
      G(i,i+x) = exp(lgamma(maxlgr + Type(1.0))+ 
      lgamma(alpha(i) + bbeta) + 
      lgamma(maxlgr - Type(x) + bbeta) + 
      lgamma(Type(x) + alpha(i)) - 
      lgamma(maxlgr - Type(x) + Type(1.0)) - 
      lgamma(Type(x) + Type(1.0)) - 
      lgamma(maxlgr + alpha(i) + bbeta) - 
      lgamma(bbeta) - 
      lgamma(alpha(i))); 
    } 
  } 
  G(maxlength-1,maxlength-1) = 1; 
  
  
  // initial length at age 
  //Type t0 = log(1-recl/linf)/kk-1;
  
  for(int a=minage-1; a < maxage; a++){ 
    mu(a) = linf*(1-exp(-kk*((a+1)))); 
  } 
  
  //looping variables 
  int year, step, l, a;  
  
  Type survBio, harvBio, survNum, harvNum, rtmp, SSB; 
  rtmp = Type(0);

  for(year=0; year < numyears; year++){ 
    for(step=0; step < 4; step++){ 
      
      
   
      
      if(year == 0 && step == 0){ // initial conditions 
	for(a = minage; a < maxage; a++){ 
	  for(l = minlength+1; l < maxlength; l++){	   
	    stkArr(a,l,year,step) =  
	      initial(a-1)*(pnorm(len(l),mu(a),initSigma(a)) - 
			    pnorm(len(l-1),mu(a),initSigma(a)))*Type(1e9); 
	  }  
	  // fix the edges 
	  stkArr(a,minlength,year,step) =  
	    initial(a-1)*pnorm(len(minlength),mu(a),initSigma(a))*Type(1e9); 
	  stkArr(a,maxlength-1,year,step) =  
	    initial(a-1)*(1-pnorm(len(maxlength-1),mu(a),initSigma(a)))*Type(1e9); 
	  
	   
	} 
      } else if(step == 0) { // update age 
      
	for(a = minage; a < maxage; a++){ 
	  for(l = minlength; l < maxlength; l++){ 
	    stkArr(a,l,year,0) = stkArr(a-1,l,year-1,3); 
	  } 
	} 
	// plus group 
	for(l = minlength; l < maxlength; l++){ 
	  stkArr(maxage-1,l,year,0) += stkArr(maxage-1,l,year-1,3); 
	} 
      } else { // move between timestep 
	for(a = minage-1; a < maxage; a++){ 
	  for(l = minlength; l < maxlength; l++){ 
	    stkArr(a,l,year,step) = stkArr(a,l,year,step-1); 
	  } 
	} 
      } 


      if(step == 0){ // recruitment 
	SSB = Type(0);
	for(a = 3; a<maxage;a++){ 
	  for(l = minlength; l<maxlength;l++){
	    SSB += wl(l)*stkArr(a,l,year,step);
	  } 
	}  

        // ricker type recruitment         
        rtmp = exp(recruits(year) - exp(log_sigma*2)/2)*rickmu*SSB*
	  exp(-ricklambda*Type(1e-11)*SSB);
        // rfunc swithes between the two
	rtmp = exp(recruits(year))*Type(1e9)*(Type(1)-rfunc)+rtmp*rfunc;
	
        
	for(l = minlength+1; l < maxlength; l++){ 	  
	  stkArr(minage-1,l,year,step) = 
	    rtmp*(pnorm(len(l),recl,recsd) - 
		  pnorm(len(l-1),recl,recsd));	
	}  
	stkArr(minage-1,minlength,year,step) =  
	  rtmp*pnorm(len(minlength),recl,recsd); 
	stkArr(minage-1,maxlength-1,year,step) =  
	  rtmp*(1-pnorm(len(maxlength-1),recl,recsd)); 
      }   

      
      // calculate catches 
      harvBio = 0; 
      survBio = 0; 
      survNum = 0;
      harvNum = 0;
      for(a = minage-1; a<maxage;a++){ 
        for(l = minlength; l<maxlength;l++){
          harvBio += suitComm(l)*wl(l)*stkArr(a,l,year,step);
          survBio += suitSurv(l)*wl(l)*stkArr(a,l,year,step); 
          survNum += suitSurv(l)*stkArr(a,l,year,step);
          harvNum += 0.25*fleetMort(4*year+step)*suitComm(l)*stkArr(a,l,year,step);
        } 
      } 
      
      
      // fleet operations 
      for(a = minage-1; a<maxage;a++){ 
        for(l = minlength; l<maxlength;l++){ 
          if(step==1){ 
            // assuming catch of 1 kg
            survArr(a,l,year,step) = stkArr(a,l,year,step) *  
            suitSurv(l)/(survNum); 
            // reporting 
            survArrALP(a,l,year,step) = survArr(a,l,year,step); 
            survArrLP(l,year,step) += survArr(a,l,year,step); 
          } 
          
          //	  commArr(a,l,year,step) = std::min(fleetCatches(year,step),0.95*harvBio)*
          //	    stkArr(a,l,year,step) * suitComm(l)/(harvBio); 
          commArr(a,l,year,step) = Type(0.25)*fleetMort(4*year+step)*suitComm(l)*stkArr(a,l,year,step);
          stkArr(a,l,year,step) -= commArr(a,l,year,step); 
          //reporting 
          commArrALP(a,l,year,step) = commArr(a,l,year,step)/(harvNum); 
          commArrLP(l,year,step) += commArr(a,l,year,step)/(harvNum); 
        } 
      } 
      
      for(a = minage - 1; a<maxage; a++){ 
        for(l = minlength; l<maxlength;l++){ 
          tmpGrowth(a,l) = 0;
        }
      }
      
      // growth  
      for(a = minage - 1; a<maxage; a++){ 
        for(l = minlength; l<maxlength;l++){ 
          for(int x = 0; x < std::min(maxlgr+1,maxlength-l); x++){ 
            /// check this
            tmpGrowth(a,l+x) += G(l,l+x)*stkArr(a,l,year,step); 
          } 
        } 
      } 
      
      for(a = minage - 1; a<maxage; a++){ 
        for(l = minlength; l<maxlength;l++){ 
          stkArr(a,l,year,step) = tmpGrowth(a,l);
        }
      }
      
      for(a = minage - 1; a<maxage; a++){ 
        for(l = minlength; l<maxlength;l++){ 
          stkArr(a,l,year,step) = exp(-M(a)*0.25)*stkArr(a,l,year,step); 
        } 
      } 
      
    }    
    
    // survey index 
    l = minlength;
    for(int lg=0; lg < SIlgroups.size(); lg++){  
      predidx(lg,year) = 0; 
      while(len(l) <= std::min(SIlgroups(lg), Type(maxlength)-1)){ 
        for(a=minage-1;a<maxage;a++){ 
          predidx(lg,year) += stkArr(a,l,year,1); 
        } 
        l++; 
      } 
    } 
    
  }
   
  
  // likelihood functions 
  lik_idx = 0; 
  lik_sldist = 0; 
  lik_saldist = 0; 
  lik_cldist = 0; 
  lik_caldist = 0; 
  lik_rec = 0;
  
  lik_rec -= dnorm(recruits,Type(0),exp(log_sigma),true).sum()+ 0*meanrec;// + 0*log_sigma;
  for(year = 0; year < numyears; year++){ 
    // deal with the survey first 

    //lik_rec -= dnorm(recruits(year),Type(0),Type(0.01),true) 

    for(int lg=0; lg < SIlgroups.size(); lg++){ 
      lik_idx -= dnorm(log(SI(lg,year)),
		       (SIa(lg) + log(predidx(lg,year)+Type(0.00001))),
		       exp(lik_sigma(0)),true);
      
    }     
    for(l=minlength; l<maxlength; l++){ 
      // assumes that missing full observation at year/step combination is  
      // indicated by -1 
      if(ldistSI(l-minlength,year) != -1){ 
        lik_sldist -= dnorm(ldistSI(l-minlength,year),
			    survArrLP(l,year,1),
			    exp(lik_sigma(1)),true);  
      } 
      
      for(a=minage-1; a < maxage; a++){ 
        if(aldistSI(a,l-minlength,year) != -1){ 
          lik_saldist -= dnorm(aldistSI(a,l-minlength,year),
			       survArrALP(a,l,year,1),
			       exp(lik_sigma(2)),true);  
        } 
      } 
      
    } 
    // now commercial 
    for(step = 0; step < 4; step++){ 
      for(l=minlength; l<maxlength; l++){ 
        // assumes that missing full observation at year/step combination is  
        // indicated by -1 
        if(ldistComm(l-minlength,year,step) != -1){ 
          lik_cldist -= dnorm(ldistComm(l-minlength,year,step),
			      commArrLP(l,year,step), 
			      exp(lik_sigma(3)),true);  
        } 
        
        for(a=minage-1; a < maxage; a++){ 
          if(aldistComm(a,l-minlength,year,step) != -1){ 
            lik_caldist -= dnorm(aldistComm(a,l-minlength,year,step),
				 commArrALP(a,l,year,step), 
				 exp(lik_sigma(4)),true);  
          } 
        }
        
      } 
    } 
  } 
  
  
  
  Type nll = (compW(0)*lik_idx + compW(1)*lik_sldist + compW(2)*lik_saldist + 
	      compW(3)*lik_cldist + compW(4)*lik_caldist + compW(5)*lik_rec);
  
  REPORT(G);
  REPORT(stkArr);
  REPORT(commArr);
  REPORT(len);
  REPORT(predidx);
  REPORT(lik_idx);
  REPORT(lik_sldist);
  REPORT(lik_saldist);
  REPORT(lik_cldist);
  REPORT(lik_caldist);
  REPORT(lik_rec);
  REPORT(survArrLP);
  REPORT(survArrALP);
  REPORT(commArrLP);
  REPORT(commArrALP);

  //ADREPORT(ricklambda);
  
  return nll;
} 
