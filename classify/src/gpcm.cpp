#include "gpcm.h"
#include <math.h>
#include <vector>
using namespace Rcpp;

SEXP gpcm_c(SEXP x, SEXP y, SEXP z, SEXP zz){

	BEGIN_RCPP

	//NumericVector gpcm(float theta, float alpha, float beta[], int length, int mx);
	NumericVector gpcm(float theta, float alpha, std::vector<float> &beta, int length, int mx);
	NumericVector wlord(NumericMatrix xx, NumericVector yy);
	int i;
	int j;
	int itms;
	int mx_cats;
	int ppl;
	int pp;
	//Beta values are matrix with columns as categories
	NumericMatrix betas(x);
	NumericVector thetas(y);
	NumericVector cats(z);
	NumericVector alphas(zz);
	
	//Items
	itms = cats.size();
	//Maximum categories
	mx_cats = betas.ncol();
	//People
	ppl = thetas.size();
	
	std::vector<float> this_beta(mx_cats);
	//float this_beta[mx_cats];
	
	/* calculate number of score points
    sum of categories */
    int kk = 0;
    for (int i = 0; i < itms; i++){
    	kk += cats[i];
	}
    kk -= itms - 1;
	
	//Store the cum probabilities
    //One row per score point, one column per candidate	
	NumericMatrix cum_pb(kk,ppl);
	
	// Matrix to store probabilities
	// Column for item score category
	// Rows for item
	NumericMatrix pb(itms,mx_cats);
	
	//Loop through candidate theta values
    for (pp = 0; pp < ppl; pp++){	
			
			float theta;
			theta = thetas[pp];

			//Loop through items and get probabilities
			for (i = 0 ; i < itms ; i++){
			
				int k = cats[i];

				for (j = 0 ; j < mx_cats ; j++){
					this_beta[j] = betas(i,j);
				}	
			    
				NumericVector tmp_probs = gpcm(theta,alphas[i],this_beta,k,mx_cats);
				for (j = 0 ; j < k ; j++){
				   pb(i,j) = tmp_probs(j);				 
				}				
				
			}
			
			cum_pb(_,pp) = wlord(pb,cats);
	}		
	return cum_pb;
    END_RCPP

}

NumericVector gpcm(float theta, float alpha, std::vector<float> &beta, int length, int mx)
{
	NumericVector probs(mx);
	float denom = 0;
	std::vector<float> denoms(length);
	//float denoms[length];
	int i;
    int j;
	for (i = 0 ; i < length ; i++)
	  denoms[i] = 0;
	for (i = 0 ; i < length ; i++)
		for (j = 0 ; j <= i ; j++)
		    denoms[i] += float(alpha*(theta - beta[j]));
    for (i = 0 ; i < length ; i++)
        denom += exp(denoms[i]);
    for (i = 0 ; i < length ; i++)
        probs[i] = exp(denoms[i])/denom;
    return probs;
}

NumericVector wlord(NumericMatrix xx, NumericVector yy){

    // Number of items
    int xsize = yy.size();
    // Number of category columns
    //int xcols = xx.nrow();
	int xcols = xx.ncol();

	/* calculate number of score points
    sum of categories */
    int n_yy = yy.size();

    std::vector<int> cats(n_yy);
    int k = 0;
    for (int i = 0; i < n_yy; i++){
    	k += yy[i];
    	cats[i] = yy[i];
	}
	const int scrpts = k;
    k -= xsize - 1;

    /* Create SEXP to hold the answer */
    NumericVector pb(k);

    int min_c=1;
	int max_c=0;
	int prev_mx =0;
	int prev_min=0;
	double frx=0;
    const size_t arr_size = xsize;

    int *cat;
	cat = &cats[0];
	double *pia;
	double *ppa;
	//double *pla;
	double *ppia;
    double *ppla;

    //cond probs
	std::vector< std::vector<double> > pa(arr_size, std::vector<double>(scrpts,0));    
	std::vector< std::vector<double> >ia(xsize, std::vector<double>(xcols,0));

	for(int i=0;i<xsize;i++){
	  for(int j=0;j<xcols;j++){
		ia[i][j]=xx(i,j);
	  }
    }

	//loop through probs
	    int itm = 0;
	    for (int *ibegin=&cats[0], *iend=ibegin+arr_size; ibegin!=iend; ++ibegin){
	        if(itm==0){
	                   //loop through cats
	                   pia = &ia[0][0];
	                   ppa = &pa[0][0];
	                   for(int i=0; i!=*ibegin;i++){
	                           *ppa = *pia;
	                           pia++;
							   ppa++;
	                           max_c++;
	                   }
	        } else {
	                   pia = &ia[itm][0];
	                   //pla = &pa[itm-1][0];
	                   ppa = &pa[itm][0];

		               //loop through potential score points
	                   //potential score max
	                  	prev_mx = max_c;
	                   	prev_min = min_c;
	                    max_c = max_c + *cat;
	                    min_c = min_c + 1;
		                int l = 0;

			            for (int j=min_c;j!=max_c+1;j++){
	                        frx = 0;
	                    	//loop through each category
	                       	for (int k = 0;k!=*cat;++k){
	                            //calculate x-Wjk
	                    		if(l-k >= 0 && l-k < 1 + prev_mx-prev_min) {
									ppia= &ia[itm][0];
									ppia= ppia + k;
	                                //p.last
									ppla= &pa[itm-1][0];
	                                ppla= ppla + (l-k);
	                                frx = frx + (*ppia * *ppla);
	         					}

	          				}
	                    	*ppa = frx;
	                       	ppa++;
	                    	l++;
	                   	}
	              }
	              itm++;
	              cat++;

	    }

		ppa = &pa[itm-1][0];

		for (int k=0;k!=1 + max_c - min_c;k++){
        	pb[k] = *ppa;
        	ppa++;
    	}


  /* Wrap up and return the result to R */
    return pb;
	//return NULL;

}
