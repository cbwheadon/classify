#include "exp.h"
#include <math.h>
#include <vector>
using namespace Rcpp;

SEXP exp_c(SEXP x, SEXP y, SEXP z, SEXP zz){

	BEGIN_RCPP

	float expect(float theta, float alpha, std::vector<float> &beta, int length, int mx);
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
	//People
	ppl = thetas.size();
	//return value numeric vector
	NumericVector expected(ppl);
	
	//Items
	itms = cats.size();
	//Maximum categories
	mx_cats = betas.ncol();
		
	std::vector<float> this_beta(mx_cats);
	
	//Loop through candidate theta values
    for (pp = 0; pp < ppl; pp++){	
			
			float theta;
			theta = thetas[pp];
			expected[pp] = 0;
			
			//Loop through items and get probabilities
			for (i = 0 ; i < itms ; i++){
			
				int k = cats[i];

				for (j = 0 ; j < mx_cats ; j++){
					this_beta[j] = betas(i,j);
				}	
			    
				expected[pp] += expect(theta,alphas[i],this_beta,k,mx_cats);			
				
			}
			
	}		
	return expected;
    END_RCPP

}

float expect(float theta, float alpha, std::vector<float> &beta, int length, int mx)
{
	float probs = 0;
	float denom = 0;
	std::vector<float> denoms(length);
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
        probs += (exp(denoms[i])/denom)*i;
    return probs;
}
