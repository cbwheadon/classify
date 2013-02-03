#include "rcpp_w_lord.h"
#include <vector>

SEXP rcpp_w_lord_c(SEXP x, SEXP y){

	using namespace Rcpp;

	BEGIN_RCPP

	NumericMatrix xx(x);
	NumericVector yy(y);
	
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
	//double pa[arr_size][scrpts];
    //double ia[xsize][xcols];
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
    END_RCPP

}
