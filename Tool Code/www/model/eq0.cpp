#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]

NumericVector EQ(int L, int N, NumericVector oldeq, NumericVector transit, 
        NumericVector transitionsiu1, NumericVector transitionsiu2, 
        NumericVector transitionsiv1, NumericVector transitionsiv2, 
        NumericVector eq){
  int i, iu1, iu2, iv1, iv2;
  
  for(i = 0; i < N; i++){
    eq[i] = oldeq[i];
  }
  for(i = 0; i < L; i++){
    
    iu1 = transitionsiu1[i] - 1;
    iv1 = transitionsiv1[i];
    iu2 = transitionsiu2[i] - 1;
    iv2 = transitionsiv2[i];
    
    eq[iu1] = eq[iu1] + transit[i] * iv1;
    eq[iu2] = eq[iu2] + transit[i] * iv2;
    
  } 
  
  return(eq);
}