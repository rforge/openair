#include <Rcpp.h>
using namespace Rcpp;



// This function calculates rolling means taking acount of the window size and a
// data capture threshold. Means are calculated only when the data capture is >
// than 'cap' else NA is returned.

// The bulk of the calculations are run using the main function
// The ends are then specifically dealt with depending how the window is aligned

// Declarations
NumericVector ends(NumericVector A, LogicalVector NA, NumericVector res, std::string align, 
int start, int end, double lenr, double capr);

RcppExport SEXP rollingMean(SEXP x, SEXP lenr, SEXP capr, SEXP alignr) {
  NumericVector A(x); // the data
  int n = A.size(); // length of data
  double cap = as<double>(capr); // data capture %
  double len = as<double>(lenr); // window size %
  std::string align = as<std::string>(alignr); // window (left, center, right)

  NumericVector res(n); // for results
  LogicalVector NA(A); // for missings

  double sum = 0; // sum of values over window
  double sumNA = 0; // number of missings
  int start = 0; //use this to set offset to get alignment
  int end = 0; //use this to set offset to get alignment

  NA = is_na(A) ; // logical vector of missings

  // make sure window width is less than data length
  if (len > n) {
    //  std::cout << "Window width more than length of data, use a smaller window width." ;
    len = 1;
  }

  // determine where to index and update
  if (align == "left") start = (len - 1);
  if ((align == "center") || (align == "centre")) {
    start = floor(len / 2);
    align = "centre"; // force single spelling!
  }

  // main loop
  for (int i = 0; i <= (n - len); i++) {
    sum = 0; // initialise
    sumNA = 0;

    // now go through each window
    for (int j = i; j < (i + len); j++) {

      if (NA(j)) {
        sumNA += 1; // count missings
      }
      else
        {
          sum += A(j); // sum values that are not missing
        }
    }

    // calculate mean if within data capture threshold, if not set to missing

    if (1 - sumNA / len < cap / 100) {
      res(i + len - 1 - start) = NA_REAL;
    }
    else
      {
        res(i + len - 1 - start) = sum / (len - sumNA);
      }
  }

  if (align == "right") start = 0, end = len - 2;
  if (align == "left") start = n - len, end = n - 1;

   res = ends(A, NA, res, align, start, end, len, cap);

  // For align = 'centre' need to deal with both ends
  if (align == "centre") {
    start = 0, end = floor(len / 2);
    align = "right";
    res = ends(A, NA, res, align, start, end, len, cap);

    align = "left";
    start = n - floor(len / 2), end = n - 1;
    res = ends(A, NA, res, align, start, end, len, cap);
  }

  return(res);
}

// function to deal with ends where there is < len records
// This makes sure data capture threshold is still taken into account
NumericVector ends(NumericVector A, LogicalVector NA, NumericVector res, std::string
  	   align, int start, int end, double lenr, double capr) {

  double sum = 0.0; // sum of values over window
  double sumNA = 0; // number of missings
  int nd = 0; //count of data points


  if (align == "right") {

    for (int i = start; i <= end; i++) {

      sumNA = end - start - i + 1; //missing because < len
      sum = 0.0;
      nd = 0;

      for (int j = start; j <= i; j++) {

	if (NA(j)) {
	  sumNA += 1; // count missings
	}
	else
	  {
	    nd +=1;
	    sum += A(j); // sum values that are not missing
	  }
      }

      if (1 - sumNA / lenr < capr / 100) {
	res(i) = NA_REAL;
      }
      else
	{
	  res(i) = sum / nd;
	}
    }
  }

  // left align deals with the end of the data
  if (align == "left") {

    for (int i = end; i >= start; i--) {

      sumNA = i - start + 1; //missing because < len
      sum = 0.0;
      nd = 0;

      for (int j = end; j >= i; j--) {

	if (NA(j)) {
	  sumNA += 1; // count missings
	}
	else
	  {
	    nd +=1;
	    sum += A(j); // sum values that are not missing
	  }
      }

      if (1 - sumNA / lenr < capr / 100) {
	res(i) = NA_REAL;
      }
      else
	{
	  res(i) = sum / nd;
	}
    }
  }
  return(res);
}
