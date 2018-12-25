#include <iostream>
#include <cstring>
#include <vector>
#include <string>
#include <random>
#include <algorithm>
#include <omp.h>

int main(int argc, const char *argv[]) {

  //Here we declare our main vars
  const int N = 10000000;
  const double T = 1;
  const int r = 0;
  const int K = 100;
  const double s = 0.5;
  const int S0 = 100;
  const int m = 1000;
  const double k=0.9;

  const double D = T / m;

  //Here we declare our vectors. Do not fill with zeroes, push_back will add more elems, and not overwrite
  std::vector<double> A;
  A.reserve(N);
  std::vector<double> kappa(21, 0);
  std::vector<double> U(21, 0);

  //Here we declare the formula of the mean, standard deviation, and size of distribution
  const double distMean = (r -  pow (s, 2.0) / 2) * D;
  const double distStdDev = s * pow(D, 0.5);
  const int distSize = 1000;


  //omp_set_nested(0);
  #pragma omp parallel for schedule(dynamic) num_threads(8)
  for (int i = N; i > 0; --i) {
    //setenv OMP_NESTED true;
    //Here we generate a random integer, we instantiate the Mersenne Twister 19937 generator, and we instantiate the normal distribution
    std::random_device randDev;
    std::mt19937 generator(randDev());
    std::normal_distribution<double> dist(distMean, distStdDev);

    //Here we create a vector of 1000 elems, with our normal distribution
    std::vector<double> X;
    X.reserve(distSize);
    #pragma omp parallel for num_threads(8)
    for (int m = distSize; m > 0; --m) {
      X.push_back(dist(generator));
    }

    //Here we Init cum sum vector to size 1000. If you don't do it... Well, Seg Fault 11 will knock on your door
    std::vector<double> XCumSum(1000);

    //Here we get the vector cum sum of X.
    std::partial_sum(X.begin(), X.end(), XCumSum.begin(), std::plus<double>());

    //Here we get the exponential of the vector cum sum
    std::vector<double> expCumSum;
    expCumSum.reserve(distSize);
    for (int m = distSize; m > 0; --m) {
      expCumSum.push_back( exp( XCumSum[m] ) );
    }

    //Here we find the mean of vector exp cum sum
    //double sumOfExpCumSum = std::accumulate(expCumSum.begin(), expCumSum.end(), 0.0);

    //double meanOfExpCumSum = sumOfExpCumSum / (expCumSum.size());
    //double meanOfExpCumSum = std::accumulate(expCumSum.begin(), expCumSum.end(), 0.0) / expCumSum.size();
    //cout << meanOfExpCumSum << endl;

    //Here we multiply k * S0 * meanOfExpCumSum, which is part 2 of the max equation
    //double maxPart2 = k * S0 * meanOfExpCumSum;
    //double maxPart2 = k * S0 * ( std::accumulate(expCumSum.begin(), expCumSum.end(), 0.0) / expCumSum.size() ) ;
    //cout << maxPart2 << endl;

    //Here we multiply S0 * with exp of the sum of vector X, which is part 1 of max the equation
    //double sum = std::accumulate(X.begin(), X.end(), 0.0);
    //double expOfSum = exp(sum);
    //double maxPart1 = S0 * expOfSum;
    //double maxPart1 = S0 * exp( std::accumulate(X.begin(), X.end(), 0.0) );

    //Here we to the max of substraction of part 2 and 3, and 0.
    //double max = std::max( (maxPart1 - maxPart2), 0.0 );
    //double result = exp(-r*T) * max;
    double result = exp( -r * T ) *
                    std::max( ( (S0 * exp( std::accumulate(X.begin(), X.end(), 0.0) ) ) -
                    ( k * S0 * ( std::accumulate(expCumSum.begin(), expCumSum.end(), 0.0) / expCumSum.size() )) ), 0.0 );


    //Here we push result to A
    A.push_back(result);

  }

  //Here we get sum the A vector, and get its mean
  float ASumMean = ( std::accumulate(A.begin(), A.end(), 0.0) ) / A.size();
  std::cout << ASumMean << std::endl;
  return ASumMean;
}
