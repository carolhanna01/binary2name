/*
  GNU Gama C++ library tests/matvec
  Copyright (C) 2017  Ales Cepek <cepek@gnu.org>

  This file is part of the GNU Gama C++ library tests/matvec
  
  GNU Gama is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.
  
  GNU Gama is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with GNU Gama.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <iostream>
#include <sstream>
#include <limits>
#include <matvec/symmat.h>
#include <matvec/sortvec.h>
#include <matvec/svd.h>
#include <matvec/matvec.h>


int main()
{
  using namespace GNU_gama;
  using namespace std;

  const double dbl_epsilon = std::numeric_limits<double>::epsilon();
  
  int result = 0;
  
  cout << "\n   Mat / Vec  .........   demo_001  matvec "
       << GNU_gama::matvec_version() << "\n"
          "------------------------------------------------------\n\n";  
  {

    Vec<> a1(4);
    a1.set_all(1.1);
    cout << trans(a1);
    a1 *= 2.0;
    cout << a1.dim() << "  ";
    Vec<>::const_iterator i=a1.begin();
    Vec<>::const_iterator e=a1.end();
    while (i != e) 
      cout << *i++ << " ";
    cout << "\n\n";

    Vec<> a2(4); a2(1) = 4.0; a2(2) = 3.0; a2(3) = 2.0; a2(4) = 1.0;
    Vec<> a3 = a1+a2;
    cout << 2.0*trans(a3) << endl;
    cout << endl;

    sort(a2);
    cout << "sort vec  "
         << trans(a2)
         << endl;
  }

  {
    cout << "   SVD   \n"
            "------------------------------------------------------\n\n";
    
    Mat<> A(5, 3);
    Vec<> b(5);

    try    // List initialisation for Mat<> and Vec<>
      {
        A = 
	  {{1.001, 0.006, 0.012},
	   {0.002, 1.007, 0.013},
	   {0.003, 0.008, 1.014},
	   {1.004, 0.009, 1.015},
	   {0.005, 1.011, 1.016}};
        
        b = {1.1, 1.9, 3.1, 4, 5.1};
      }
    catch (const Exception::matvec& e)
      {
        cout << e.what() << endl;
        return 1;
      }
    
    SVD<> svd(A);
    Vec<> x;
    svd.solve(b, x);

    TransVec<> x1 = trans(x);
    TransVec<> x2 = trans(inv(trans(A)*A)*(trans(A)*b));
    TransVec<> x3 = trans(inv(trans(A)*A)*trans(A)*b);
    TransVec<> x4 = (trans(b)* A)*inv(trans(A)*A);
    TransVec<> x5 = trans(b)*(A *inv(trans(A)*A));
    
    cout << "x = " << x1;
    cout << "    " << x2;
    cout << "    " << x3;
    cout << "    " << x4;
    cout << "    " << x5;
    cout << endl;

    if ((x1-x2).norm_Linf() > 100*dbl_epsilon) result++;
    if ((x1-x3).norm_Linf() > 100*dbl_epsilon) result++;
    if ((x1-x4).norm_Linf() > 100*dbl_epsilon) result++;
    if ((x1-x5).norm_Linf() > 100*dbl_epsilon) result++;
  }

  {
    cout << "   SymMat   \n"
            "------------------------------------------------------\n\n";

    Vec<>    b(3);
    SymMat<> T;  
    SymMat<> S;
    {
      std::istringstream inp
	("3  2.04 3.05 4.0 "
	 "3  2.1           "
	 "   0.4  3.1      " 
	 "   0.5  0.6  4.1 "
	 "3  1.9           "     
	 "   0.1  2.9      " 
	 "   0.2  0.3  3.9 ");

      inp >> b >> T >> S;
    }

    S *= 2.0;
    S = (1.0*S + S - S)*0.5;
    cout << S << endl << Square(trans(S)) << endl;
    
    cout << "Matrix + - *  "
         << Lower(Square(T + S) - (Square(T) + Square(S)))
          + Lower(Square(T - S) - (Square(T) - Square(S)))
          + Lower(Square(T * S) - (Square(T) * Square(S)))
         << endl;

    cout << "Vector *  "
         << ( trans(S*b - Square(S)*b) )
          + ( trans(b)*S - trans(b)*Square(S) )
         << endl;

    cout << "inv()  "
         << ( inv(S) - Upper((inv(Square(S)))) )
         << endl;

    Mat<> A;
    {
      std::istringstream inp(
          " 4 3  0.01 0.02 0.03"
          "      0.04 0.05 0.06"
          "      0.07 0.08 0.09"
          "      0.10 0.11 0.12");
      inp >> A;
    }
    cout << "Mat * SymMat  "
         << ( A*T  - A*Square(T) )
         <<endl;

    SymMat<> Ch  = S;
    Vec<>    rhs = b;
    Ch.cholDec();
    Ch.solve(rhs);
    TransVec<> err = trans(2.0*rhs - inv(S)*b - inv(Square(S))*b);
    cout << "Cholesky  "
         << Ch << endl
         << err;

    if (err.norm_Linf() > 100*dbl_epsilon) result++;
  }

  return result;
}
