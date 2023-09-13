#include <Rcpp.h>
using namespace Rcpp;

//' Example hello function
//' 
//' @details Creates a pointless two element mekiiiiiiii anjeeeeeeeenngggggg
//'
//' @export
//' @examples
//' rcpp_hello_world()
// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar", "lalalala" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}
