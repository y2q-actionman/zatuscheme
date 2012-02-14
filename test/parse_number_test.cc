#include <string>
#include <sstream>
#include <utility>
#include <cstdlib>
#include <memory>
#include <cstdio>

#include "number.hh"

#define PRINT_BUFSIZE 100

using namespace std;

static bool result;

template<typename Fun>
void fail_message(Number::Type t, istream& i, 
                  const Number& n, const Fun& callback){
  // extract input from stream
  char buf[PRINT_BUFSIZE];

  i.clear(); // clear eof
  i.seekg(0, ios_base::beg);
  i.get(buf, sizeof(buf));

  fprintf(stdout, "[failed] input='%s', expect type='", buf);

  describe(stdout, t);
  fputc('\'', stdout);

  callback();

  fputs("\n\tgotten: ", stdout);
  describe(stdout, n);
  fputc('\n', stdout);

  result = false;
}

template<Number::Type type, typename Fun>
void check_generic(istream& i, const Fun& f){
  const Number n = parse_number(i);

  if(n.type() != type){
    fail_message(type, i, n, f);
    return;
  }
}

template<Number::Type type, typename Fun, 
         typename ex_type = typename to_type<Number::Type>::get<type>::type>
void check_generic(istream& i,
                   const ex_type& expect,
                   const Fun& f){
  const Number n = parse_number(i);

  if(n.type() != type || n.get<ex_type>() != expect){
    fail_message(type, i, n, f);
    return;
  }
}


void check(istream& i){
  check_generic<Number::Type::uninitialized>
    (i, [](){});
}

void check(const string& input){
  stringstream is(input);
  return check(is);
}

void check(istream& i, long expect){
  check_generic<Number::Type::integer>
    (i, expect,
     [=](){
      fprintf(stdout, ", expected int='%ld'", expect);
    });
}

void check(istream& i, double expect){
  check_generic<Number::Type::real>
    (i, expect,
     [=](){
      fprintf(stdout, ", expected real='%f'", expect);
    });
}

void check(istream& i, const Number::complex_type& z){
  check_generic<Number::Type::complex>
    (i, z,
     [=](){
      fprintf(stdout, ", expected complex='(%f %f)'",
              z.real(), z.imag());
    });
}

template<typename T>
void check(const string& input, T&& t){
  stringstream is(input);
  return check(is, t);
}


int main(){
  result = true;

  // invalids
  check("hogehoge");
  check(".");

  // int
  check("100", 100l);
  check("-100", -100l);
  check("1##", 100l);

  check("#b10", 2l);
  check("#o10", 8l);
  check("#x10", 16l);
  check("#x9abcdef", 0x9abcdefl);

  // float
  check("-1.1", -1.1);
  check("1.", 1.0);
  check(".1", 0.1);
  check("3.14159265358979e0", 3.14159265358979e0);
  check("0.6s0", 0.6e0);
  check(".1f10", 0.1e10);
  check("3.d2", 3e2);
  check("3#.l-3", 30e-3);


  check("#b1.0");
  check("#o1.0");
  check("#x1.0");
  check("#d1.0", 1.0);

  // complex
  check("1.0+1i", Number::complex_type(1, 1));
  check("-2.5+0.0i", Number::complex_type(-2.5, 0));
  check("1.0@3", polar(1.0, 3.0));
  check("1.0i");
  check("+1.0i", Number::complex_type(0, 1.0));

  // prefix
  check("#e1", 1l);
  check("#i1", 1.0);
  check("#e1.0");
  check("#i1.0", 1.0);
  check("#e1.0i");
  check("#i-1.0i", Number::complex_type(0, -1.0));

  check("#o#e10", 8l);
  check("#i#x10", 16.0);

  check("#x#x10");
  check("#i#e1");


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

