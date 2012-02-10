#include <string>
#include <sstream>
#include <utility>
#include <cstdlib>
#include <memory>
#include <cstdio>

#include "number.hh"

using namespace std;

static bool result;

template<typename Fun>
void fail_message(Number::Type t, istream& i, 
                  const Number& n, const Fun& callback){

  { // extract input from stream
    i.clear(); // clear eof

    const auto now_pos = i.tellg();
    const auto size = now_pos + static_cast<streamoff>(1);
    const unique_ptr<char[]> tmp(new char[size]);

    i.seekg(0, ios_base::beg);
    i.get(tmp.get(), size);

    fprintf(stdout, "[failed] input='%s', expect type='",
            tmp.get());
  }

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


void check_uninit(istream& i){
  check_generic<Number::Type::uninitialized>
    (i, [](){});
}

void check_uninit(const string& input){
  stringstream is(input);
  return check_uninit(is);
}

void check_int(istream& i, long expect){
  check_generic<Number::Type::integer>
    (i, expect,
     [=](){
      fprintf(stdout, ", expected int='%ld'", expect);
    });
}

void check_int(const string& input, long expect){
  stringstream is(input);
  return check_int(is, expect);
}

void check_real(istream& i, double expect){
  check_generic<Number::Type::real>
    (i, expect,
     [=](){
      fprintf(stdout, ", expected real='%f'", expect);
    });
}

void check_real(const string& input, double expect){
  stringstream is(input);
  return check_real(is, expect);
}

void check_complex(istream& i, const Number::complex_type& z){
  check_generic<Number::Type::complex>
    (i, z,
     [=](){
      fprintf(stdout, ", expected complex='(%f %f)'",
              z.real(), z.imag());
    });
}

void check_complex(const string& input, const Number::complex_type& z){
  stringstream is(input);
  return check_complex(is, z);
}


int main(){
  result = true;

  // invalids
  check_uninit("hogehoge");
  check_uninit(".");

  // int
  check_int("100", 100);
  check_int("-100", -100);
  check_int("1##", 100);

  check_int("#b10", 2);
  check_int("#o10", 8);
  check_int("#x10", 16);
  check_int("#x9abcdef", 0x9abcdef);

  // float
  check_real("-1.1", -1.1);
  check_real("1.", 1.0);
  check_real(".1", 0.1);
  check_real("3.14159265358979e0", 3.14159265358979e0);
  check_real("0.6s0", 0.6e0);
  check_real(".1f10", 0.1e10);
  check_real("3.d2", 3e2);
  check_real("3#.l-3", 30e-3);


  check_uninit("#b1.0");
  check_uninit("#o1.0");
  check_uninit("#x1.0");
  check_real("#d1.0", 1.0);

  // complex
  check_complex("1.0+1i", Number::complex_type(1, 1));
  check_complex("-2.5+0.0i", Number::complex_type(-2.5, 0));
  check_complex("1.0@3", polar(1.0, 3.0));
  check_uninit("1.0i");
  check_complex("+1.0i", Number::complex_type(0, 1.0));

  // prefix
  check_int("#e1", 1);
  check_real("#i1", 1.0);
  check_uninit("#e1.0");
  check_real("#i1.0", 1.0);
  check_uninit("#e1.0i");
  check_complex("#i-1.0i", Number::complex_type(0, -1.0));

  check_int("#o#e10", 8);
  check_real("#i#x10", 16.0);

  check_uninit("#x#x10");
  check_uninit("#i#e1");


  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

