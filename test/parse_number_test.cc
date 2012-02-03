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

  fputs("\n\tgotten token: ", stdout);
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


int main(){
  result = true;

  // invalids
  check_uninit("hogehoge");

  // int
  check_int("100", 100);
  check_int("-100", -100);

  // float
  check_real("1.1", strtod("1.1", nullptr));

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

