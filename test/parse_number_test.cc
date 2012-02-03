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

void check_uninit(istream& i){
  const Number n = parse_number(i);

  if(n.type() != Number::Type::uninitialized){
    fail_message(Number::Type::uninitialized, i, n,
                 [](){});
    return;
  }
}

void check_uninit(const string& input){
  stringstream is(input);
  return check_uninit(is);
}

int main(){
  result = true;

  // invalids
  check_uninit("hogehoge");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

