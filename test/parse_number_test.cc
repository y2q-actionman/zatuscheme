#include <string>
#include <cstdlib>
#include <sstream>

#include "number.hh"
#include "describe.hh"
#include "test_util.hh"

using namespace std;

static bool result;

template<typename T>
void fail_message(Number::Type t, istream& i, 
                  const Number& n, T expect){
  result = false;

  string buf;
  std::getline(i, buf);

  cerr << "[failed] input='" << buf << "', expect type='" << stringify(t) << "'";
  cerr << ", expected = '" << expect << "'";
  cerr << "\n\tgotten: ";
  describe(stdout, n);
  cerr << '\n';
}

void check(istream& i){
  static constexpr auto type = Number::Type::uninitialized;

  with_expect_error([&]() -> void {
      const Number n = parse_number(i);
      if(n.type() != type){
        fail_message(type, i, n, "(uninitialized)");
        return;
      }
    });
}

template<typename T>
void check(istream& i, const T& expect){
  static constexpr auto type = to_tag<Number::Type, T>();

  const Number n = parse_number(i);
  if(n.type() != type || n.get<T>() != expect){
    fail_message(type, i, n, expect);
    return;
  }
}


void check(const string& input){
  stringstream ss(input);
  check(ss);
}

template<typename T>
void check(const string& input, T&& t){
  stringstream ss(input);
  check(ss, t);
}

// printing test
void check(const Number& n, int radix, const char* expect){
  stringstream ss;
  print(ss, n, radix);

  auto evaled = ss.str();

  if(evaled != expect){
    cerr << "[failed] printed " << evaled << ", expected " << expect << "\n";
    result = false;
  }
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
  check("#b-10", -2l);
  check("#o10", 8l);
  check("#o-10", -8l);
  check("#x10", 16l);
  check("#x-10", -16l);
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
  check("#e1.0", 1l);
  check("#i1.0", 1.0);
  check("#e1.0i");
  check("#i-1.0i", Number::complex_type(0, -1.0));

  check("#o#e10", 8l);
  check("#i#x10", 16.0);

  check("#x#x10");
  check("#i#e1");

  // printing test
  check(Number(100l), 10, "100");
  check(Number(100l), 8, "#o144");
  check(Number(100l), 16, "#x64");
  check(Number(100l), 2, "#b1100100");

  check(Number(-100l), 10, "-100");
  check(Number(-100l), 8, "#o-144");
  check(Number(-100l), 16, "#x-64");
  check(Number(-100l), 2, "#b-1100100");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
