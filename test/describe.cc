#include "describe.hh"
#include <ostream>

using namespace std;

std::ostream& operator<<(std::ostream& o, Ptr_tag t){
  return (o << stringify(t));
}

// std::ostream& operator<<(std::ostream& o, Lisp_ptr p){
//   return (o << "[" << p.tag() << "] " << p.get<void*>());
// }

std::ostream& operator<<(std::ostream& o, const ProcInfo& info){
  return (o << "[required_args=" << info.required_args << ", max_args=" << info.max_args << "]");
}

std::ostream& operator<<(std::ostream& o, proc_flag::Variadic v){
  return (o << boolalpha << static_cast<bool>(v) << noboolalpha);
}

std::ostream& operator<<(std::ostream& o, Notation n){
  return (o << stringify(n));
}
