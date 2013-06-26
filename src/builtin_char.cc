#include <functional>
#include <cctype>

#include "builtin_char.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "zs_error.hh"

using namespace std;

namespace {

template<typename Fun>
Lisp_ptr char_pred(Lisp_ptr arg1, const Fun& fun){
  if(arg1.tag() != Ptr_tag::character){
    return Lisp_ptr{false};
  }

  return Lisp_ptr{fun(arg1.get<char>())};
}

template<typename Fun>
Lisp_ptr char_conversion(Lisp_ptr arg1, const Fun& fun){
  if(arg1.tag() != Ptr_tag::character){
    throw builtin_type_check_failed(Ptr_tag::character, arg1);
  }

  return Lisp_ptr{fun(arg1.get<char>())};
}
  
} // namespace

namespace builtin {

Lisp_ptr internal_char_casecmp(ZsArgs args){
  for(auto p : args){
    if(p.tag() != Ptr_tag::character){
      throw builtin_type_check_failed(Ptr_tag::character, p);
    }
  }
    
  return Lisp_ptr{Ptr_tag::integer,
      tolower(args[0].get<char>()) - tolower(args[1].get<char>())};
}

Lisp_ptr char_isalpha(ZsArgs args){
  return char_pred(args[0], [](char c) -> bool{ return std::isalpha(c); });
}

Lisp_ptr char_isdigit(ZsArgs args){
  return char_pred(args[0], [](char c) -> bool{ return std::isdigit(c); });
}

Lisp_ptr char_isspace(ZsArgs args){
  return char_pred(args[0], [](char c) -> bool{ return std::isspace(c); });
}

Lisp_ptr char_isupper(ZsArgs args){
  return char_pred(args[0], [](char c) -> bool{ return std::isupper(c); });
}

Lisp_ptr char_islower(ZsArgs args){
  return char_pred(args[0], [](char c) -> bool{ return std::islower(c); });
}


Lisp_ptr char_to_int(ZsArgs args){
  return char_conversion(args[0],
                         [](char c){
                           return Lisp_ptr(Ptr_tag::integer, c);
                         });
}

Lisp_ptr char_from_int(ZsArgs args){
  if(args[0].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed(Ptr_tag::integer, args[0]);
  }

  return Lisp_ptr{static_cast<char>(args[0].get<int>())};
}

Lisp_ptr char_toupper(ZsArgs args){
  return char_conversion(args[0],
                         [](char c){ return static_cast<char>(std::toupper(c)); });
}

Lisp_ptr char_tolower(ZsArgs args){
  return char_conversion(args[0],
                         [](char c){ return static_cast<char>(std::tolower(c)); });
}

} // namespace builtin
