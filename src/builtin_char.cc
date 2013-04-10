#include <functional>
#include <cctype>

#include "builtin_char.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "zs_error.hh"

using namespace std;

namespace {

zs_error char_type_check_failed(const char* func_name, Lisp_ptr p){
  return zs_error_arg1(func_name,
                       printf_string("arg is not %s!", stringify(Ptr_tag::character)),
                       {p});
}

template<typename Fun>
Lisp_ptr char_compare(Lisp_ptr arg1, Lisp_ptr arg2,
                      const char* name, const Fun& fun){
  if(arg1.tag() != Ptr_tag::character){
    throw char_type_check_failed(name, arg1);
  }

  if(arg2.tag() != Ptr_tag::character){
    throw char_type_check_failed(name, arg2);
  }

  return Lisp_ptr{fun(arg1.get<char>(), arg2.get<char>())};
}

template<typename Fun>
struct ci_comparator{
  inline bool operator()(char c1, char c2) const{
    static constexpr Fun fun;
    return fun(tolower(c1), tolower(c2));
  }
};

template<typename Fun>
Lisp_ptr char_pred(Lisp_ptr arg1, const Fun& fun){
  if(arg1.tag() != Ptr_tag::character){
    return Lisp_ptr{false};
  }

  return Lisp_ptr{fun(arg1.get<char>())};
}

template<typename Fun>
Lisp_ptr char_conversion(Lisp_ptr arg1, const char* name, const Fun& fun){
  if(arg1.tag() != Ptr_tag::character){
    throw char_type_check_failed(name, arg1);
  }

  return Lisp_ptr{fun(arg1.get<char>())};
}
  
} // namespace

namespace builtin {

Lisp_ptr char_eq(ZsArgs args){
  return char_compare(args[0], args[1], "char=?",
                      std::equal_to<char>());
}

Lisp_ptr char_less(ZsArgs args){
  return char_compare(args[0], args[1], "char<?",
                      std::less<char>());
}

Lisp_ptr char_greater(ZsArgs args){
  return char_compare(args[0], args[1], "char>?",
                      std::greater<char>());
}

Lisp_ptr char_less_eq(ZsArgs args){
  return char_compare(args[0], args[1], "char<=?",
                      std::less_equal<char>());
}

Lisp_ptr char_greater_eq(ZsArgs args){
  return char_compare(args[0], args[1], "char>=?",
                      std::greater_equal<char>());
}
  

Lisp_ptr char_ci_eq(ZsArgs args){
  return char_compare(args[0], args[1], "char-ci=?",
                      ci_comparator<std::equal_to<int> >());
}

Lisp_ptr char_ci_less(ZsArgs args){
  return char_compare(args[0], args[1], "char-ci<?",
                      ci_comparator<std::less<int> >());
}

Lisp_ptr char_ci_greater(ZsArgs args){
  return char_compare(args[0], args[1], "char-ci>?",
                      ci_comparator<std::greater<int> >());
}

Lisp_ptr char_ci_less_eq(ZsArgs args){
  return char_compare(args[0], args[1], "char-ci<=?",
                      ci_comparator<std::less_equal<int> >());
}

Lisp_ptr char_ci_greater_eq(ZsArgs args){
  return char_compare(args[0], args[1], "char-ci>=?",
                      ci_comparator<std::greater_equal<int> >());
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
  return char_conversion(args[0], "char->integer",
                         [](char c){
                           return Lisp_ptr(Ptr_tag::integer, c);
                         });
}

Lisp_ptr char_from_int(ZsArgs args){
  if(args[0].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed("integer->char", Ptr_tag::integer, args[0]);
  }

  return Lisp_ptr{static_cast<char>(args[0].get<int>())};
}

Lisp_ptr char_toupper(ZsArgs args){
  return char_conversion(args[0], "char-upcase",
                         [](char c){ return static_cast<char>(std::toupper(c)); });
}

Lisp_ptr char_tolower(ZsArgs args){
  return char_conversion(args[0], "char-downcase",
                         [](char c){ return static_cast<char>(std::tolower(c)); });
}

} // namespace builtin
