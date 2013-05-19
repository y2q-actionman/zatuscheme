#include <vector>
#include <utility>
#include <functional>
#include <string>
#include <cstring>
#include <algorithm>

#include "builtin_string.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "eval.hh"
#include "zs_error.hh"
#include "cons_util.hh"
#include "zs_memory.hh"

using namespace std;

namespace {

template<typename Fun>
Lisp_ptr string_compare(Lisp_ptr arg1, Lisp_ptr arg2, Fun fun){
  if(arg1.tag() != Ptr_tag::string){
    throw builtin_type_check_failed(nullptr, Ptr_tag::string, arg1);
  }

  if(arg2.tag() != Ptr_tag::string){
    throw builtin_type_check_failed(nullptr, Ptr_tag::string, arg2);
  }

  return Lisp_ptr{fun(*arg1.get<String*>(),
                      *arg2.get<String*>())};
}

template<typename Fun>
struct ci_compare{
  inline bool operator()(const String& s1, const String& s2) const {
    static constexpr Fun fun;
    return fun(strcasecmp(s1.c_str(), s2.c_str()), 0);
  }
};

} // namespace

namespace builtin {

Lisp_ptr string_make(ZsArgs args){
  if(args[0].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed(nullptr, Ptr_tag::integer, args[0]);
  }
  auto char_count = args[0].get<int>();

  switch(args.size()){
  case 1:
    return {zs_new<String>(char_count, '\0')};
  case 2: {
    auto c = args[1].get<char>();
    if(!c){
      throw builtin_type_check_failed(nullptr, Ptr_tag::character, args[1]);
    }
    return {zs_new<String>(char_count, c)};
  }
  default:
    throw builtin_argcount_failed(nullptr, 1, 2, args.size());
  }
}

Lisp_ptr string_string(ZsArgs args){
  String ret;
  for(auto p : args){
    auto c = p.get<char>();
    if(!c){
      throw builtin_type_check_failed(nullptr, Ptr_tag::character, p);
    }

    ret.push_back(c);
  }

  return {zs_new<String>(std::move(ret))};
}
  
Lisp_ptr string_length(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(nullptr, Ptr_tag::string, args[0]);
  }

  // TODO: add range check, and remove cast
  return Lisp_ptr{Ptr_tag::integer,
      static_cast<int>(str->length())};
}

Lisp_ptr string_ref(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(nullptr, Ptr_tag::string, args[0]);
  }

  if(args[1].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed(nullptr, Ptr_tag::integer, args[1]);
  }
  auto ind = args[1].get<int>();

  if(ind < 0 || ind >= static_cast<signed>(str->length())){
    throw zs_error(printf_string("index is out-of-bound ([0, %ld), supplied %d\n",
                                 str->length(), ind));
  }

  return Lisp_ptr{(*str)[ind]};
}

Lisp_ptr string_set(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(nullptr, Ptr_tag::string, args[0]);
  }

  if(args[1].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed(nullptr, Ptr_tag::integer, args[1]);
  }
  auto ind = args[1].get<int>();

  if(ind < 0 || ind >= static_cast<signed>(str->length())){
    throw zs_error(printf_string("index is out-of-bound ([0, %ld), supplied %d\n",
                                 str->length(), ind));
  }

  auto ch = args[2].get<char>();
  if(!ch){
    throw builtin_type_check_failed(nullptr, Ptr_tag::character, args[2]);
  }

  (*str)[ind] = ch;
  return Lisp_ptr{ch};
}


Lisp_ptr internal_string_strcmp(ZsArgs args){
  for(auto p : args){
    if(p.tag() != Ptr_tag::string){
      throw builtin_type_check_failed(nullptr, Ptr_tag::string, p);
    }
  }

  return Lisp_ptr{Ptr_tag::integer,
      args[0].get<String*>()->compare(*args[1].get<String*>())};
}

Lisp_ptr string_ci_equal(ZsArgs args){
  return string_compare(args[0], args[1],
                        ci_compare<std::equal_to<int> >());
}

Lisp_ptr string_ci_less(ZsArgs args){
  return string_compare(args[0], args[1],
                        ci_compare<std::less<int> >());
}

Lisp_ptr string_ci_greater(ZsArgs args){
  return string_compare(args[0], args[1],
                        ci_compare<std::greater<int> >());
}

Lisp_ptr string_ci_less_eq(ZsArgs args){
  return string_compare(args[0], args[1],
                        ci_compare<std::less_equal<int> >());
}

Lisp_ptr string_ci_greater_eq(ZsArgs args){
  return string_compare(args[0], args[1],
                        ci_compare<std::greater_equal<int> >());
}


Lisp_ptr string_substr(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(nullptr, Ptr_tag::string, args[0]);
  }

  int ind[2];

  for(int i = 1; i < 3; ++i){
    if(args[i].tag() != Ptr_tag::integer){
      throw builtin_type_check_failed(nullptr, Ptr_tag::integer, args[i]);
    }
    ind[i-1] = args[i].get<int>();
  }

  if(!(0 <= ind[0] && ind[0] <= ind[1] && ind[1] <= static_cast<signed>(str->length()))){
    throw zs_error(printf_string("index is out-of-bound ([0, %ld), supplied [%d, %d)\n",
                                 str->length(), ind[0], ind[1]));
  }

  return {zs_new<String>(str->substr(ind[0], ind[1] - ind[0]))};
}

Lisp_ptr string_append(ZsArgs args){
  String ret;

  for(auto p : args){
    auto str = p.get<String*>();
    if(!str){
      throw builtin_type_check_failed(nullptr, Ptr_tag::string, p);
    }

    ret.append(*str);
  }

  return {zs_new<String>(std::move(ret))};
}

Lisp_ptr string_to_list(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(nullptr, Ptr_tag::string, args[0]);
  }

  return make_cons_list(str->begin(), str->end());
}

Lisp_ptr string_from_list(ZsArgs args){
  if(args[0].tag() != Ptr_tag::cons){
    throw builtin_type_check_failed(nullptr, Ptr_tag::cons, args[0]);
  }

  String ret;
  
  for(auto p : args[0]){
    auto ch = p.get<char>();
    if(!ch){
      throw builtin_type_check_failed(nullptr, Ptr_tag::character, p);
    }
    ret.push_back(ch);
  }

  return {zs_new<String>(std::move(ret))};
}

} // namespace builtin
