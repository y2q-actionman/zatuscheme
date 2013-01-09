#include <memory>
#include <algorithm>
#include <iterator>
// #include <iostream>

#include "s_rules.hh"
#include "s_closure.hh"
#include "cons_util.hh"
#include "env.hh"
#include "zs_error.hh"
#include "builtin_equal.hh"
#include "builtin_extra.hh"
#include "builtin_util.hh"
#include "printer.hh"

using namespace std;

namespace {

Lisp_ptr pick_first(Lisp_ptr p){
  if(p.tag() == Ptr_tag::cons){
    auto c = p.get<Cons*>();
    if(!c) throw zs_error("syntax-rules: the pattern is empty list\n");

    return c->car();
  }else if(p.tag() == Ptr_tag::vector){
    auto v = p.get<Vector*>();
    assert(v);
    if(v->empty()) throw zs_error("syntax-rules: the pattern is empty vector\n");

    return (*v)[0];
  }else{
    throw zs_error("syntax-rules: informal pattern passed! (%s)\n", stringify(p.tag()));
  }
}

} // namespace

namespace Procedure{

constexpr ProcInfo SyntaxRules::sr_procinfo;

SyntaxRules::SyntaxRules(Env* e, Lisp_ptr lits, Lisp_ptr rules)
  : env_(e), literals_(), rules_(){
  for(auto i : lits){
    if(!identifierp(i))
      throw builtin_identifier_check_failed("syntax-rules", i);

    literals_.push_back(i);
  }
  literals_.shrink_to_fit();

  for(auto i : rules){
    bind_cons_list_strict
      (i,
       [&](Lisp_ptr pat, Lisp_ptr tmpl){
        // checks forms
        pick_first(pat);
        pick_first(tmpl);

        rules_.push_back({pat, tmpl});
      });
  }
  rules_.shrink_to_fit();
}

SyntaxRules::~SyntaxRules() = default;

std::pair<Env*, Lisp_ptr> SyntaxRules::match(Lisp_ptr form, Env* form_env) const{
  // check form
  pick_first(form);

  static constexpr auto env_cleaner = [](Env* e){
    e->visit_map([](Env::map_type& map){
        for(auto i : map){
          if(auto sc = i.second.get<SyntacticClosure*>()){
            delete sc;
          }
        }
        map.clear();
      });
  };

  static constexpr auto env_deleter = [&](Env* e){
    env_cleaner(e);
    delete e; 
  };

  unique_ptr<Env, decltype(env_deleter)> env{env_->push(), env_deleter};

  for(auto i : rules_){
    auto ignore_ident = pick_first(i.first);
    if(try_match_1(env.get(), i.first, form, form_env, ignore_ident)){
      return {env.release(), i.second};
    }else{
      env_cleaner(env.get());
    }
  }

  throw zs_error("syntax-rules error: no matching pattern found!\n");
}

bool SyntaxRules::try_match_1(Env* env, Lisp_ptr pattern, 
                              Lisp_ptr form, Env* form_env,
                              Lisp_ptr ignore_ident) const{
  static const auto ellipsis_sym = intern(vm.symtable(), "...");

  static const auto check_duplicate = [env](Lisp_ptr ident){
    assert(identifierp(ident));

    env->visit_map([&ident](const Env::map_type& map){
        if(map.find(ident) != map.end()){
          throw zs_error("syntax-rules error: duplicated pattern variable! (%s)\n",
                         identifier_symbol(ident)->name().c_str());
        }
      });
  };

  // cout << __func__ << endl;
  // cout << "pattern " << pattern << ", form " << form << endl;
    

  if(identifierp(pattern)){
    if(find(begin(literals_), end(literals_), pattern) != end(literals_)){
      // literal identifier
      if(!identifierp(form)) return false;

      return identifier_eq(this->env_, pattern, form_env, form);
    }else{
      // non-literal identifier
      if(pattern != ignore_ident){
        check_duplicate(pattern);
        env->local_set(pattern, new SyntacticClosure(form_env, nullptr, form));
      }
      return true;
    }
  }else if(pattern.tag() == Ptr_tag::cons){
    if(form.tag() != Ptr_tag::cons)
      return false;

    if(nullp(pattern) && nullp(form)){
      return true;
    }

    auto p_i = begin(pattern), p_e = end(pattern);
    auto f_i = begin(form), f_e = end(form);

    for(; p_i != p_e; ++p_i, (f_i ? ++f_i : f_i)){
      auto p_n = next(p_i);

      // checks ellipsis
      if(identifierp(*p_i)
         && (p_n != p_e) && identifierp(*p_n) 
         && identifier_symbol(*p_n) == ellipsis_sym){
        if(p_i == begin(pattern)){
          throw zs_error("syntax-rules error: '...' is appeared following the first identifier.\n");
        }

        if(!nullp(p_e.base())){
          throw zs_error("syntax-rules error: '...' is appeared in a inproper list pattern!\n");
        }

        if(!nullp(f_e.base())){
          throw zs_error("syntax-rules error: '...' is used for a inproper list form!\n");
        }

        check_duplicate(*p_i);
        env->local_set(*p_i, f_i.base());
        return true;
      }

      if(f_i == f_e) break; // this check is delayed to here, for checking the ellipsis.

      if(!try_match_1(env, *p_i, *f_i, form_env, ignore_ident)){
        return false;
      }
    }

    // checks length
    if((p_i == p_e) && (f_i == f_e)){
      return try_match_1(env, p_i.base(), f_i.base(), form_env, ignore_ident);
    }else{
      return false;
    }
  }else if(pattern.tag() == Ptr_tag::vector){
    if(form.tag() != Ptr_tag::vector)
      return false;

    auto p_v = pattern.get<Vector*>();
    auto f_v = form.get<Vector*>();

    auto p_i = begin(*p_v), p_e = end(*p_v);
    auto f_i = begin(*f_v), f_e = end(*f_v);

    for(; p_i != p_e; ++p_i, ++f_i){
      auto p_n = next(p_i);

      // checks ellipsis
      if(identifierp(*p_i)
         && (p_n != p_e) && identifierp(*p_n) 
         && identifier_symbol(*p_n) == ellipsis_sym){
        if(p_i == begin(*p_v)){
          throw zs_error("syntax-rules error: '...' is appeared following the first identifier.\n");
        }

        check_duplicate(*p_i);
        env->local_set(*p_i, new Vector(f_i, f_e));
        return true;
      }

      if(f_i == f_e) break; // this check is delayed to here, for checking the ellipsis.

      if(!try_match_1(env, *p_i, *f_i, form_env, ignore_ident)){
        return false;
      }
    }

    // checks length
    if((p_i == p_e) && (f_i == f_e)){
      return true;
    }else{
      return false;
    }
  }else{
    return equal_internal(pattern, form);
  }
}

} // Procedure
