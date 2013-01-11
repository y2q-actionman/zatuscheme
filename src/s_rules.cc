#include <memory>
#include <algorithm>
#include <iterator>
#include <unordered_set>

#include "s_rules.hh"
#include "s_closure.hh"
#include "cons_util.hh"
#include "env.hh"
#include "zs_error.hh"
#include "builtin_equal.hh"
#include "builtin_extra.hh"
#include "builtin_util.hh"
#include "printer.hh"

// #include <iostream>
// #include "env.hh"

using namespace std;

namespace Procedure{

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

void check_pattern(const SyntaxRules& sr, Lisp_ptr p,
                   unordered_set<Lisp_ptr> tab){
  if(identifierp(p)){
    if(find(begin(sr.literals()), end(sr.literals()), p) != end(sr.literals())){
      // literal identifier
      return;
    }
      
    // pattern variable
    if(tab.find(p) != tab.end()){
      throw zs_error("syntax-rules error: duplicated pattern variable! (%s)\n",
                     identifier_symbol(p)->name().c_str());
    }
    tab.insert(p);
    return;
  }else if(p.tag() == Ptr_tag::cons){
    if(nullp(p)) return;

    auto i = begin(p);

    for(; i; ++i){
      check_pattern(sr, *i, tab);
    }

    check_pattern(sr, i.base(), tab);
  }else if(p.tag() == Ptr_tag::vector){
    auto v = p.get<Vector*>();

    for(auto i : *v){
      check_pattern(sr, i, tab);
    }
  }else{
    return;
  }
}

void check_pattern(const SyntaxRules& sr, Lisp_ptr p){
  pick_first(p);
  check_pattern(sr, p, {});
}

} // namespace


constexpr ProcInfo SyntaxRules::sr_procinfo;

SyntaxRules::SyntaxRules(Env* e, Lisp_ptr lits, Lisp_ptr rls)
  : env_(e), literals_(), rules_(){
  for(auto i : lits){
    if(!identifierp(i))
      throw builtin_identifier_check_failed("syntax-rules", i);

    literals_.push_back(i);
  }
  literals_.shrink_to_fit();

  for(auto i : rls){
    bind_cons_list_strict
      (i,
       [&](Lisp_ptr pat, Lisp_ptr tmpl){
        check_pattern(*this, pat);
        rules_.push_back({pat, tmpl});
      });
  }
  rules_.shrink_to_fit();
}

SyntaxRules::~SyntaxRules() = default;

static
bool try_match_1(const SyntaxRules& sr, Lisp_ptr ignore_ident,
                 Env* env, Lisp_ptr pattern, 
                 Env* form_env, Lisp_ptr form){

  // cout << __func__ << endl;
  // cout << "pattern " << pattern << ", form " << form << endl;
    
  const auto ellipsis_sym = intern(vm.symtable(), "...");

  if(identifierp(pattern)){
    if(find(begin(sr.literals()), end(sr.literals()), pattern) != end(sr.literals())){
      // literal identifier
      if(!identifierp(form)) return false;

      return identifier_eq(env, pattern, form_env, form);
    }else{
      // non-literal identifier
      if(pattern != ignore_ident){
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

    for(; p_i && (p_i != p_e); ++p_i, (f_i ? ++f_i : f_i)){
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

        env->local_set(*p_i, f_i.base());
        return true;
      }

      if(f_i == f_e) break; // this check is delayed to here, for checking the ellipsis.

      if(!try_match_1(sr, ignore_ident, env, *p_i, form_env, *f_i)){
        return false;
      }
    }

    // checks length
    if((p_i == p_e) && (f_i == f_e)){
      return try_match_1(sr, ignore_ident, env, p_i.base(), form_env, f_i.base());
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

        env->local_set(*p_i, new Vector(f_i, f_e));
        return true;
      }

      if(f_i == f_e) break; // this check is delayed to here, for checking the ellipsis.

      if(!try_match_1(sr, ignore_ident, env, *p_i, form_env, *f_i)){
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

std::pair<Env*, Lisp_ptr> match(const SyntaxRules& sr, Lisp_ptr form, Env* form_env){
  static constexpr auto env_cleaner = [](Env* e){
    for(auto i : e->internal_map()){
      if(auto sc = i.second.get<SyntacticClosure*>()){
        delete sc;
      }
    }
    e->internal_map().clear();
  };

  static constexpr auto env_deleter = [env_cleaner](Env* e){
    env_cleaner(e);
    delete e; 
  };

  unique_ptr<Env, decltype(env_deleter)> env{sr.env()->push(), env_deleter};

  for(auto i : sr.rules()){
    auto ignore_ident = pick_first(i.first);
    if(try_match_1(sr, ignore_ident, env.get(), i.first, form_env, form)){
      return {env.release(), i.second};
    }else{
      env_cleaner(env.get());
    }
  }

  throw zs_error("syntax-rules error: no matching pattern found!\n");
}

} // Procedure
