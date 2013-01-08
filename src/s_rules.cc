#include <memory>
#include <algorithm>

#include "s_rules.hh"
#include "s_closure.hh"
#include "cons_util.hh"
#include "env.hh"
#include "zs_error.hh"
#include "builtin_equal.hh"
#include "builtin_extra.hh"

using namespace std;

namespace {

bool check_form(Lisp_ptr p){
  return (p.tag() == Ptr_tag::cons && !nullp(p));
}

} // namespace

namespace Procedure{

constexpr ProcInfo SyntaxRules::sr_procinfo;

SyntaxRules::SyntaxRules(Env* e, Lisp_ptr lits, Lisp_ptr rules)
  : env_(e), literals_(), rules_(){
  for(auto i : lits){
    literals_.push_back(identifier_symbol(i));
  }
  literals_.shrink_to_fit();

  for(auto i : rules){
    bind_cons_list_strict
      (i,
       [&](Lisp_ptr pat, Lisp_ptr tmpl){
        if(!check_form(pat)){
          throw zs_error("syntax-rules: informal pattern passed! (%s)\n",
                         stringify(pat.tag()));
        }

        if(!check_form(tmpl)){
          throw zs_error("syntax-rules: informal template passed! (%s)\n",
                         stringify(tmpl.tag()));
        }

        rules_.push_back({pat, tmpl});
      });
  }
  rules_.shrink_to_fit();
}

SyntaxRules::~SyntaxRules() = default;

std::pair<Env*, Lisp_ptr> SyntaxRules::match(Lisp_ptr form, Env* form_env) const{
  if(!check_form(form)){
    throw zs_error("syntax-rules: informal form passed! (%s)\n",
                   stringify(form.tag()));
  }

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
    if(try_match_1(env.get(), i.first, form, form_env, true)){
      return {env.release(), i.second};
    }else{
      env_cleaner(env.get());
    }
  }

  throw zs_error("syntax-rules error: no matching pattern found!\n");
}

bool SyntaxRules::try_match_1(Env* env, Lisp_ptr pattern, 
                              Lisp_ptr form, Env* form_env, bool is_first) const{
  static Symbol* ellipsis_sym = intern(vm.symtable(), "...");

  // if(is_first){
  //   auto p_next = pattern.get<Cons*>()->cdr();
  //   if(p_next.tag() != Ptr_tag::cons){
  //     throw zs_error("syntax-rules error: informal pattern appeared (dot paur)\n");
  //   }

  //   auto f_next = form.get<Cons*>()->cdr();
  //   if(f_next.tag() != Ptr_tag::cons){
  //     throw zs_error("syntax-rules error: informal form appeared (dot pair)\n");
  //   }

  //   // ignore first match
  //   return try_match_1(env, p_next, f_next, form_env, false);
  // }

  if(identifierp(pattern)){
    auto p_sym = identifier_symbol(pattern);
    if(p_sym == ellipsis_sym){
      // ellipsis
      
    }else if(find(begin(literals_), end(literals_), p_sym) != end(literals_)){
      // literal identifier
      if(!identifierp(form)) return false;

      return proc_identifier_eq_internal(this->env_, p_sym,
                                         form_env, identifier_symbol(form));
    }else{
      // non-literal identifier
      if(env->traverse(p_sym, {})){
        throw zs_error("syntax-rules error: duplicated pattern variable! (%s)\n",
                       p_sym->name().c_str());
      }

      if(!is_first)
        env->traverse(p_sym, new SyntacticClosure(form_env, nullptr, form));
    }
  }else if(pattern.tag() == Ptr_tag::cons){
    if(nullp(pattern) && nullp(form)){
      if(is_first){
        throw zs_error("syntax-rules error: empty list was passed!\n");
      }        
      // matched as list
      return true;
    }


  // }else if(form.tag() == Ptr_tag::vector){
  //   ;
  }else{
    return equal_internal(pattern, form);
  }
}




} // Procedure
