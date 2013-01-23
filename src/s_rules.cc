#include <memory>
#include <algorithm>
#include <iterator>
#include <unordered_set>
#include <unordered_map>
#include <utility>

#include "s_rules.hh"
#include "s_closure.hh"
#include "cons_util.hh"
#include "env.hh"
#include "zs_error.hh"
#include "builtin_equal.hh"
#include "builtin_extra.hh"
#include "builtin_util.hh"
#include "printer.hh"
#include "hasher.hh"
#include "eval.hh"

#ifndef NDEBUG
#include <iostream>
#endif

using namespace std;

typedef std::unordered_set<Lisp_ptr, eq_hash_obj, eq_obj> MatchSet;
typedef std::unordered_set<Lisp_ptr, eq_id_hash_obj, eq_id_obj> ExpandSet;

namespace Procedure{

namespace {

void push_tail_cons_list_nl(Lisp_ptr p, Lisp_ptr value){
  if(p.tag() != Ptr_tag::cons)
    throw zs_error("internal %s: the passed list is a dotted list!\n", __func__);

  auto c = p.get<Cons*>();
  if(!c)
    throw zs_error("internal %s: the passed list is an empty list!\n", __func__);

  if(nullp(c->cdr())){
    c->rplacd(make_cons_list({value}));
  }else{
    push_tail_cons_list_nl(c->cdr(), value);
  }
}

void push_tail_cons_list(Lisp_ptr* p, Lisp_ptr value){
  if(nullp(*p)){
    *p = make_cons_list({value});
  }else{
    push_tail_cons_list_nl(*p, value);
  }
}

Lisp_ptr nthcdr_cons_list(Lisp_ptr p, unsigned n){
  ConsIter ci = begin(p);
  std::advance(ci, n);
  return ci.base();
}

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

void check_pattern(const SyntaxRules& sr, Lisp_ptr p, MatchSet tab){
  if(identifierp(p)){
    for(auto l : sr.literals()){
      if(eq_internal(l, p)){
        // literal identifier
        return;
      }
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
  : env_(e), literals_(lits), rules_(rls){
  for(auto i : lits){
    if(!identifierp(i))
      throw builtin_identifier_check_failed("syntax-rules", i);
  }

  for(auto i : rls){
    bind_cons_list_strict
      (i,
       [&](Lisp_ptr pat, Lisp_ptr tmpl){
        (void)tmpl;
        check_pattern(*this, pat);
      });
  }
}

SyntaxRules::~SyntaxRules() = default;

template<typename DefaultGen>
static
void ensure_binding(EqHashMap& match_obj,
                    const SyntaxRules& sr, Lisp_ptr ignore_ident, Lisp_ptr pattern,
                    const DefaultGen& default_gen_func){
  const auto ellipsis_sym = intern(vm.symtable(), "...");

  if(identifierp(pattern)){
    for(auto l : sr.literals()){
      if(eq_internal(l, pattern)){
        return; // literal identifier
      }
    }
    // non-literal identifier
    if(!identifier_eq(sr.env(), ignore_ident, sr.env(), pattern)){
      match_obj.insert({pattern, default_gen_func()});
    }
    return;
  }else if(pattern.tag() == Ptr_tag::cons){
    if(nullp(pattern)){
      return;
    }

    auto p_i = begin(pattern);

    for(; p_i; ++p_i){
      // checks ellipsis
      if(identifierp(*p_i) && identifier_symbol(*p_i) == ellipsis_sym){
        return;
      }
      
      ensure_binding(match_obj, sr, ignore_ident, *p_i, default_gen_func);
    }

    // checks length
    if((p_i.base().tag() == Ptr_tag::cons)){
      return;
    }else{
      // dotted list case
      ensure_binding(match_obj, sr, ignore_ident, p_i.base(), default_gen_func);
      return;
    }
  }else if(pattern.tag() == Ptr_tag::vector){
    auto p_v = pattern.get<Vector*>();
    auto p_i = begin(*p_v), p_e = end(*p_v);

    for(; p_i != p_e; ++p_i){
      ensure_binding(match_obj, sr, ignore_ident, *p_i, default_gen_func);
    }
  }else{
    return;
  }
  
}

static
pair<EqHashMap, bool>
try_match_1(const SyntaxRules& sr, Lisp_ptr ignore_ident, Lisp_ptr pattern, 
            Env* form_env, Lisp_ptr form){
  const auto ellipsis_sym = intern(vm.symtable(), "...");

  if(identifierp(pattern)){
    for(auto l : sr.literals()){
      if(eq_internal(l, pattern)){
        // literal identifier
        if(!identifierp(form)
           || !identifier_eq(sr.env(), pattern, form_env, form)){
          return {{}, false};
        }

        return {{}, true};
      }
    }

    // non-literal identifier
    EqHashMap match_obj;

    if(!identifier_eq(sr.env(), ignore_ident, sr.env(), pattern)){
      match_obj.insert({pattern, form});
    }
    return {match_obj, true};
  }else if(pattern.tag() == Ptr_tag::cons){
    if(form.tag() != Ptr_tag::cons){
      return {{}, false};
    }

    if(nullp(pattern) && nullp(form)){
      return {{}, true};
    }

    EqHashMap match_obj;

    auto p_i = begin(pattern);
    auto f_i = begin(form);

    for(; p_i; ++p_i, (f_i ? ++f_i : f_i)){
      // checks ellipsis
      auto p_n = next(p_i);
      if((p_n) && identifierp(*p_n) && identifier_symbol(*p_n) == ellipsis_sym){
        if(eq_internal(*p_i, ignore_ident)){
          throw zs_error("syntax-rules error: '...' is appeared following the first identifier.\n");
        }

        auto p_e = p_i;
        while(p_e) ++p_e;

        if(!nullp(p_e.base())){
          throw zs_error("syntax-rules error: '...' is appeared in a inproper list pattern!\n");
        }

        // accumulating...
        EqHashMap acc_map;
        ensure_binding(acc_map, sr, ignore_ident, *p_i,
                       [](){ return Cons::NIL; });

        for(; f_i; ++f_i){
          auto m = try_match_1(sr, ignore_ident, *p_i, form_env, *f_i);
          if(!m.second){
            return {{}, false};
          }

          for(auto i : m.first){
            auto place = acc_map.find(i.first);
            assert(place->second.tag() == Ptr_tag::cons);
            push_tail_cons_list(&place->second, i.second);
          }
        }

        if(!nullp(f_i.base())){
          throw zs_error("syntax-rules error: '...' is used for a inproper list form!\n");
        }

        match_obj.insert(begin(acc_map), end(acc_map));
        return {match_obj, true};
      }

      if(!f_i) break; // this check is delayed to here, for checking the ellipsis.

      auto m = try_match_1(sr, ignore_ident, *p_i, form_env, *f_i);
      if(!m.second){
        return {{}, false};
      }

      match_obj.insert(begin(m.first), end(m.first));
    }

    // checks length
    if((p_i.base().tag() == Ptr_tag::cons) && (f_i.base().tag() == Ptr_tag::cons)){
      // proper list
      return {match_obj, (nullp(p_i.base()) && nullp(f_i.base()))};
    }else{
      // dotted list
      auto m = try_match_1(sr, ignore_ident, p_i.base(), form_env, f_i.base());
      if(!m.second){
        return {{}, false};
      }
      match_obj.insert(begin(m.first), end(m.first));
      return {match_obj, true};
    }
  // }else if(pattern.tag() == Ptr_tag::vector){
  //   if(form.tag() != Ptr_tag::vector){
  //     return false;
  //   }

  //   auto p_v = pattern.get<Vector*>();
  //   auto f_v = form.get<Vector*>();

  //   auto p_i = begin(*p_v), p_e = end(*p_v);
  //   auto f_i = begin(*f_v), f_e = end(*f_v);

  //   for(; p_i != p_e; ++p_i, ++f_i){
  //     // checks ellipsis
  //     auto p_n = next(p_i);

  //     if((p_n != p_e) && identifierp(*p_n) && identifier_symbol(*p_n) == ellipsis_sym){
  //       if(eq_internal(*p_i, ignore_ident)){
  //         throw zs_error("syntax-rules error: '...' is appeared following the first identifier.\n");
  //       }

  //       // accumulating...
  //       ensure_binding(match_obj, sr, ignore_ident, *p_i);
  //       for(; f_i != f_e; ++f_i){
  //         if(!try_match_1(match_obj, sr, ignore_ident, *p_i, form_env, *f_i, true)){
  //           return false;
  //         }
  //       }

  //       return true;
  //     }

  //     if(f_i == f_e) break; // this check is delayed to here, for checking the ellipsis.

  //     if(!try_match_1(match_obj, sr, ignore_ident, *p_i, form_env, *f_i)){
  //       return false;
  //     }
  //   }

  //   // checks length
  //   if((p_i == p_e) && (f_i == f_e)){
  //     return true;
  //   }else{
  //     return false;
  //   }
  }else{
    return {{}, equal_internal(pattern, form)};
  }
}


static
Lisp_ptr close_to_pattern_variable(ExpandSet& expand_obj,
                                   const EqHashMap& match_obj, const SyntaxRules& sr,
                                   Env* form_env, Lisp_ptr form){
  if(is_self_evaluating(form)){
    return form;
  }else if(form.tag() == Ptr_tag::symbol){
    for(auto l : sr.literals()){
      if(eq_internal(l, form)){
        return form;
      }
    }

    auto new_sc = new SyntacticClosure(form_env, nullptr, form);
    auto iter = expand_obj.find(new_sc);
    if(iter == expand_obj.end()){
      expand_obj.insert(new_sc);
      return new_sc;
    }else{
      delete new_sc;
      return *iter;
    }
  }else if(form.tag() == Ptr_tag::cons){
    GrowList gl;
    for(auto i : form){
      gl.push(close_to_pattern_variable(expand_obj, match_obj, sr, form_env, i));
    }
    return gl.extract();
  }else if(form.tag() == Ptr_tag::vm_op){
    UNEXP_DEFAULT();
  }else if(form.tag() == Ptr_tag::syntactic_closure){
    return form;
  }else{
    UNEXP_DEFAULT();
  }
}

static
EqHashMap remake_matchobj(const EqHashMap& match_obj, int pick_depth){
  EqHashMap ret;

  for(auto i : match_obj){
    if(i.second.tag() == Ptr_tag::vector){
      auto v = i.second.get<Vector*>();

      if(pick_depth < static_cast<signed>(v->size())){
        ret.insert({i.first, (*v)[pick_depth]});
        continue;
      }
    }else if(i.second.tag() == Ptr_tag::cons){
      auto nth = nthcdr_cons_list(i.second, pick_depth);
      if(!nullp(nth)){
        ret.insert({i.first, nth.get<Cons*>()->car()});
        continue;
      }
    }

    ret.insert({i.first, {}});
  }

  return ret;
}

static
pair<Lisp_ptr, bool> expand(ExpandSet& expand_obj,
                            const EqHashMap& match_obj, 
                            const SyntaxRules& sr,
                            Lisp_ptr tmpl,
                            bool pick_limit_ok){
  const auto ellipsis_sym = intern(vm.symtable(), "...");

  if(identifierp(tmpl)){
    auto m_ret = match_obj.find(tmpl);
    if(m_ret != match_obj.end()){
      if(m_ret->second){
        return {m_ret->second, pick_limit_ok};
      }else{
        return {{}, false};
      }
    }else{
      return {close_to_pattern_variable(expand_obj, match_obj, sr, sr.env(), tmpl), pick_limit_ok};
    }
  }else if(tmpl.tag() == Ptr_tag::cons){
    if(nullp(tmpl)) return {tmpl, pick_limit_ok};

    GrowList gl;
    auto t_i = begin(tmpl);

    for(; t_i; ++t_i){
      auto t_n = next(t_i);

      // check ellipsis
      if((t_n) && identifierp(*t_n) && identifier_symbol(*t_n) == ellipsis_sym){
        int depth = 0;
        while(1){
          auto emap = remake_matchobj(match_obj, depth);
          auto ex = expand(expand_obj, emap, sr, *t_i, true);
          if(!ex.second){
            break;
          }

          gl.push(ex.first);
          ++depth;
        }

        ++t_i;
      }else{
        auto ex = expand(expand_obj, match_obj, sr, *t_i, true);
        if(!ex.second){
          return {{}, false};
        }
        gl.push(ex.first);
      }
    }

    auto ex = expand(expand_obj, match_obj, sr, t_i.base(), true);
    if(!ex.second){
      return {{}, false};
    }

    auto l = gl.extract_with_tail(ex.first);
    return {l, pick_limit_ok};
  // }else if(tmpl.tag() == Ptr_tag::vector){
  //   auto t_vec = tmpl.get<Vector*>();

  //   Vector vec;

  //   for(auto t_i = begin(*t_vec), t_e = end(*t_vec); t_i != t_e; ++t_i){
  //     auto t_n = next(t_i);

  //     // check ellipsis
  //     if(identifierp(*t_i)
  //        && (t_n != t_e) && identifierp(*t_n)
  //        && identifier_symbol(*t_n) == ellipsis_sym){
  //       auto m_ret = match_obj.find(*t_i);
        
  //       if(m_ret == match_obj.end()){
  //         throw zs_error("syntax-rules error: invalid template: followed by '...', but not bound by pattern\n");
  //       }

  //       if(m_ret->second.tag() == Ptr_tag::vector){
  //         auto m_ret_vec = m_ret->second.get<Vector*>();
  //         vec.insert(vec.end(), begin(*m_ret_vec), end(*m_ret_vec));
  //       }else{
  //         throw zs_error("syntax-rules error: invalid template: matched pattern variable is not followed by ...\n");
  //       }

  //       ++t_i;
  //     }else{
  //       vec.push_back(expand(expand_obj, match_obj, *t_i));
  //     }
  //   }

  //   return new Vector(move(vec));
  }else{
    return {tmpl, pick_limit_ok};
  }
}

Lisp_ptr SyntaxRules::apply(Lisp_ptr form, Env* orig_form_env) const{
  Env* form_env = orig_form_env->push(); // TODO: stop leak when exception.

#ifndef NDEBUG
  cout << "## " << __func__ << ": form = " << form << ", env = " << form_env << '\n';
  // if(dump_mode)
  //   cout << *vm.frame();
#endif

  for(auto i : this->rules()){
    auto pat = i.get<Cons*>()->car();
    auto tmpl = i.get<Cons*>()->cdr().get<Cons*>()->car();

#ifndef NDEBUG
    cout << "## trying: pattern = " << pat << '\n';
#endif

    auto ignore_ident = pick_first(pat);
    auto match_ret = try_match_1(*this, ignore_ident, pat, form_env, form);
    if(match_ret.second){
#ifndef NDEBUG
      cout << "## matched!:\tpattern = " << pat << '\n';
      cout << "## \t\ttemplate = " << tmpl << '\n';
      cout << "## \t\tform = " << form << '\n';
      for(auto ii : match_ret.first){
        cout << "## env\t" << ii.first << " = " << ii.second << '\n';
      }
#endif
      
      ExpandSet expand_obj;
      auto ex = expand(expand_obj, match_ret.first, *this, tmpl, false);
#ifndef NDEBUG
      cout << "## expand = " << ex.first << '\n';
      cout << endl;
#endif
      return ex.first;
    }else{
      // cleaning map ?
    }
  }

#ifndef NDEBUG
  cout << "## no match: form = " << form << endl;
#endif
  throw zs_error("syntax-rules error: no matching pattern found!\n");
}

} // Procedure
