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
#include "builtin_extra.hh"
#include "builtin_util.hh"
#include "printer.hh"
#include "equality.hh"
#include "eval.hh"

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

bool is_ellipsis(Lisp_ptr p){
  if(!identifierp(p)) return false;

  auto sym = identifier_symbol(p);
  return sym->name() == "...";
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
      if(is_ellipsis(*p_i)){
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
      if((p_n) && is_ellipsis(*p_n)){
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
  }else if(pattern.tag() == Ptr_tag::vector){
    if(form.tag() != Ptr_tag::vector){
      return {{}, false};
    }

    EqHashMap match_obj;

    auto p_v = pattern.get<Vector*>();
    auto f_v = form.get<Vector*>();

    auto p_i = begin(*p_v), p_e = end(*p_v);
    auto f_i = begin(*f_v), f_e = end(*f_v);

    for(; p_i != p_e; ++p_i, ++f_i){
      // checks ellipsis
      auto p_n = next(p_i);
      if((p_n != p_e) && is_ellipsis(*p_n)){
        if(eq_internal(*p_i, ignore_ident)){
          throw zs_error("syntax-rules error: '...' is appeared following the first identifier.\n");
        }

        // accumulating...
        EqHashMap acc_map;
        ensure_binding(match_obj, sr, ignore_ident, *p_i,
                       [](){ return new Vector(); });

        for(; f_i != f_e; ++f_i){
          auto m = try_match_1(sr, ignore_ident, *p_i, form_env, *f_i);
          if(!m.second){
            return {{}, false};
          }

          for(auto i : m.first){
            auto place = acc_map.find(i.first);
            assert(place->second.tag() == Ptr_tag::vector);
            place->second.get<Vector*>()->push_back(i.second);
          }
        }

        match_obj.insert(begin(acc_map), end(acc_map));
        return {match_obj, true};
      }

      if(f_i == f_e) break; // this check is delayed to here, for checking the ellipsis.

      auto m = try_match_1(sr, ignore_ident, *p_i, form_env, *f_i);
      if(!m.second){
        return {{}, false};
      }

      match_obj.insert(begin(m.first), end(m.first));
    }

    // checks length
    if((p_i == p_e) && (f_i == f_e)){
      return {match_obj, true};
    }else{
      return {match_obj, false};
    }
  }else{
    return {{}, equal_internal(pattern, form)};
  }
}


static
Lisp_ptr close_to_pattern_variable(ExpandSet& expand_ctx,
                                   const SyntaxRules& sr,
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
    auto iter = expand_ctx.find(new_sc);
    if(iter == expand_ctx.end()){
      expand_ctx.insert(new_sc);
      return new_sc;
    }else{
      delete new_sc;
      return *iter;
    }
  }else if(form.tag() == Ptr_tag::cons){
    GrowList gl;
    for(auto i : form){
      gl.push(close_to_pattern_variable(expand_ctx, sr, form_env, i));
    }
    return gl.extract();
  }else if(form.tag() == Ptr_tag::vector){
    auto v = form.get<Vector*>();
    Vector* ret = new Vector();
    for(auto i : *v){
      ret->push_back(close_to_pattern_variable(expand_ctx, sr, form_env, i));
    }
    return {ret};
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
      ConsIter ci = begin(i.second);
      std::advance(ci, pick_depth);
      auto nthcdr = ci.base();

      if(!nullp(nthcdr)){
        ret.insert({i.first, nthcdr.get<Cons*>()->car()});
        continue;
      }
    }

    ret.insert({i.first, {}});
  }

  return ret;
}

static
pair<Lisp_ptr, bool> expand(ExpandSet& expand_ctx,
                            const EqHashMap& match_obj, 
                            const SyntaxRules& sr,
                            Lisp_ptr tmpl,
                            bool pick_limit_ok){
  if(identifierp(tmpl)){
    auto m_ret = match_obj.find(tmpl);
    if(m_ret != match_obj.end()){
      if(m_ret->second){
        return {m_ret->second, pick_limit_ok};
      }else{
        return {{}, false};
      }
    }else{
      return {close_to_pattern_variable(expand_ctx, sr, sr.env(), tmpl), pick_limit_ok};
    }
  }else if(tmpl.tag() == Ptr_tag::cons){
    if(nullp(tmpl)) return {tmpl, pick_limit_ok};

    GrowList gl;
    auto t_i = begin(tmpl);

    for(; t_i; ++t_i){
      auto t_n = next(t_i);

      // check ellipsis
      if((t_n) && is_ellipsis(*t_n)){
        int depth = 0;
        while(1){
          auto emap = remake_matchobj(match_obj, depth);
          auto ex = expand(expand_ctx, emap, sr, *t_i, true);
          if(!ex.second){
            break;
          }

          gl.push(ex.first);
          ++depth;
        }

        ++t_i;
      }else{
        auto ex = expand(expand_ctx, match_obj, sr, *t_i, true);
        if(!ex.second){
          return {{}, false};
        }
        gl.push(ex.first);
      }
    }

    auto ex = expand(expand_ctx, match_obj, sr, t_i.base(), true);
    if(!ex.second){
      return {{}, false};
    }

    auto l = gl.extract_with_tail(ex.first);
    return {l, pick_limit_ok};
  }else if(tmpl.tag() == Ptr_tag::vector){
    auto t_vec = tmpl.get<Vector*>();

    Vector vec;

    for(auto t_i = begin(*t_vec), t_e = end(*t_vec); t_i != t_e; ++t_i){
      auto t_n = next(t_i);

      // check ellipsis
      if((t_n != t_e) && is_ellipsis(*t_n)){
        int depth = 0;
        while(1){
          auto emap = remake_matchobj(match_obj, depth);
          auto ex = expand(expand_ctx, emap, sr, *t_i, true);
          if(!ex.second){
            break;
          }

          vec.push_back(ex.first);
          ++depth;
        }

        ++t_i;
      }else{
        auto ex = expand(expand_ctx, match_obj, sr, *t_i, true);
        if(!ex.second){
          return {{}, false};
        }
        vec.push_back(ex.first);
      }
    }

    return {new Vector(move(vec)), pick_limit_ok};
  }else{
    return {tmpl, pick_limit_ok};
  }
}

Lisp_ptr SyntaxRules::apply(Lisp_ptr form, Env* form_env) const{
  for(auto i : this->rules()){
    auto pat = nth_cons_list<0>(i);
    auto tmpl = nth_cons_list<1>(i);
    auto ignore_ident = pick_first(pat);

    auto match_ret = try_match_1(*this, ignore_ident, pat, form_env, form);
    if(match_ret.second){
      ExpandSet expand_ctx;
      auto ex = expand(expand_ctx, match_ret.first, *this, tmpl, false);
      return ex.first;
    }else{
      // cleaning map ?
    }
  }

  throw zs_error("syntax-rules error: no matching pattern found!\n");
}

} // Procedure
