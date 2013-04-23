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
#include "printer.hh"
#include "equality.hh"
#include "eval.hh"

using namespace std;

namespace {

// internal types
typedef std::unordered_map<Lisp_ptr, Lisp_ptr, eq_hash_obj, eq_obj> EqHashMap;
typedef std::unordered_set<Lisp_ptr, eq_hash_obj, eq_obj> MatchSet;
typedef std::unordered_set<Lisp_ptr, eq_id_hash_obj, eq_id_obj> ExpandSet;

// error classes
struct try_match_failed{};
struct expand_failed {};

bool is_literal_identifier(const SyntaxRules& sr, Lisp_ptr p){
  for(auto l : sr.literals()){
    if(eq_internal(l, p)){
      return true;
    }
  }
  return false;
}

bool is_ellipsis(Lisp_ptr p){
  if(!identifierp(p)) return false;

  auto sym = identifier_symbol(p);
  return sym->name() == "...";
}

void push_tail_cons_list_nl(Lisp_ptr p, Lisp_ptr value){
  if(p.tag() != Ptr_tag::cons)
    throw zs_error(printf_string("internal %s: the passed list is a dotted list!\n", __func__));

  auto c = p.get<Cons*>();
  if(!c)
    throw zs_error(printf_string("internal %s: the passed list is an empty list!\n", __func__));

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
    if(!c) throw zs_error_arg1("syntax-rules", "the pattern is empty list");

    return c->car();
  }else if(p.tag() == Ptr_tag::vector){
    auto v = p.get<Vector*>();
    assert(v);
    if(v->empty()) throw zs_error_arg1("syntax-rules", "the pattern is empty vector");

    return (*v)[0];
  }else{
    throw zs_error_arg1("syntax-rules", "informal pattern passed!", {p});
  }
}

void check_pattern(const SyntaxRules& sr, Lisp_ptr p, MatchSet tab){
  if(identifierp(p)){
    if(is_literal_identifier(sr, p)){
      // literal identifier
      return;
    }
      
    // pattern variable
    if(tab.find(p) != tab.end()){
      throw zs_error_arg1("syntax-rules", "duplicated pattern variable!", {p});
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


template<typename DefaultGen>
void ensure_binding(EqHashMap& match_obj,
                    const SyntaxRules& sr, Lisp_ptr ignore_ident, Lisp_ptr pattern,
                    const DefaultGen& default_gen_func){
  if(identifierp(pattern)){
    if(is_literal_identifier(sr, pattern)){
      return; // literal identifier
    }

    // non-literal identifier
    if(!eq_internal(ignore_ident, pattern)){
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

    if(!nullp(p_i.base())){
      // dotted list case
      ensure_binding(match_obj, sr, ignore_ident, p_i.base(), default_gen_func);
    }
  }else if(pattern.tag() == Ptr_tag::vector){
    auto p_v = pattern.get<Vector*>();

    for(auto p : *p_v){
      ensure_binding(match_obj, sr, ignore_ident, p, default_gen_func);
    }
  }else{
    return;
  }
}

template<typename Iter, typename Fun1, typename Fun2>
EqHashMap
try_match_1_seq(const SyntaxRules& sr, Lisp_ptr ignore_ident,
                Iter pattern_begin, Iter pattern_end,
                Env* form_env,
                Iter form_begin, Iter form_end,
                Fun1 dotted_checker,
                Fun2 dotted_handler){
  EqHashMap match_obj;
  auto p_i = pattern_begin;
  auto f_i = form_begin;
  
  for(; p_i != pattern_end; ++p_i){
    // checks ellipsis
    auto p_n = next(p_i);
    if((p_n != pattern_end) && is_ellipsis(*p_n)){
      if(eq_internal(*p_i, ignore_ident)){
        throw zs_error_arg1("syntax-rules", "'...' is appeared following the first identifier");
      }

      if(dotted_checker(pattern_end)){
        throw zs_error_arg1("syntax-rules", "'...' is appeared in a inproper list pattern");
      }

      if(dotted_checker(form_end)){
        throw zs_error_arg1("syntax-rules", "'...' is used for a inproper list form");
      }

      // accumulating...
      // BUG: this code only accepts cons. vector is rejected.
      //      (commit adcd5e7f56298d1dddc777dd894762cf0d3ec92c)
      EqHashMap acc_map;
      ensure_binding(acc_map, sr, ignore_ident, *p_i,
                     [](){ return Cons::NIL; });

      for(; f_i != form_end; ++f_i){
        auto m = try_match_1(sr, ignore_ident, *p_i, form_env, *f_i);

        for(auto i : m){
          auto place = acc_map.find(i.first);
          assert(place->second.tag() == Ptr_tag::cons);
          push_tail_cons_list(&place->second, i.second);
        }
      }

      match_obj.insert(begin(acc_map), end(acc_map));
      return match_obj;
    }

    if(f_i == form_end) break; // this check is delayed to here, for checking the ellipsis.

    auto m = try_match_1(sr, ignore_ident, *p_i, form_env, *f_i);
    match_obj.insert(begin(m), end(m));

    assert(f_i != form_end);
    ++f_i;
  }
  assert((p_i == pattern_end) || (f_i == form_end));


  if(dotted_checker(p_i)){
    return dotted_handler(p_i, f_i, match_obj);
  }else if((p_i == pattern_end) && (f_i == form_end)){
    return match_obj;
  }else{
    throw try_match_failed();
  }
}


EqHashMap
try_match_1(const SyntaxRules& sr, Lisp_ptr ignore_ident, Lisp_ptr pattern, 
            Env* form_env, Lisp_ptr form){
  if(identifierp(pattern)){
    if(is_literal_identifier(sr, pattern)){
      // literal identifier
      if(identifierp(form) && identifier_eq(sr.env(), pattern, form_env, form)){
        return {};
      }else{
        throw try_match_failed();
      }
    }

    // non-literal identifier
    EqHashMap match_obj;

    if(!eq_internal(ignore_ident, pattern)){
      match_obj.insert({pattern, form});
    }
    return match_obj;
  }else if(pattern.tag() == Ptr_tag::cons){
    if(form.tag() != Ptr_tag::cons){
      throw try_match_failed();
    }

    return try_match_1_seq
      (sr, ignore_ident,
       begin(pattern), end(pattern),
       form_env,
       begin(form), end(form),
       [](ConsIter i){ return !nullp(i.base()); },
       [&](ConsIter p_i, ConsIter f_i, EqHashMap& match_obj) -> EqHashMap{
         if(p_i.base().tag() == Ptr_tag::cons){
          throw try_match_failed();
         }

         auto m = try_match_1(sr, ignore_ident, p_i.base(), form_env, f_i.base());
         match_obj.insert(begin(m), end(m));
         return match_obj;
      });
  }else if(pattern.tag() == Ptr_tag::vector){
    if(form.tag() != Ptr_tag::vector){
      throw try_match_failed();
    }

    auto p_v = pattern.get<Vector*>();
    auto f_v = form.get<Vector*>();

    return try_match_1_seq
      (sr, ignore_ident,
       begin(*p_v), end(*p_v),
       form_env,
       begin(*f_v), end(*f_v),
       [](Vector::iterator){ return false; },
       [](Vector::iterator, Vector::iterator, EqHashMap&) -> EqHashMap{
         return {};
      });
  }else{
    if(equal_internal(pattern, form)){
      return {};
    }else{
      throw try_match_failed();
    }
  }
}

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

Lisp_ptr expand(ExpandSet&, const EqHashMap&, 
                const SyntaxRules&, Lisp_ptr);

template<typename Iter, typename Fun>
Iter expand_seq(ExpandSet& expand_ctx,
                const EqHashMap& match_obj, 
                const SyntaxRules& sr,
                Iter i_begin,
                Iter i_end,
                Fun push_handler){
  auto i = i_begin;

  for(; i != i_end; ++i){
    auto i_next = next(i);

    // check ellipsis
    if((i_next != i_end) && is_ellipsis(*i_next)){
      int depth = 0;
      while(1){
        auto emap = remake_matchobj(match_obj, depth);
        try{
          auto ex = expand(expand_ctx, emap, sr, *i);
          push_handler(ex);
          ++depth;
        }catch(const expand_failed& e){
          break;
        }
      }

      ++i;
    }else{
      auto ex = expand(expand_ctx, match_obj, sr, *i);
      push_handler(ex);
    }
  }

  return i;
}

Lisp_ptr expand(ExpandSet& expand_ctx,
                const EqHashMap& match_obj, 
                const SyntaxRules& sr,
                Lisp_ptr tmpl){
  if(identifierp(tmpl)){
    auto m_ret = match_obj.find(tmpl);
    if(m_ret != match_obj.end()){
      if(m_ret->second){
        return m_ret->second;
      }else{
        throw expand_failed();
      }
    }

    // close to pattern variable
    if(tmpl.tag() == Ptr_tag::symbol){
      if(is_literal_identifier(sr, tmpl)){
        return tmpl;
      }

      unique_ptr<SyntacticClosure, zs_deleter<SyntacticClosure> > new_sc
        (zs_new<SyntacticClosure>(sr.env(), nullptr, tmpl));
      auto iter = expand_ctx.find(new_sc.get());
      if(iter == expand_ctx.end()){
        expand_ctx.insert(new_sc.get());
        return new_sc.release();
      }else{
        return *iter;
      }
    }else if(tmpl.tag() == Ptr_tag::syntactic_closure){
      return tmpl;
    }else{
      UNEXP_DEFAULT();
    }
  }else if(tmpl.tag() == Ptr_tag::cons){
    if(nullp(tmpl)) return tmpl;

    GrowList gl;

    auto last = 
      expand_seq(expand_ctx, match_obj, sr,
                 begin(tmpl), end(tmpl),
                 [&](Lisp_ptr ex){ gl.push(ex); });

    auto ex = expand(expand_ctx, match_obj, sr, last.base());
    return gl.extract_with_tail(ex);
  }else if(tmpl.tag() == Ptr_tag::vector){
    auto t_vec = tmpl.get<Vector*>();

    Vector vec;

    expand_seq(expand_ctx, match_obj, sr,
               begin(*t_vec), end(*t_vec),
               [&](Lisp_ptr ex){ vec.push_back(ex); });

    return zs_new<Vector>(move(vec));
  }else{
    return tmpl;
  }
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
    auto pat_i = begin(i);
    auto tmpl_i = next(pat_i);
    if(next(tmpl_i)){
      throw zs_error_arg1("syntax-rules", "invalid pattern: too long", {i});
    }
      
    check_pattern(*this, *pat_i);
  }
}

SyntaxRules::~SyntaxRules() = default;

Lisp_ptr SyntaxRules::apply(Lisp_ptr form, Env* form_env) const{
  for(auto i : this->rules()){
    auto pat = nth_cons_list<0>(i);
    auto tmpl = nth_cons_list<1>(i);
    auto ignore_ident = pick_first(pat);

    try{
      auto match_ret = try_match_1(*this, ignore_ident, pat, form_env, form);

      ExpandSet expand_ctx;
      return expand(expand_ctx, match_ret, *this, tmpl);
    }catch(const try_match_failed& e){
      continue;
    }catch(const expand_failed& e){
      throw zs_error_arg1("syntax-rules", "expand failed!", {form});
    }
  }

  throw zs_error_arg1("syntax-rules", "no matching pattern found!", {form});
}
