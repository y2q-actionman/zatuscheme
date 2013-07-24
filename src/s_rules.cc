#include <iterator>
#include <unordered_set>
#include <unordered_map>
#include <vector>

#include "builtin_extra.hh"
#include "cons_util.hh"
#include "equality.hh"
#include "printer.hh"
#include "s_rules.hh"
#include "s_closure.hh"
#include "symbol.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

using namespace std;

namespace {

// internal types
typedef std::unordered_map<Lisp_ptr, Lisp_ptr, EqHashObj, EqObj> EqHashMap;
typedef std::unordered_set<Lisp_ptr, EqHashObj, EqObj> MatchSet;
typedef std::unordered_map<Lisp_ptr, SyntacticClosure*, EqHashObj, EqObj> ExpandMap;

// error classes
struct try_match_failed{};
struct expand_failed {};

bool is_literal_identifier(const SyntaxRules& sr, Lisp_ptr p){
  for(auto i = begin(sr.literals()); i; ++i){
    if(eq_internal(*i, p)){
      return true;
    }
  }
  return false;
}

bool is_ellipsis(Lisp_ptr p){
  if(p.tag() == Ptr_tag::symbol){
    return p.get<Symbol*>()->name() == "...";
  }else if(p.tag() == Ptr_tag::syntactic_closure){
    return is_ellipsis(p.get<SyntacticClosure*>()->expr());
  }else{
    return false;
  }
}

void push_tail_cons_list_nl(Lisp_ptr p, Lisp_ptr value){
  if(p.tag() != Ptr_tag::cons)
    throw_zs_error(p, "internal %s: the passed list is a dotted list!\n", __func__);

  auto c = p.get<Cons*>();
  if(!c)
    throw_zs_error(p, "internal %s: the passed list is an empty list!\n", __func__);

  if(nullp(cdr(c))){
    rplacd(c, make_cons_list({value}));
  }else{
    push_tail_cons_list_nl(cdr(c), value);
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
    check_nonnull_cons(p);

    return nth_cons_list<0>(p);
  }else if(p.tag() == Ptr_tag::vector){
    auto v = p.get<Vector*>();
    assert(v);
    if(v->empty()) throw_zs_error({}, "the pattern is empty vector");

    return (*v)[0];
  }else{
    throw_zs_error(p, "informal pattern passed!");
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
      throw_zs_error(p, "duplicated pattern variable!");
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

EqHashMap
try_match_1(const SyntaxRules& sr, Lisp_ptr ignore_ident, Lisp_ptr pattern, 
            Env* form_env, Lisp_ptr form){
  if(identifierp(pattern)){
    if(is_literal_identifier(sr, pattern)){
      // literal identifier
      if(identifierp(form) && eq_internal(pattern, form)){
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

    EqHashMap match_obj;
    auto p_i = begin(pattern);
    auto f_i = begin(form);
  
    for(; p_i; ++p_i){
      // checks ellipsis
      auto p_n = next(p_i);
      if((p_n) && is_ellipsis(*p_n)){
        if(eq_internal(*p_i, ignore_ident)){
          throw_zs_error({}, "'...' is appeared following the first identifier");
        }

        auto p_e = end(pattern);
        auto f_e = end(form);

        EqHashMap acc_map;
        ensure_binding(acc_map, sr, ignore_ident, *p_i,
                       [](){ return Cons::NIL; });

        for(; f_i; ++f_i){
          auto m = try_match_1(sr, ignore_ident, *p_i, form_env, *f_i);

          for(auto i : m){
            auto place = acc_map.find(i.first);
            assert(place->second.tag() == Ptr_tag::cons);
            push_tail_cons_list(&place->second, i.second);
          }
        }

        match_obj.insert(begin(acc_map), end(acc_map));

        auto tail = try_match_1(sr, ignore_ident, p_e.base(), form_env, f_e.base());
        match_obj.insert(begin(tail), end(tail));

        return match_obj;
      }

      if(!f_i) break; // this check is delayed to here, for checking the ellipsis.

      auto m = try_match_1(sr, ignore_ident, *p_i, form_env, *f_i);
      match_obj.insert(begin(m), end(m));

      assert(f_i);
      ++f_i;
    }
    assert(!p_i || !f_i);


    if(!nullp(p_i.base())){
      if(p_i.base().tag() == Ptr_tag::cons){
        throw try_match_failed();
      }

      auto m = try_match_1(sr, ignore_ident, p_i.base(), form_env, f_i.base());
      match_obj.insert(begin(m), end(m));
      return match_obj;
    }else if(!p_i && !f_i){
      return match_obj;
    }else{
      throw try_match_failed();
    }
  }else if(pattern.tag() == Ptr_tag::vector){
    if(form.tag() != Ptr_tag::vector){
      throw try_match_failed();
    }

    auto p_v = pattern.get<Vector*>();
    auto f_v = form.get<Vector*>();

    EqHashMap match_obj;
    auto p_i = begin(*p_v), p_e = end(*p_v);
    auto f_i = begin(*f_v), f_e = end(*f_v);
  
    for(; p_i != p_e; ++p_i){
      // checks ellipsis
      auto p_n = next(p_i);
      if((p_n != p_e) && is_ellipsis(*p_n)){
        if(eq_internal(*p_i, ignore_ident)){
          throw_zs_error({}, "'...' is appeared following the first identifier");
        }

        // accumulating...
        EqHashMap acc_map;
        ensure_binding(acc_map, sr, ignore_ident, *p_i,
                       [](){ return Cons::NIL; });

        for(; f_i != f_e; ++f_i){
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

      if(f_i == f_e) break; // this check is delayed to here, for checking the ellipsis.

      auto m = try_match_1(sr, ignore_ident, *p_i, form_env, *f_i);
      match_obj.insert(begin(m), end(m));

      assert(f_i != f_e);
      ++f_i;
    }
    assert((p_i == p_e) || (f_i == f_e));


    if((p_i == p_e) && (f_i == f_e)){
      return match_obj;
    }else{
      throw try_match_failed();
    }
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
    if(i.second.tag() == Ptr_tag::cons){
      auto ci = begin(i.second);

      for(int d = 0; d < pick_depth; ++d){
        if(!ci) break;
        ++ci;
      }

      if(ci){
        ret.insert({i.first, *ci});
        continue;
      }
    }

    ret.insert({i.first, {}});
  }

  return ret;
}

Lisp_ptr expand(ExpandMap& expand_ctx,
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

      auto iter = expand_ctx.find(tmpl);
      if(iter != expand_ctx.end()){
        return iter->second;
      }else{
        auto new_sc = zs_new<SyntacticClosure>(sr.env(), Cons::NIL, tmpl);
        expand_ctx.insert({tmpl, new_sc});
        return new_sc;
      }
    }else if(tmpl.tag() == Ptr_tag::syntactic_closure){
      return tmpl;
    }else{
      UNEXP_DEFAULT();
    }
  }else if(tmpl.tag() == Ptr_tag::cons){
    if(nullp(tmpl)) return tmpl;

    GrowList gl;

    auto i = begin(tmpl);
    for(; i; ++i){
      auto i_next = next(i);

      // check ellipsis
      if(i_next && is_ellipsis(*i_next)){
        for(int depth = 0; true; ++depth){
          auto emap = remake_matchobj(match_obj, depth);
          try{
            gl.push(expand(expand_ctx, emap, sr, *i));
          }catch(const expand_failed& e){
            break;
          }
        }
        i = i_next;
      }else{
        gl.push(expand(expand_ctx, match_obj, sr, *i));
      }
    }

    return gl.extract_with_tail(expand(expand_ctx, match_obj, sr, i.base()));
  }else if(tmpl.tag() == Ptr_tag::vector){
    auto t_vec = tmpl.get<Vector*>();

    Vector vec;

    for(auto i = begin(*t_vec), i_end = end(*t_vec); i != i_end; ++i){
      auto i_next = next(i);

      // check ellipsis
      if((i_next != i_end) && is_ellipsis(*i_next)){
        for(int depth = 0; true; ++depth){
          auto emap = remake_matchobj(match_obj, depth);
          try{
            vec.push_back(expand(expand_ctx, emap, sr, *i));
          }catch(const expand_failed& e){
            break;
          }
        }
        i = i_next;
      }else{
        vec.push_back(expand(expand_ctx, match_obj, sr, *i));
      }
    }

    return zs_new<Vector>(move(vec));
  }else{
    return tmpl;
  }
}

} // namespace

constexpr ProcInfo SyntaxRules::sr_procinfo;

SyntaxRules::SyntaxRules(Env* e, Lisp_ptr lits, Lisp_ptr rls)
  : env_(e), literals_(lits), rules_(rls), name_(){
  for(auto i = begin(lits); i; ++i){
    check_identifier_type(*i);
  }

  for(auto i = begin(rls); i; ++i){
    auto pat_i = begin(*i);
    auto tmpl_i = next(pat_i);
    if(next(tmpl_i)){
      throw_zs_error(*i, "invalid pattern: too long");
    }
      
    check_pattern(*this, *pat_i);
  }
}

SyntaxRules::~SyntaxRules() = default;

Lisp_ptr SyntaxRules::apply(Lisp_ptr form, Env* form_env) const{
  for(auto i = begin(rules()); i; ++i){
    auto pat = nth_cons_list<0>(*i);
    auto tmpl = nth_cons_list<1>(*i);
    auto ignore_ident = pick_first(pat);

    try{
      auto match_ret = try_match_1(*this, ignore_ident, pat, form_env, form);

      ExpandMap expand_ctx;
      return expand(expand_ctx, match_ret, *this, tmpl);
    }catch(const try_match_failed& e){
      continue;
    }catch(const expand_failed& e){
      throw_zs_error(form, "expand failed!");
    }
  }

  throw_zs_error(form, "no matching pattern found!");
}
