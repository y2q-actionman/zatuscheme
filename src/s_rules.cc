#include <memory>
#include <algorithm>
#include <iterator>
#include <unordered_set>
#include <unordered_map>

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

// #include <iostream>

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

static
void ensure_binding(std::unordered_map<Lisp_ptr, Lisp_ptr>& match_obj,
                    const SyntaxRules& sr, Lisp_ptr ignore_ident, Lisp_ptr pattern){
  if(identifierp(pattern)){
    if(find(begin(sr.literals()), end(sr.literals()), pattern) != end(sr.literals())){
      return;
    }else{
      // non-literal identifier
      if(pattern != ignore_ident){
        match_obj.insert({pattern, new Vector()});
      }
      return;
    }
  }else if(pattern.tag() == Ptr_tag::cons){
    if(nullp(pattern)){
      return;
    }

    auto p_i = begin(pattern);

    for(; p_i; ++p_i){
      ensure_binding(match_obj, sr, ignore_ident, *p_i);
    }

    // checks length
    if((p_i.base().tag() == Ptr_tag::cons)){
      return;
    }else{
      // dotted list case
      ensure_binding(match_obj, sr, ignore_ident, p_i.base());
      return;
    }
  }else if(pattern.tag() == Ptr_tag::vector){
    auto p_v = pattern.get<Vector*>();
    auto p_i = begin(*p_v), p_e = end(*p_v);

    for(; p_i != p_e; ++p_i){
      ensure_binding(match_obj, sr, ignore_ident, *p_i);
    }
  }else{
    return;
  }
  
}

static
bool try_match_1(std::unordered_map<Lisp_ptr, Lisp_ptr>& match_obj,
                 const SyntaxRules& sr, Lisp_ptr ignore_ident, Lisp_ptr pattern, 
                 Env* form_env, Lisp_ptr form,
                 bool insert_by_push){

  // cout << __func__ << "\tpattern = " << pattern << "\n\t\tform = " << form << endl;
  // for(auto ii : match_obj){
  //   cout << '\t' << ii.first << " = " << ii.second << '\n';
  // }

  if(form.tag() == Ptr_tag::syntactic_closure){
    // destruct syntactic closure
    auto sc = form.get<SyntacticClosure*>();

    return try_match_1(match_obj, sr, ignore_ident, pattern, sc->env(), sc->expr(), insert_by_push);
  }

  const auto ellipsis_sym = intern(vm.symtable(), "...");

  if(identifierp(pattern)){
    if(find(begin(sr.literals()), end(sr.literals()), pattern) != end(sr.literals())){
      // literal identifier
      // if(!identifierp(form))
      //   cout << __func__ << "\tcheck failed @ " << __LINE__ << endl;
      if(!identifierp(form)) return false;

      // if(!identifier_eq(sr.env(), pattern, form_env, form))
      //   cout << __func__ << "\tcheck failed @ " << __LINE__ << endl;
      return identifier_eq(sr.env(), pattern, form_env, form);
    }else{
      // non-literal identifier
      if(pattern != ignore_ident){
        auto val = new SyntacticClosure(form_env, nullptr, form);
        if(insert_by_push){
          auto place = match_obj.find(pattern);
          assert(place->second.tag() == Ptr_tag::vector);
          place->second.get<Vector*>()->push_back(val);
        }else{
          match_obj.insert({pattern, val});
        }
      }
      return true;
    }
  }else if(pattern.tag() == Ptr_tag::cons){
    if(form.tag() != Ptr_tag::cons){
      // cout << __func__ << "\tcheck failed @ " << __LINE__ << endl;
      return false;
    }

    if(nullp(pattern) && nullp(form)){
      return true;
    }

    auto p_i = begin(pattern);
    auto f_i = begin(form);

    for(; p_i; ++p_i, (f_i ? ++f_i : f_i)){
      // checks ellipsis
      auto p_n = next(p_i);
      if((p_n) && identifierp(*p_n) && identifier_symbol(*p_n) == ellipsis_sym){
        if(*p_i == ignore_ident){
          throw zs_error("syntax-rules error: '...' is appeared following the first identifier.\n");
        }

        auto p_e = p_i;
        while(p_e) ++p_e;

        if(!nullp(p_e.base())){
          throw zs_error("syntax-rules error: '...' is appeared in a inproper list pattern!\n");
        }

        // accumulating...
        ensure_binding(match_obj, sr, ignore_ident, *p_i);
        for(; f_i; ++f_i){
          if(!try_match_1(match_obj, sr, ignore_ident, *p_i, form_env, *f_i, true)){
            // cout << __func__ << "\tcheck failed @ " << __LINE__ << endl;
            return false;
          }
        }

        if(!nullp(f_i.base())){
          throw zs_error("syntax-rules error: '...' is used for a inproper list form!\n");
        }

        return true;
      }

      if(!f_i) break; // this check is delayed to here, for checking the ellipsis.

      if(!try_match_1(match_obj, sr, ignore_ident, *p_i, form_env, *f_i, insert_by_push)){
        // cout << __func__ << "\tcheck failed @ " << __LINE__ << endl;
        return false;
      }
    }

    // checks length
    if((p_i.base().tag() == Ptr_tag::cons) && (f_i.base().tag() == Ptr_tag::cons)){
      // cout << "\treached @ " << __LINE__ << endl;
      // cout << "\t\t pat = " << p_i.base() << ", form = " << f_i.base() << endl;
      return (nullp(p_i.base()) && nullp(f_i.base()));
    }else{
      // dotted list case
      // cout << __func__ << "\treached @ " << __LINE__ << endl;
      return try_match_1(match_obj, sr, ignore_ident, p_i.base(), form_env, f_i.base(), insert_by_push);
    }
  }else if(pattern.tag() == Ptr_tag::vector){
    if(form.tag() != Ptr_tag::vector){
      // cout << __func__ << "\tcheck failed @ " << __LINE__ << endl;
      return false;
    }

    auto p_v = pattern.get<Vector*>();
    auto f_v = form.get<Vector*>();

    auto p_i = begin(*p_v), p_e = end(*p_v);
    auto f_i = begin(*f_v), f_e = end(*f_v);

    for(; p_i != p_e; ++p_i, ++f_i){
      // checks ellipsis
      auto p_n = next(p_i);

      if((p_n != p_e) && identifierp(*p_n) && identifier_symbol(*p_n) == ellipsis_sym){
        if(identifierp(*p_i)){
          // like: (A B C ...)
          if(*p_i == ignore_ident){
            throw zs_error("syntax-rules error: '...' is appeared following the first identifier.\n");
          }

          match_obj.insert({*p_i, new Vector(f_i, f_e)});
          return true;
        }else{
          // like: ((A B C) ...)
          throw zs_error("syntax-rules error: ellipsis pattern is under implementing...\n");
        }
      }

      if(f_i == f_e) break; // this check is delayed to here, for checking the ellipsis.

      if(!try_match_1(match_obj, sr, ignore_ident, *p_i, form_env, *f_i, insert_by_push)){
        // cout << __func__ << "\tcheck failed @ " << __LINE__ << endl;
        return false;
      }
    }

    // checks length
    if((p_i == p_e) && (f_i == f_e)){
      return true;
    }else{
      // cout << __func__ << "\tcheck failed @ " << __LINE__ << endl;
      return false;
    }
  }else{
    // cout << __func__ << "\tcheck failed @ " << __LINE__ << endl;
    return equal_internal(pattern, form);
  }
}

static
Lisp_ptr expand(const std::unordered_map<Lisp_ptr, Lisp_ptr>& match_obj,
                Lisp_ptr tmpl){
  // cout << __func__ << " arg = " << tmpl << endl;
  // for(auto ii : match_obj){
  //   cout << '\t' << ii.first << " = " << ii.second << '\n';
  // }

  const auto ellipsis_sym = intern(vm.symtable(), "...");

  if(identifierp(tmpl)){
    auto m_ret = match_obj.find(tmpl);
    if(m_ret != match_obj.end()){
      return m_ret->second;
    }else{
      return tmpl;
    }
  }else if(tmpl.tag() == Ptr_tag::cons){
    if(nullp(tmpl)) return tmpl;

    GrowList gl;
    auto t_i = begin(tmpl);

    for(; t_i; ++t_i){
      auto t_n = next(t_i);

      // check ellipsis
      if(identifierp(*t_i)
         && (t_n) && identifierp(*t_n)
         && identifier_symbol(*t_n) == ellipsis_sym){
        auto m_ret = match_obj.find(*t_i);
        
        if(m_ret == match_obj.end()){
          // cout << __func__ << " key = " << *t_i << endl;
          throw zs_error("syntax-rules error: invalid template: followed by '...', but not bound by pattern\n");
        }

        if(m_ret->second.tag() == Ptr_tag::cons){
          // this can be replaced directly?
          for(auto i : m_ret->second){
            gl.push(i);
          }
        }else if(m_ret->second.tag() == Ptr_tag::vector){
          auto m_ret_vec = m_ret->second.get<Vector*>();
          for(auto i : *m_ret_vec){
            gl.push(i);
          }
        }else{
          throw zs_error("syntax-rules error: invalid template: not sequence type\n");
        }

        ++t_i;
      }else{
        gl.push(expand(match_obj, *t_i));
      }
    }

    return gl.extract_with_tail(expand(match_obj, t_i.base()));
  }else if(tmpl.tag() == Ptr_tag::vector){
    auto t_vec = tmpl.get<Vector*>();

    Vector vec;

    for(auto t_i = begin(*t_vec), t_e = end(*t_vec); t_i != t_e; ++t_i){
      auto t_n = next(t_i);

      // check ellipsis
      if(identifierp(*t_i)
         && (t_n != t_e) && identifierp(*t_n)
         && identifier_symbol(*t_n) == ellipsis_sym){
        auto m_ret = match_obj.find(*t_i);
        
        if(m_ret == match_obj.end()){
          throw zs_error("syntax-rules error: invalid template: followed by '...', but not bound by pattern\n");
        }

        if(m_ret->second.tag() == Ptr_tag::cons){
          vec.insert(vec.end(), begin(m_ret->second), end(m_ret->second));
        }else if(m_ret->second.tag() == Ptr_tag::vector){
          auto m_ret_vec = m_ret->second.get<Vector*>();
          vec.insert(vec.end(), begin(*m_ret_vec), end(*m_ret_vec));
        }else{
          throw zs_error("syntax-rules error: invalid template: not sequence type\n");
        }

        ++t_i;
      }else{
        vec.push_back(expand(match_obj, *t_i));
      }
    }

    return new Vector(move(vec));
  }else{
    return tmpl;
  }
}

Lisp_ptr SyntaxRules::apply(Lisp_ptr form, Env* form_env) const{
  std::unordered_map<Lisp_ptr, Lisp_ptr> match_obj;

  // cout << "## " << __func__ << ": form = " << form << endl;

  for(auto i : this->rules()){
    auto pat = i.get<Cons*>()->car();
    auto tmpl = i.get<Cons*>()->cdr().get<Cons*>()->car();

    // cout << "## trying: pattern = " << pat << endl;

    auto ignore_ident = pick_first(pat);
    if(try_match_1(match_obj, *this, ignore_ident, pat, form_env, form, false)){
      // cout << "## matched!:\tpattern = " << pat << '\n';
      // cout << "## \t\tform = " << form << '\n';
      // for(auto ii : match_obj){
      //   cout << '\t' << ii.first << " = " << ii.second << '\n';
      // }

      auto ex = expand(match_obj, tmpl);
      // cout << "## expand = " << ex << '\n';
      // cout << endl;
      return ex;
    }else{
      // cleaning map
      for(auto e : match_obj){
        if(auto sc = e.second.get<SyntacticClosure*>()){
          delete sc;
        }
      }
      match_obj.clear();
    }
  }

  // cout << "## no match: form = " << form << endl;
  throw zs_error("syntax-rules error: no matching pattern found!\n");
}

} // Procedure
