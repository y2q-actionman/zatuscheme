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
bool try_match_1(std::unordered_map<Lisp_ptr, Lisp_ptr>& result,
                 const SyntaxRules& sr, Lisp_ptr ignore_ident, Lisp_ptr pattern, 
                 Env* form_env, Lisp_ptr form){

  // cout << __func__ << endl;
  // cout << "pattern " << pattern << ", form " << form << endl;
    
  const auto ellipsis_sym = intern(vm.symtable(), "...");

  if(identifierp(pattern)){
    if(find(begin(sr.literals()), end(sr.literals()), pattern) != end(sr.literals())){
      // literal identifier
      if(!identifierp(form)) return false;

      return identifier_eq(sr.env(), pattern, form_env, form);
    }else{
      // non-literal identifier
      if(pattern != ignore_ident){
        result.insert({pattern, new SyntacticClosure(form_env, nullptr, form)});
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

        result.insert({*p_i, f_i.base()});
        return true;
      }

      if(f_i == f_e) break; // this check is delayed to here, for checking the ellipsis.

      if(!try_match_1(result, sr, ignore_ident, *p_i, form_env, *f_i)){
        return false;
      }
    }

    // checks length
    if((p_i == p_e) && (f_i == f_e)){
      return try_match_1(result, sr, ignore_ident, p_i.base(), form_env, f_i.base());
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

        result.insert({*p_i, new Vector(f_i, f_e)});
        return true;
      }

      if(f_i == f_e) break; // this check is delayed to here, for checking the ellipsis.

      if(!try_match_1(result, sr, ignore_ident, *p_i, form_env, *f_i)){
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

static
Lisp_ptr expand(const std::unordered_map<Lisp_ptr, Lisp_ptr>& match_obj,
                Lisp_ptr tmpl){
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
          throw zs_error("syntax-rules error: invalid template: followed by '...', but not bound by pattern\n");
        }

        if(auto m_ret_lis = m_ret->second.get<Cons*>()){
          // this can be replaced directly?
          for(auto i : m_ret_lis){
            gl.push(i);
          }
        }else if(auto m_ret_vec = m_ret->second.get<Vector*>()){
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

        if(auto m_ret_lis = m_ret->second.get<Cons*>()){
          vec.insert(vec.end(), begin(m_ret_lis), end(m_ret_lis));
        }else if(auto m_ret_vec = m_ret->second.get<Vector*>()){
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
  Env::map_type ret_map;

  for(auto i : this->rules()){
    auto ignore_ident = pick_first(i.first);
    if(try_match_1(ret_map, *this, ignore_ident, i.first, form_env, form)){
      auto tmp = expand(ret_map, i.second);
      cout << "expand: " << i.second << " -> " << tmp << endl;

      return tmp;
    }else{
      // cleaning map
      for(auto e : ret_map){
        if(auto sc = e.second.get<SyntacticClosure*>()){
          delete sc;
        }
      }
      ret_map.clear();
    }
  }

  throw zs_error("syntax-rules error: no matching pattern found!\n");
}

} // Procedure
