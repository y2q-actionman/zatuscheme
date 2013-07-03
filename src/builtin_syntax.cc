#include <string>
#include <vector>

#include "builtin_syntax.hh"
#include "cons_util.hh"
#include "eval.hh"
#include "lisp_ptr.hh"
#include "procedure.hh"
#include "s_closure.hh"
#include "s_rules.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

using namespace std;
using namespace proc_flag;

namespace {

Lisp_ptr lambda_internal(Lisp_ptr args, Lisp_ptr code){
  auto arg_info = parse_func_arg(args);

  if(arg_info.first < 0){
    throw_zs_error(args, "invalid args!");
  }

  if(!code || nullp(code)){
    throw_zs_error(code, "invalid body!");
  }

  return zs_new<IProcedure>(code, 
                            ProcInfo{arg_info.first, arg_info.second},
                            args, vm.frame);
}

} // namespace

namespace builtin {

Lisp_ptr syntax_quote(ZsArgs args){
  if(args[0].tag() == Ptr_tag::syntactic_closure){
    return args[0].get<SyntacticClosure*>()->expr();
  }else{
    return args[0];
  }
}

Lisp_ptr syntax_lambda(ZsArgs wargs){
  return lambda_internal(nth_cons_list<1>(wargs[0]), // args
                         nthcdr_cons_list<2>(wargs[0])); // body
}

Lisp_ptr syntax_if(ZsArgs args){
  vm.return_value = {(args.size() == 3) ? args[2] : Lisp_ptr(), // alt
                     args[1],   // conseq
                     vm_op_if,
                     args[0]};  // test
  return {};
}

Lisp_ptr syntax_set(ZsArgs args){
  vm.return_value = {args[0], vm_op_set, args[1]};
  return {};
}

Lisp_ptr syntax_define(ZsArgs args){
  auto i1 = nth_cons_list<1>(args[0]);

  // extracting
  if(identifierp(i1)){         // i1 points variable's name.
    auto expr_cons = nthcdr_cons_list<2>(args[0]);

    assert(expr_cons.get<Cons*>());
    if(!nullp(cdr(expr_cons.get<Cons*>()))){
      throw_zs_error(args[0], "informal syntax: too long");
    }

    vm.code.insert(vm.code.end(), {i1, vm_op_define, car(expr_cons.get<Cons*>())});
    return {};
  }else if(i1.tag() == Ptr_tag::cons){
    vm.code.insert(vm.code.end(),
                   {nth_cons_list<0>(i1), // funcname
                    vm_op_define,
                    lambda_internal(nthcdr_cons_list<1>(i1), // arg_list
                                    nthcdr_cons_list<2>(args[0]))}); // code
    return {};
  }else{
    throw_zs_error(args[0], "informal syntax!");
  }
}

Lisp_ptr syntax_quasiquote(ZsArgs args){
  auto& arg = args[0];

  if(arg.tag() != Ptr_tag::cons && arg.tag() != Ptr_tag::vector){
    // acting as a normal quote.
    return make_cons_list({intern(*vm.symtable, "quote"), arg});
  }

  const auto quasiquote_sym = intern(*vm.symtable, "quasiquote");
  const auto unquote_sym = intern(*vm.symtable, "unquote");
  const auto unquote_splicing_sym = intern(*vm.symtable, "unquote-splicing");
  const auto list_star_sym = intern(*vm.symtable, "%list*");
  const auto vector_sym = intern(*vm.symtable, "%vector");

  GrowList gl;

  const auto qq_elem = [&](Lisp_ptr p){
    gl.push(make_cons_list({quasiquote_sym, p}));
  };

  if(arg.tag() == Ptr_tag::cons){
    if(nullp(arg)){
      return Cons::NIL;
    }

    // check unquote -- like `,x
    auto first_sym = nth_cons_list<0>(arg).get<Symbol*>();
    if(first_sym == unquote_sym
       || first_sym == unquote_splicing_sym){
      return arg;
    }

    gl.push(list_star_sym);

    // generic lists
    auto i = begin(arg);
    for(; i; ++i){
      qq_elem(*i);
    }
    qq_elem(i.base());

    return gl.extract();
  }else if(arg.tag() == Ptr_tag::vector){
    gl.push(vector_sym);

    auto v = arg.get<Vector*>();
    for(auto p : *v){
      qq_elem(p);
    }

    return gl.extract();
  }else{
    UNEXP_DEFAULT();
  }
}

Lisp_ptr syntax_unquote_splicing(ZsArgs args){
  if(args[0].tag() != Ptr_tag::cons){
    throw_builtin_type_check_failed(Ptr_tag::cons, args[0]);
  }

  vm.return_value.assign(begin(args[0]), end(args[0]));
  return {};
}

Lisp_ptr syntax_syntax_rules(ZsArgs args){
  auto env = args[1].get<Env*>();
  if(!env){
    throw_builtin_type_check_failed(Ptr_tag::env, args[1]);
  }

  return zs_new<SyntaxRules>(env,
                             nth_cons_list<1>(args[0]), // literals
                             nthcdr_cons_list<2>(args[0])); // rest
}
    
Lisp_ptr syntax_internal_memv(ZsArgs args){
  if(args[1].tag() != Ptr_tag::cons){
    throw_builtin_type_check_failed(Ptr_tag::cons, args[1]);
  }

  for(auto i = begin(args[1]); i; ++i){
    if(eqv_internal(args[0], *i))
      return i.base();
  }

  return Lisp_ptr{false};
}

Lisp_ptr syntax_internal_list_star(ZsArgs args){
  GrowList gl;

  for(auto i = 0; i < args.size() - 1; ++i){
    gl.push(args[i]);
  }

  return gl.extract_with_tail(args[args.size() - 1]);
}

Lisp_ptr syntax_internal_vector(ZsArgs args){
  return {zs_new<Vector>(args.begin(), args.end())};
}

} // namespace builtin
