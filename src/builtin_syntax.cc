#include <string>

#include "builtin_syntax.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "eval.hh"
#include "zs_error.hh"
#include "procedure.hh"
#include "printer.hh"
#include "delay.hh"
#include "cons_util.hh"
#include "s_closure.hh"
#include "s_rules.hh"
#include "builtin.hh"
#include "zs_memory.hh"

using namespace std;
using namespace proc_flag;

namespace {

Lisp_ptr lambda_internal(Lisp_ptr args, Lisp_ptr code, Lisp_ptr name){
  auto arg_info = parse_func_arg(args);

  if(arg_info.first < 0){
    throw zs_error_arg1("lambda", "invalid args!", {args});
  }
  if(!code){
    throw zs_error_arg1("lambda", "invalid body!");
  }
  
  return zs_new<IProcedure>(code, 
                            ProcInfo{arg_info.first, arg_info.second},
                            args, vm.frame(), name);
}

Lisp_ptr let_internal(ZsArgs wargs, Entering entering){
  auto arg_i = begin(wargs[0]);

  // skips first 'let' symbol
  ++arg_i;
  if(!arg_i){
    throw zs_error_arg1("let", "informal syntax -- (LET . <...>)", {wargs[0]});
  }

  // checks named let
  Lisp_ptr name = {};

  if(identifierp(*arg_i)){
    name = *arg_i;

    ++arg_i;
    if(!arg_i){
      throw zs_error_arg1("let", "informal syntax -- (LET <name> . <...>)", {wargs[0]});
    }
  }

  // parses binding list
  int len = 0;
  GrowList gl_syms;
  GrowList gl_vals;

  for(auto bind : *arg_i){
    if(bind.tag() != Ptr_tag::cons || nullp(bind)){
      throw zs_error_arg1("let", "informal object found in let binding", {bind});
    }

    ++len;

    gl_syms.push(nth_cons_list<0>(bind));
    gl_vals.push(nth_cons_list<1>(bind));
  }

  ++arg_i;
  if(!arg_i){
    throw zs_error_arg1("let", "informal body", {wargs[0]});
  }

  // picks body
  auto body = arg_i.base();


  // parsing done. insert code.
  wargs.cleanup();

  auto proc = zs_new<IProcedure>(body, 
                                 ProcInfo{len, Variadic::f,  Passing::eval,
                                     Returning::pass, MoveReturnValue::t,
                                     entering},
                                 gl_syms.extract(), vm.frame(), name);

  vm.stack.push_back(push_cons_list({}, gl_vals.extract()));
  vm.code.insert(vm.code.end(), {vm_op_call, proc});
  return {};
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
  auto args = nth_cons_list<1>(wargs[0]);
  auto body = nthcdr_cons_list<2>(wargs[0]);

  return lambda_internal(args, body, {});
}

Lisp_ptr syntax_if(ZsArgs args){
  assert(args.size() == 2 || args.size() == 3);

  Lisp_ptr test = args[0];
  Lisp_ptr conseq = args[1];
  Lisp_ptr alt = (args.size() == 3) ? args[2] : Lisp_ptr();
    
  vm.return_value = {alt, conseq, vm_op_if, test};
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
    if(!nullp(expr_cons.get<Cons*>()->cdr())){
      throw zs_error_arg1("define", "informal syntax: too long");
    }

    vm.code.insert(vm.code.end(), {i1, vm_op_local_set, expr_cons.get<Cons*>()->car()});
    return {};
  }else if(i1.tag() == Ptr_tag::cons){ // i1 points (funcname . arg-list)
    auto funcname = nth_cons_list<0>(i1);
    auto arg_list = nthcdr_cons_list<1>(i1);
    auto code = nthcdr_cons_list<2>(args[0]);

    auto value = lambda_internal(arg_list, code, funcname);
    vm.code.insert(vm.code.end(), {funcname, vm_op_local_set, value});
    return {};
  }else{
    throw zs_error_arg1("define", "informal syntax!");
  }
}

Lisp_ptr syntax_begin(ZsArgs args){
  auto body = nthcdr_cons_list<1>(args[0]);
  if(!body || nullp(body)){
    throw zs_error_arg1("begin", "has no exprs.");
  }
  
  vm.return_value = {body, vm_op_begin};
  return {};
}

Lisp_ptr syntax_let(ZsArgs args){
  return let_internal(move(args), Entering::at_jump);
}

Lisp_ptr syntax_letrec(ZsArgs args){
  // This heavyly depends on the implementation.
  //
  // normal let / funtion call
  //   1. evaluate args
  //      when a lambda form occurs, captures outer environment.
  //   2. enter a new environment.
  //   3. set args to the environment.
  // 
  // early bind
  //   1. enter a new environment.
  //   2. evaluate args
  //      when a lambda form occurs, captures the environment.
  //      so these lambdas refer the same environment.
  //   3. set args to the environment.
  return let_internal(move(args), Entering::at_bind);
}

Lisp_ptr syntax_delay(ZsArgs args){
  return {zs_new<Delay>(args[0], vm.frame())};
}

Lisp_ptr syntax_quasiquote(ZsArgs args){
  auto arg = nth_cons_list<1>(args[0]);

  if(arg.tag() != Ptr_tag::cons && arg.tag() != Ptr_tag::vector){
    // acting as a normal quote.
    return make_cons_list({find_builtin_nproc("quote"), arg});
  }


  const auto quasiquote_fun = find_builtin_nproc("quasiquote");
  const auto unquote_fun = find_builtin_nproc("unquote");
  const auto unquote_splicing_fun = find_builtin_nproc("unquote-splicing");

  GrowList gl;

  const auto qq_elem = [&](Lisp_ptr p){
    gl.push(make_cons_list({quasiquote_fun, p}));
  };

  if(arg.tag() == Ptr_tag::cons){
    if(nullp(arg)){
      return Cons::NIL;
    }

    // check unquote -- like `,x
    auto first_val = vm.frame()->find(nth_cons_list<0>(arg));
    if(eq_internal(first_val, unquote_fun)
       || eq_internal(first_val, unquote_splicing_fun)){
      return arg;
    }

    // generic lists
    auto i = begin(arg);
    for(; i; ++i){
      qq_elem(*i);
    }
    qq_elem(i.base());

    return push_cons_list(find_builtin_nproc("list*"), gl.extract());
  }else if(arg.tag() == Ptr_tag::vector){
    auto v = arg.get<Vector*>();
    for(auto p : *v){
      qq_elem(p);
    }

    return push_cons_list(find_builtin_nproc("vector"), gl.extract());
  }else{
    UNEXP_DEFAULT();
  }
}

Lisp_ptr syntax_unquote(ZsArgs args){
  return args[0];
}

Lisp_ptr syntax_unquote_splicing(ZsArgs args){
  if(args[0].tag() != Ptr_tag::cons){
    throw builtin_type_check_failed("unquote-splicing", Ptr_tag::cons, args[0]);
  }

  vm.return_value.assign(begin(args[0]), end(args[0]));
  return {};
}

Lisp_ptr syntax_else(ZsArgs){
  throw zs_error_arg1("else", "cannot be used as operator!!");
}

Lisp_ptr syntax_arrow(ZsArgs){
  throw zs_error_arg1("=>", "cannot be used as operator!!");
}

Lisp_ptr syntax_define_syntax(ZsArgs args){
  if(!identifierp(args[0])){
    throw builtin_identifier_check_failed("define-syntax", args[0]);
  }

  // TODO: check args[1] is a transformer.
  vm.code.insert(vm.code.end(), {args[0], vm_op_local_set, args[1]});
  return Lisp_ptr{true};
}

Lisp_ptr syntax_let_syntax(ZsArgs args){
  // TODO: check each arg is a transformer.
  return let_internal(move(args), Entering::at_jump);
}

Lisp_ptr syntax_letrec_syntax(ZsArgs args){
  // TODO: check each arg is a transformer.
  return let_internal(move(args), Entering::at_bind);
}

Lisp_ptr syntax_syntax_rules(ZsArgs args){
  auto env = args[1].get<Env*>();
  if(!env){
    throw builtin_type_check_failed("syntax-rules", Ptr_tag::env, args[1]);
  }

  auto literals = nth_cons_list<1>(args[0]);
  auto rest = nthcdr_cons_list<2>(args[0]);

  return zs_new<SyntaxRules>(env, literals, rest);
}
    
} // namespace builtin
