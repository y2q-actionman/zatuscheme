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

Lisp_ptr syntax_lambda(ZsArgs args){
  return lambda_internal(args[0], args[1]);
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
  if(identifierp(args[0])){
    auto& ident = args[0];

    if(nullp(args[1]) || !nullp(nthcdr_cons_list<1>(args[1]))){
      throw_zs_error(args[1], "informal length");
    }
    auto expr = nth_cons_list<0>(args[1]);

    vm.code.insert(vm.code.end(), {ident, vm_op_define, expr});
    return {};
  }else if(args[0].tag() == Ptr_tag::cons){
    auto& largs = args[0];
    auto& code = args[1];
    vm.code.insert(vm.code.end(),
                   {nth_cons_list<0>(largs), // funcname
                    vm_op_define,
                    lambda_internal(nthcdr_cons_list<1>(largs), // arg_list
                                    code)});
    return {};
  }else{
    throw_zs_error(args[0], "informal syntax!");
  }
}

Lisp_ptr syntax_internal_quasiquote_vector(ZsArgs args){
  const auto quasiquote_sym = intern(*vm.symtable, "quasiquote");
  const auto vector_sym = intern(*vm.symtable, "%vector");

  GrowList gl;

  const auto qq_elem = [&](Lisp_ptr p){
    gl.push(make_cons_list({quasiquote_sym, p}));
  };

  gl.push(vector_sym);

  for(auto i = begin(args[0]); i; ++i){
    qq_elem(*i);
  }

  return gl.extract();
}

Lisp_ptr syntax_unquote_splicing(ZsArgs args){
  check_type(Ptr_tag::cons, args[0]);

  vm.return_value.assign(begin(args[0]), end(args[0]));
  return {};
}

Lisp_ptr syntax_syntax_rules(ZsArgs args){
  check_type(Ptr_tag::env, args[1]);

  return zs_new<SyntaxRules>(args[1].get<Env*>(),
                             nth_cons_list<1>(args[0]), // literals
                             nthcdr_cons_list<2>(args[0])); // rest
}
    
Lisp_ptr syntax_internal_memv(ZsArgs args){
  check_type(Ptr_tag::cons, args[1]);

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
