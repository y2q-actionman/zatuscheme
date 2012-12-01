#include "builtin_procedure.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "eval.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

namespace {

Lisp_ptr type_check_procedure(){
  auto arg = pick_args_1();
  return Lisp_ptr{(arg.tag() == Ptr_tag::i_procedure) || (arg.tag() == Ptr_tag::n_procedure)};
}

Lisp_ptr proc_values(){
  vm.return_value.clear();

  stack_to_vector(vm.stack, vm.return_value);

  if(vm.return_value.empty()){
    vm.return_value.resize(1);
  }

  return vm_op_nop;
}

} // namespace

const BuiltinFunc
builtin_procedure[] = {
  {"procedure?", {
      type_check_procedure,
      {Calling::function, 1}}},
  {"apply", {
      apply_func,
      {Calling::function, 1, Variadic::t}}},
  {"force", {
      func_force,
      {Calling::function, 1}}},
  {"values", {
      proc_values,
      {Calling::function, 0, Variadic::t}}},
  {"call-with-values", {
      call_with_values,
      {Calling::function, 2}}},
  {"call-with-current-continuation", {
      call_cc,
      {Calling::function, 1}}},
  {"dynamic-wind", {
      dynamic_wind,
      {Calling::function, 3}}},
};

const size_t builtin_procedure_size = sizeof(builtin_procedure) / sizeof(builtin_procedure[0]);


const char* builtin_procedure_load[] = {
  "(define (map proc . lists)"
  "  (define (all-null? lists)" // TODO: move this logic into arg-cllect
  "     (if (null? lists) #t"
  "         (if (null? (car lists)) (all-null? (cdr lists))"
  "             #f)))"
  "  (define (worker lists rets)"
  "    (if (or (null? lists) (all-null? lists))"
  "        rets"
  "      (let arg-collect ((args ()) (next-lists ()) (lis lists))"
  "        (if (null? lis)"
  "            (worker next-lists (cons (apply proc args) rets))"
  "          (if (null? (car lis))"
  "              (begin (display \"error_lengths_are_mismatched\") (newline) #f)"
  "            (arg-collect (cons (caar lis) args)"
  "                         (cons (cdar lis) next-lists)"
  "                         (cdr lis)))))))"
  "  (reverse (worker lists ())))",

  "(define (for-each proc . lists)"
  "  (define (worker lists)"
  "    (if (null? lists)"
  "        #t"
  "      (let arg-collect ((args ()) (next-lists ()) (lis lists))"
  "        (if (null? lis)"
  "            (begin (apply proc (reverse args))"
  "                   (worker (reverse next-lists)))"
  "          (if (null? (car lis))"
  "              #f"
  "            (arg-collect (cons (caar lis) args)"
  "                         (cons (cdar lis) next-lists)"
  "                         (cdr lis)))))))"
  "  (worker lists))"
};

const size_t builtin_procedure_load_size
= sizeof(builtin_procedure_load) / sizeof(builtin_procedure_load[0]);
