#include <cassert>
#include <istream>
#include <ostream>
#include <fstream>
#include <iostream>

#include "builtin_port.hh"
#include "lisp_ptr.hh"
#include "port.hh"
#include "reader.hh"
#include "printer.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

namespace {

#define CURRENT_INPUT_PORT_SYMNAME "current-input-port-value"
#define CURRENT_OUTPUT_PORT_SYMNAME "current-output-port-value"

template<typename T>
zs_error port_type_check_failed(const char* func_name, Lisp_ptr p){
  return make_zs_error("native func: %s: arg is not %s! (%s)\n",
                       func_name, stringify(to_tag<Ptr_tag, T*>()), stringify(p.tag()));
}

template<typename T>
void port_io_p(){
  auto arg = pick_args_1();

  vm.return_value[0] = Lisp_ptr{arg.tag() == to_tag<Ptr_tag, T*>()};
}
  
void port_i_p(){
  port_io_p<InputPort>();
}

void port_o_p(){
  port_io_p<OutputPort>();
}

template<typename IOType, typename F_IOType>
void port_open_file(const char* name){
  auto arg = pick_args_1();
  auto str = arg.get<String*>();
  if(!str){
    throw builtin_type_check_failed(name, Ptr_tag::string, arg);
  }

  IOType* p = new F_IOType(str->c_str());
  if(!*p){
    throw make_zs_error("native error: %s: failed at opening file\n", name);
  }
  
  vm.return_value[0] = {p};
}  

void port_open_file_i(){
  port_open_file<InputPort, ifstream>("open-input-file");
}  

void port_open_file_o(){
  port_open_file<OutputPort, ofstream>("open-output-file");
}  

template<typename IOType, typename F_IOType>
void port_close(const char* name){
  auto arg = pick_args_1();
  auto p = arg.get<IOType*>();
  if(!p){
    throw port_type_check_failed<IOType>(name, arg);
  }

  auto fio = dynamic_cast<F_IOType*>(p);
  if(!fio){
    cerr << "native func warning: " << name << ": passed port is not associated to file\n";
    vm.return_value[0] = Lisp_ptr{false};
    return;
  }

  fio->close();
  vm.return_value[0] = Lisp_ptr{true};
}

void port_close_i(){
  port_close<InputPort, std::ifstream>("close-input-port");
}

void port_close_o(){
  port_close<OutputPort, std::ofstream>("close-output-port");
}


template<typename Fun>
void port_input_call(const char* name, Fun&& fun){
  std::vector<Lisp_ptr> args;
  stack_to_vector(vm.stack, args);

  InputPort* p;

  switch(args.size()){
  case 0:
    p = vm.find(intern(vm.symtable(), CURRENT_INPUT_PORT_SYMNAME)).get<InputPort*>();
    assert(p);
    break;
  case 1:
    p = args[0].get<InputPort*>();
    if(!p){
      throw port_type_check_failed<InputPort>(name, args[0]);
    }
    break;
  default:
    throw builtin_variadic_argcount_failed(name, 1);
  }

  vm.return_value[0] = Lisp_ptr{fun(p)};
}

void port_read(){
  port_input_call("read",
                  [](std::istream* is){ return read(*is); });
}

void port_read_char(){
  port_input_call("read-char",
                  [](std::istream* is) -> char { return is->get(); });
}

void port_peek_char(){
  port_input_call("peek-char",
                  [](std::istream* is) -> char{ return is->peek(); });
}

void port_eof_p(){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::character){
    vm.return_value[0] = Lisp_ptr{false};
    return;
  }

  vm.return_value[0] = Lisp_ptr{arg.get<char>() == EOF};
}  
  
template<typename Fun>
void port_output_call(const char* name, Fun&& fun){
  std::vector<Lisp_ptr> args;
  stack_to_vector(vm.stack, args);

  OutputPort* p;

  switch(args.size()){
  case 1:
    p = vm.find(intern(vm.symtable(), CURRENT_OUTPUT_PORT_SYMNAME)).get<OutputPort*>();
    assert(p);
    break;
  case 2:
    p = args[1].get<OutputPort*>();
    if(!p){
      throw port_type_check_failed<OutputPort>(name, args[1]);
    }
    break;
  default:
    throw builtin_variadic_argcount_failed(name, 2);
  }

  vm.return_value[0] = Lisp_ptr{fun(args[0], p)};
}

void port_write(){
  port_output_call("write",
                   [](Lisp_ptr c, std::ostream* os) -> bool{
                     print(*os, c, print_human_readable::f);
                     return true;
                   });
}

void port_display(){
  port_output_call("display",
                   [](Lisp_ptr c, std::ostream* os) -> bool{
                     print(*os, c, print_human_readable::t);
                     return true;
                   });
}

void port_write_char(){
  port_output_call("write-char",
                   [](Lisp_ptr c, std::ostream* os) -> Lisp_ptr{
                     if(c.tag() != Ptr_tag::character){
                       throw builtin_type_check_failed("write-char", Ptr_tag::character, c);
                     }

                     os->put(c.get<char>());
                     return c;
                   });
}

} //namespace

const BuiltinFunc
builtin_port[] = {
  {"input-port?", {
      port_i_p,
      {Calling::function, 1}}},
  {"output-port?", {
      port_o_p,
      {Calling::function, 1}}},

  {"open-input-file", {
      port_open_file_i,
      {Calling::function, 1}}},
  {"open-output-file", {
      port_open_file_o,
      {Calling::function, 1}}},

  {"close-input-port", {
      port_close_i,
      {Calling::function, 1}}},
  {"close-output-port", {
      port_close_o,
      {Calling::function, 1}}},
  
  {"read", {
      port_read,
      {Calling::function, 0, Variadic::t}}},
  {"read-char", {
      port_read_char,
      {Calling::function, 0, Variadic::t}}},
  {"peek-char", {
      port_peek_char,
      {Calling::function, 0, Variadic::t}}},

  {"eof-object?", {
      port_eof_p,
      {Calling::function, 1}}},

  {"write", {
      port_write,
      {Calling::function, 1, Variadic::t}}},
  {"display", {
      port_display,
      {Calling::function, 1, Variadic::t}}},
  {"write-char", {
      port_write_char,
      {Calling::function, 1, Variadic::t}}}
};

const size_t builtin_port_size = sizeof(builtin_port) / sizeof(builtin_port[0]);

void install_builtin_port_value(){
  vm.local_set(intern(vm.symtable(), CURRENT_INPUT_PORT_SYMNAME),
               &std::cin);
  vm.local_set(intern(vm.symtable(), CURRENT_OUTPUT_PORT_SYMNAME),
               &std::cout);
}


const char* builtin_port_load[] = {
  "(define newline (lambda args (apply write-char '(#\\newline) args)))",
 
  "(define (current-input-port) "CURRENT_INPUT_PORT_SYMNAME")",
  "(define (current-output-port) "CURRENT_OUTPUT_PORT_SYMNAME")",

  "(define (call-with-input-file string proc)"
  "  (let* ((port (open-input-file string))"
  "         (ret (proc port)))"
  "    (close-input-port port)"
  "    ret))",

  "(define (call-with-output-file string proc)"
  "  (let* ((port (open-output-file string))"
  "         (ret (proc port)))"
  "    (close-output-port port)"
  "    ret))",

  "(define (with-input-from-file string thunk)"
  "  (let ((old-port (current-input-port)))"
  "    (dynamic-wind (lambda () (set! "CURRENT_INPUT_PORT_SYMNAME" (open-input-file string)))"
  "                  thunk"
  "                  (lambda () (close-input-port "CURRENT_INPUT_PORT_SYMNAME")"
  "                             (set! "CURRENT_INPUT_PORT_SYMNAME" old-port)))))",

  "(define (with-output-to-file string thunk)"
  "  (let ((old-port (current-output-port)))"
  "    (dynamic-wind (lambda () (set! "CURRENT_OUTPUT_PORT_SYMNAME" (open-output-file string)))"
  "                  thunk"
  "                  (lambda () (close-output-port "CURRENT_OUTPUT_PORT_SYMNAME")"
  "                             (set! "CURRENT_OUTPUT_PORT_SYMNAME" old-port)))))",
};

const size_t builtin_port_load_size
= sizeof(builtin_port_load) / sizeof(builtin_port_load[0]);
