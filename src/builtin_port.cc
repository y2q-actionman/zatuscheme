#include <cassert>
#include <cstdio>
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

zs_error port_type_check_failed(const char* func_name, Lisp_ptr p){
  return make_zs_error("native func: %s: arg is not %s! (%s)\n",
                       func_name, stringify(Ptr_tag::port), stringify(p.tag()));
}

template<typename Fun>
void port_io_p(Fun&& fun){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::port){
    vm.return_value[0] = Lisp_ptr{false};
    return;
  }

  vm.return_value[0] = Lisp_ptr{fun(arg.get<Port*>())};
}
  
void port_i_p(){
  port_io_p([](Port* p){ return (dynamic_cast<std::istream*>(p) != nullptr); });
}

void port_o_p(){
  port_io_p([](Port* p){ return (dynamic_cast<std::ostream*>(p) != nullptr); });
}

template<typename IOType>
void port_open_file(const char* name){
  auto arg = pick_args_1();
  auto str = arg.get<String*>();
  if(!str){
    throw builtin_type_check_failed(name, Ptr_tag::string, arg);
  }

  Port* p = new IOType(str->c_str());
  if(!*p){
    throw make_zs_error("native error: %s: failed at opening file\n", name);
  }
  
  vm.return_value[0] = {p};
}  

void port_open_file_i(){
  port_open_file<ifstream>("open-input-file");
}  

void port_open_file_o(){
  port_open_file<ofstream>("open-output-file");
}  

template<typename IOType, typename F_IOType>
void port_close(const char* name){
  auto arg = pick_args_1();
  auto p = arg.get<Port*>();
  if(!p){
    throw port_type_check_failed(name, arg);
  }

  if(!dynamic_cast<IOType*>(p)){
    throw make_zs_error("native func error: %s: passed port is not expected direction\n", name);
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
  port_close<std::istream, std::ifstream>("close-input-port");
}

void port_close_o(){
  port_close<std::ostream, std::ofstream>("close-output-port");
}


template<typename Fun>
void port_input_call(const char* name, Fun&& fun){
  std::vector<Lisp_ptr> args;
  stack_to_vector(vm.stack, args);

  Port* p;

  switch(args.size()){
  case 0:
    p = vm.find(intern(vm.symtable(), CURRENT_INPUT_PORT_SYMNAME)).get<Port*>();
    assert(p);
    break;
  case 1:
    p = args[0].get<Port*>();
    if(!p){
      throw port_type_check_failed(name, args[0]);
    }
    break;
  default:
    throw builtin_variadic_argcount_failed(name, 1);
  }

  auto is = dynamic_cast<std::istream*>(p);
  if(!is){
    throw make_zs_error("native func error: %s: passed port is not input port\n", name);
  }

  vm.return_value[0] = Lisp_ptr{fun(is)};
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

  Port* p;

  switch(args.size()){
  case 1:
    p = vm.find(intern(vm.symtable(), CURRENT_OUTPUT_PORT_SYMNAME)).get<Port*>();
    assert(p);
    break;
  case 2:
    p = args[1].get<Port*>();
    if(!p){
      throw port_type_check_failed(name, args[1]);
    }
    break;
  default:
    throw builtin_variadic_argcount_failed(name, 2);
  }

  auto os = dynamic_cast<std::ostream*>(p);
  if(!os){
    throw make_zs_error("native func error: %s: passed port is not output port\n", name);
  }

  vm.return_value[0] = Lisp_ptr{fun(args[0], os)};
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
               static_cast<Port*>(&std::cin));
  vm.local_set(intern(vm.symtable(), CURRENT_OUTPUT_PORT_SYMNAME),
               static_cast<Port*>(&std::cout));
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
