#include <cassert>
#include <cstdio>
#include <cstring>

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

void port_type_check_failed(const char* func_name, Lisp_ptr p){
  builtin_type_check_failed(func_name, Ptr_tag::character, p);
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
  port_io_p([](Port* p){ return p->readable(); });
}

void port_o_p(){
  port_io_p([](Port* p){ return p->writable(); });
}


static
void port_open_file(const char* name, const char* mode){
  auto arg = pick_args_1();
  auto str = arg.get<String*>();
  if(!str){
    builtin_type_check_failed(name, Ptr_tag::string, arg);
    return;
  }

  auto p = new Port(str->c_str(), mode);
  if(!*p){
    fprintf(zs::err, "native error: %s: failed at opening file\n", name);
    vm.return_value[0] = {};
    return;
  }
  
  vm.return_value[0] = {p};
}  

void port_open_file_i(){
  port_open_file("open-input-file", "r");
}  

void port_open_file_o(){
  port_open_file("open-output-file", "w");
}  
  

template<typename Fun>
void port_close(const char* name, Fun&& fun){
  auto arg = pick_args_1();
  auto p = arg.get<Port*>();
  if(!p){
    port_type_check_failed(name, arg);
    return;
  }

  if(!fun(p)){
    fprintf(zs::err, "native func warning: %s: passed port is not expected direction\n", name);
  }

  if(!p->stream()){
    fprintf(zs::err, "native func warning: %s: passed port is already closed\n", name);
    vm.return_value[0] = Lisp_ptr{false};
    return;
  }

  if(p->close() < 0){
    fprintf(zs::err, "native func warning: %s: failed at closeing port\n", name);
    vm.return_value[0] = Lisp_ptr{false};
    return;
  }
  
  vm.return_value[0] = Lisp_ptr{true};
}

void port_close_i(){
  port_close("close-input-port", [](Port* p){ return p->readable(); });
}

void port_close_o(){
  port_close("close-output-port", [](Port* p){ return p->writable(); });
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
      port_type_check_failed(name, args[0]);
      return;
    }
    break;
  default:
    builtin_variadic_argcount_failed(name, 1);
    return;
  }

  vm.return_value[0] = Lisp_ptr{fun(p)};
}

void port_read(){
  port_input_call("read",
                  [](Port* p){ return read(p->stream()); });
}

void port_read_char(){
  port_input_call("read-char",
                  [](Port* p) -> char { return fgetc(p->stream()); });
}

void port_peek_char(){
  port_input_call("peek-char",
                  [](Port* p) -> char{
                    auto ret = fgetc(p->stream());
                    ungetc(ret, p->stream());
                    return ret;
                  });
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
      port_type_check_failed(name, args[1]);
      return;
    }
    break;
  default:
    builtin_variadic_argcount_failed(name, 2);
    return;
  }

  vm.return_value[0] = Lisp_ptr{fun(args[0], p)};
}

void port_write(){
  port_output_call("write",
                   [](Lisp_ptr c, Port* p) -> bool{
                     print(p->stream(), c, print_human_readable::f);
                     return true;
                   });
}

void port_display(){
  port_output_call("display",
                   [](Lisp_ptr c, Port* p) -> bool{
                     print(p->stream(), c, print_human_readable::t);
                     return true;
                   });
}

void port_write_char(){
  port_output_call("write-char",
                   [](Lisp_ptr c, Port* p) -> Lisp_ptr{
                     if(c.tag() != Ptr_tag::character){
                       builtin_type_check_failed("write-char", Ptr_tag::character, c);
                       return {};
                     }

                     fputc(c.get<char>(), p->stream());
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
               new Port{zs::in, "r"});
  vm.local_set(intern(vm.symtable(), CURRENT_OUTPUT_PORT_SYMNAME),
               new Port{zs::out, "w"});
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
};

const size_t builtin_port_load_size
= sizeof(builtin_port_load) / sizeof(builtin_port_load[0]);
