#include <cstdio>
#include <stdio_ext.h>
#include <cstring>

#include "builtin_port.hh"
#include "lisp_ptr.hh"
#include "port.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

namespace {

static const char current_input_port_symname[] = "current-input-port-value";
static const char current_output_port_symname[] = "current-output-port-value";

template<typename Fun>
void port_io_p(Fun&& fun){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::port){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  VM.return_value = Lisp_ptr{fun(arg.get<Port*>())};
}  
  
void port_i_p(){
  port_io_p([](Port* p){ return __freadable(p) != 0; });
}

void port_o_p(){
  port_io_p([](Port* p){ return __fwritable(p) != 0; });
}


static
void port_current(const char* name){
  pick_args<0>();
  VM.return_value = VM.find(intern(VM.symtable, name));
}  

void port_current_i(){
  port_current(current_input_port_symname);
}  
  
void port_current_o(){
  port_current(current_output_port_symname);
}  


static
void port_open_file(const char* name, const char* mode){
  auto arg = pick_args_1();
  auto str = arg.get<String*>();
  if(!str){
    builtin_type_check_failed(name, Ptr_tag::string, arg);
    return;
  }

  auto ret = fopen(str->c_str(), mode);
  if(!ret){
    auto eno = errno;
    char estr[128];
    strerror_r(eno, estr, sizeof(estr));
    fprintf(zs::err, "native func: %s: failed at opening file '%s'with '%s' (%s)\n",
            name, str->c_str(), mode, estr);

    VM.return_value = {};
    return;
  }
  
  VM.return_value = {ret};
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
    builtin_type_check_failed(name, Ptr_tag::port, arg);
    return;
  }

  if(!fun(p)){
    fprintf(zs::err, "native func warning: %s: passed port is not expected direction\n", name);
  }

  auto ret = freopen("/dev/null", "r+", p);
  if(!ret){
    auto eno = errno;
    char estr[128];
    strerror_r(eno, estr, sizeof(estr));
    fprintf(zs::err, "native func warning: %s: failed at closeing port (%s)\n", name, estr);

    VM.return_value = Lisp_ptr{false};
    return;
  }
  
  VM.return_value = Lisp_ptr{true};
}

void port_close_i(){
  port_close("close-input-port", __freadable);
}

void port_close_o(){
  port_close("close-output-port", __fwritable);
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
  {"current-input-port", {
      port_current_i,
      {Calling::function, 0}}},
  {"current-output-port", {
      port_current_o,
      {Calling::function, 0}}},

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
  
};

const size_t builtin_port_size = sizeof(builtin_port) / sizeof(builtin_port[0]);

void install_builtin_port_value(){
  VM.set(intern(VM.symtable, current_input_port_symname), {zs::in});
  VM.set(intern(VM.symtable, current_output_port_symname), {zs::out});
}
