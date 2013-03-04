#include <cassert>
#include <istream>
#include <ostream>
#include <fstream>
#include <sstream>
#include <iostream>
#include <memory>

#ifdef __GLIBCXX__
# include <ext/stdio_filebuf.h>
# include <ext/stdio_sync_filebuf.h>
#endif

#include <poll.h>
#include <errno.h>
#include <cstring>

#include "builtin_port.hh"
#include "lisp_ptr.hh"
#include "reader.hh"
#include "printer.hh"
#include "zs_error.hh"
#include "builtin_util.hh"

using namespace std;

namespace {

template<typename T>
zs_error port_type_check_failed(const char* func_name, Lisp_ptr p){
  return zs_error_arg1(func_name,
                       printf_string("arg is not %s!", stringify(to_tag<Ptr_tag, T*>())),
                       {p});
}

template<typename IOType, typename F_IOType>
Lisp_ptr port_open_file(const char* name){
  ZsArgs args;
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(name, Ptr_tag::string, args[0]);
  }

  unique_ptr<IOType> p{new F_IOType(str->c_str())};
  if(!*p){
    throw zs_error_arg1(name, "failed at opening file");
  }
  
  return {p.release()};
}  

template<typename IOType, typename F_IOType>
Lisp_ptr port_close(const char* name){
  ZsArgs args;
  auto p = args[0].get<IOType*>();
  if(!p){
    throw port_type_check_failed<IOType>(name, args[0]);
  }

  auto fio = dynamic_cast<F_IOType*>(p);
  if(!fio){
    cerr << "native func warning: " << name << ": passed port is not associated to file\n";
    return Lisp_ptr{false};
  }

  fio->close();
  return Lisp_ptr{true};
}

template<typename Fun>
Lisp_ptr port_input_call(const char* name, Fun&& fun){
  ZsArgs args;

  InputPort* p;

  switch(args.size()){
  case 0:
    p = vm.frame()->find(intern(vm.symtable(), CURRENT_INPUT_PORT_SYMNAME)).get<InputPort*>();
    if(!p){
      throw zs_error_arg1(name, "internal variable '"CURRENT_INPUT_PORT_SYMNAME"' is broken!");
    }
    break;
  case 1:
    p = args[0].get<InputPort*>();
    if(!p){
      throw port_type_check_failed<InputPort>(name, args[0]);
    }
    break;
  default:
    throw builtin_argcount_failed(name, 0, 1, args.size());
  }

  return Lisp_ptr{fun(p)};
}

template<typename Fun>
Lisp_ptr port_output_call(const char* name, Fun&& fun){
  ZsArgs args;

  OutputPort* p;

  switch(args.size()){
  case 1:
    p = vm.frame()->find(intern(vm.symtable(), CURRENT_OUTPUT_PORT_SYMNAME)).get<OutputPort*>();
    if(!p){
      throw zs_error_arg1(name, "internal variable '"CURRENT_OUTPUT_PORT_SYMNAME"' is broken!");
    }
    break;
  case 2:
    p = args[1].get<OutputPort*>();
    if(!p){
      throw port_type_check_failed<OutputPort>(name, args[1]);
    }
    break;
  default:
    throw builtin_argcount_failed(name, 1, 2, args.size());
  }

  return Lisp_ptr{fun(args[0], p)};
}


// for char-ready?

bool fd_ready(int fd){
  struct pollfd fds[1] = {
    { fd, POLLIN, 0}
  };

  auto ret = poll(fds, sizeof(fds)/sizeof(fds[0]), 0);
  assert(ret <= 1);

  if(ret == -1){
    auto eno = errno;
    throw zs_error_arg1("char-ready?",
                        printf_string("calling poll(2) failed: %s", strerror(eno)));
  }else if(ret == 0){
    return false;
  }else{
    return (fds[0].revents & POLLIN) != 0;
  }
}

bool stream_ready(istream* is){
  auto buf = is->rdbuf();

  if(buf->in_avail() != 0){
    // readable (> 0) or EOF (== -1)
    return true;
  }

  if(dynamic_cast<stringbuf*>(buf)){
    return false;
  }

#ifdef __GLIBCXX__
  if(dynamic_cast<__gnu_cxx::stdio_sync_filebuf<char>*>(buf)){
    return false;
  }

  if(auto sbuf = dynamic_cast<__gnu_cxx::stdio_filebuf<char>*>(buf)){
    return fd_ready(sbuf->fd());
  }
#endif

  cerr << "char-ready? is not properly implemented." << endl;
  return false;
}

} //namespace

Lisp_ptr port_open_file_i(){
  return port_open_file<InputPort, ifstream>("open-input-file");
}  

Lisp_ptr port_open_file_o(){
  return port_open_file<OutputPort, ofstream>("open-output-file");
}  

Lisp_ptr port_close_i(){
  return port_close<InputPort, std::ifstream>("close-input-port");
}

Lisp_ptr port_close_o(){
  return port_close<OutputPort, std::ofstream>("close-output-port");
}


Lisp_ptr port_read(){
  return port_input_call("read",
                         [](std::istream* is){ return read(*is); });
}

Lisp_ptr port_read_char(){
  return port_input_call("read-char",
                         [](std::istream* is) -> char { return is->get(); });
}

Lisp_ptr port_peek_char(){
  return port_input_call("peek-char",
                         [](std::istream* is) -> char{ return is->peek(); });
}

Lisp_ptr port_eof_p(){
  ZsArgs args;
  if(args[0].tag() != Ptr_tag::character){
    return Lisp_ptr{false};
  }

  return Lisp_ptr{args[0].get<char>() == EOF};
}  


Lisp_ptr port_write(){
  return port_output_call("write",
                          [](Lisp_ptr c, std::ostream* os) -> bool{
                            print(*os, c, print_human_readable::f);
                            return true;
                          });
}

Lisp_ptr port_display(){
  return port_output_call("display",
                          [](Lisp_ptr c, std::ostream* os) -> bool{
                            print(*os, c, print_human_readable::t);
                            return true;
                          });
}

Lisp_ptr port_write_char(){
  return port_output_call("write-char",
                          [](Lisp_ptr c, std::ostream* os) -> Lisp_ptr{
                            if(c.tag() != Ptr_tag::character){
                              throw builtin_type_check_failed("write-char", Ptr_tag::character, c);
                            }

                            os->put(c.get<char>());
                            return c;
                          });
}

Lisp_ptr port_char_ready(){
  return port_input_call("char-ready?", stream_ready);
}

