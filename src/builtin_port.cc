#include <cassert>
#include <istream>
#include <ostream>
#include <fstream>
#include <sstream>
#include <iostream>

#include "builtin_port.hh"
#include "lisp_ptr.hh"
#include "reader.hh"
#include "printer.hh"
#include "zs_error.hh"
#include "vm.hh"
#include "zs_memory.hh"

using namespace std;

namespace {

template<typename T>
zs_error port_type_check_failed(const char* func_name, Lisp_ptr p){
  return zs_error_arg1(func_name,
                       printf_string("arg is not %s!", stringify(to_tag<Ptr_tag, T*>())),
                       {p});
}

template<typename IOType, typename F_IOType>
Lisp_ptr port_open_file(ZsArgs args, const char* name){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(name, Ptr_tag::string, args[0]);
  }

  unique_ptr<IOType, zs_deleter<IOType>> p
    {zs_new_with_tag<F_IOType, to_tag<Ptr_tag, IOType*>()>
        (str->c_str())};
  if(!*p){
    throw zs_error_arg1(name, "failed at opening file");
  }
  
  return {p.release()};
}  

template<typename IOType, typename F_IOType>
Lisp_ptr port_close(ZsArgs args, const char* name){
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
Lisp_ptr port_input_call(ZsArgs args, const char* name, Fun fun){
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
Lisp_ptr port_output_call(ZsArgs args, const char* name, Fun fun){
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


bool stream_ready(istream* is){
  // in_avail() can return 'the number of bytes available
  // without blocking'
  auto avails = is->rdbuf()->in_avail();

  if(avails > 0){
    return true; // readable
  }else if(avails < 0){
    return true; // error (EOF)
  }else{
    // These extra check is required in libstdc++,
    // because a fstream reached EOF returns 0.
    if(is->eof() || is->bad() || is->fail())
      return true;

    // We can check that 'fd points the file's end, just'.
    // Though, the operation for getting position (seekoff())
    // may block. SO COMPLICATED.
    //
    // In libstdc++, we can see whether the stream 
    // can be blocked or not by getting the type 
    // of stream by dynamic_cast.
    //
    // - If 'stringbuf', no chars shall not be available.
    //   We can immediately return 'false'.
    // - If 'stdio_filebuf<>' or 'stdio_sync_filebuf<>',
    //   the stream may std::cin. we can check via the
    //   file descripter (from 'stdio_sync_filebuf<>::fd()'
    //   or 'fileno(stdio_sync_filebuf<>.file())'.
    // - Otherwise, the type is guaranteed to 'filebuf'.
    //   We can assume the stream is seekable, so 'seekoff()'
    //   can be used.

    return false;
  }
}

} //namespace

namespace builtin {

Lisp_ptr port_open_file_i(ZsArgs args){
  return port_open_file<InputPort, ifstream>(move(args), "open-input-file");
}  

Lisp_ptr port_open_file_o(ZsArgs args){
  return port_open_file<OutputPort, ofstream>(move(args), "open-output-file");
}  

Lisp_ptr port_close_i(ZsArgs args){
  return port_close<InputPort, std::ifstream>(move(args), "close-input-port");
}

Lisp_ptr port_close_o(ZsArgs args){
  return port_close<OutputPort, std::ofstream>(move(args), "close-output-port");
}


Lisp_ptr port_read(ZsArgs args){
  return port_input_call(move(args), "read",
                         [](std::istream* is){ return read(*is); });
}

Lisp_ptr port_read_char(ZsArgs args){
  return port_input_call(move(args), "read-char",
                         [](std::istream* is) -> char { return is->get(); });
}

Lisp_ptr port_peek_char(ZsArgs args){
  return port_input_call(move(args), "peek-char",
                         [](std::istream* is) -> char{ return is->peek(); });
}

Lisp_ptr port_eof_p(ZsArgs args){
  if(args[0].tag() != Ptr_tag::character){
    return Lisp_ptr{false};
  }

  return Lisp_ptr{args[0].get<char>() == EOF};
}  


Lisp_ptr port_write(ZsArgs args){
  return port_output_call(move(args), "write",
                          [](Lisp_ptr c, std::ostream* os) -> bool{
                            print(*os, c, print_human_readable::f);
                            return true;
                          });
}

Lisp_ptr port_display(ZsArgs args){
  return port_output_call(move(args), "display",
                          [](Lisp_ptr c, std::ostream* os) -> bool{
                            print(*os, c, print_human_readable::t);
                            return true;
                          });
}

Lisp_ptr port_write_char(ZsArgs args){
  return port_output_call(move(args), "write-char",
                          [](Lisp_ptr c, std::ostream* os) -> Lisp_ptr{
                            if(c.tag() != Ptr_tag::character){
                              throw builtin_type_check_failed("write-char", Ptr_tag::character, c);
                            }

                            os->put(c.get<char>());
                            return c;
                          });
}

Lisp_ptr port_char_ready(ZsArgs args){
  return port_input_call(move(args), "char-ready?", stream_ready);
}

} // namespace builtin
