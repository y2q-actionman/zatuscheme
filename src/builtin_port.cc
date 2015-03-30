#include <fstream>
#include <istream>
#include <ostream>
#include <sstream>

#include "builtin_port.hh"
#include "lisp_ptr.hh"
#include "printer.hh"
#include "reader.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

using namespace std;

namespace zs {
namespace {

template<typename IOType, typename F_IOType>
Lisp_ptr port_open_file(ZsArgs&& args){
  check_type(Ptr_tag::string, args[0]);

  auto str = args[0].get<String*>();
  IOType* p = zs_new_with_tag<F_IOType, to_tag<IOType*>()>(*str);
  if(!p || !*p){
    throw_zs_error({}, "failed at opening file");
  }
  
  return {p};
}  

template<typename IOType, typename F_IOType>
Lisp_ptr port_close(ZsArgs&& args){
  check_type(to_tag<IOType*>(), args[0]);

  auto p = args[0].get<IOType*>();

  auto fio = dynamic_cast<F_IOType*>(p);
  if(!fio){
    print_zs_warning("warning: passed port is not associated to file");
    return Lisp_ptr{false};
  }

  fio->close();
  return Lisp_ptr{true};
}

template<typename Fun>
Lisp_ptr port_input_call(ZsArgs&& args, Fun fun){
  check_type(to_tag<InputPort*>(), args[0]);

  return Lisp_ptr{fun(args[0].get<InputPort*>())};
}

template<typename Fun>
Lisp_ptr port_output_call(ZsArgs&& args, Fun fun){
  check_type(to_tag<OutputPort*>(), args[1]);

  fun(args[0], args[1].get<OutputPort*>());
  return Lisp_ptr{true};
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
    // because a fstream which reached EOF returns 0.
    if(!is->good())
      return true;

    // We can check that 'fd points just the file's end'.
    // Though, the standard operation for getting
    // position (seekoff()) may block. 
    // How to be nonblock? SO COMPLICATED.
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
  return port_open_file<InputPort, ifstream>(move(args));
}  

Lisp_ptr port_open_file_o(ZsArgs args){
  return port_open_file<OutputPort, ofstream>(move(args));
}  

Lisp_ptr port_close_i(ZsArgs args){
  return port_close<InputPort, std::ifstream>(move(args));
}

Lisp_ptr port_close_o(ZsArgs args){
  return port_close<OutputPort, std::ofstream>(move(args));
}


Lisp_ptr port_read(ZsArgs args){
  return port_input_call(move(args),
                         [](std::istream* is){ return read(*is); });
}

Lisp_ptr port_read_char(ZsArgs args){
  return port_input_call(move(args),
                         [](std::istream* is){ return static_cast<char>(is->get()); });
}

Lisp_ptr port_peek_char(ZsArgs args){
  return port_input_call(move(args),
                         [](std::istream* is){ return static_cast<char>(is->peek()); });
}

Lisp_ptr port_eof_p(ZsArgs args){
  return Lisp_ptr{eof_object_p(args[0])};
}  


Lisp_ptr port_write(ZsArgs args){
  return port_output_call(move(args),
                          [](Lisp_ptr c, std::ostream* os){
                            print(*os, c, PrintReadable::f);
                          });
}

Lisp_ptr port_display(ZsArgs args){
  return port_output_call(move(args),
                          [](Lisp_ptr c, std::ostream* os){
                            print(*os, c, PrintReadable::t);
                          });
}

Lisp_ptr port_write_char(ZsArgs args){
  return port_output_call(move(args),
                          [](Lisp_ptr c, std::ostream* os){
                            check_type(Ptr_tag::character, c);

                            os->put(c.get<char>());
                          });
}

Lisp_ptr port_char_ready(ZsArgs args){
  return port_input_call(move(args), stream_ready);
}

// SRFI-6
Lisp_ptr port_open_input_string(ZsArgs args){
  check_type(Ptr_tag::string, args[0]);

  auto str = args[0].get<String*>();
  InputPort* p = zs_new_with_tag<istringstream, Ptr_tag::input_port>(*str);
  if(!p || !*p){
    throw_zs_error({}, "failed at opening file");
  }
  
  return {p};
}  

Lisp_ptr port_open_output_string(ZsArgs){
  OutputPort* p = zs_new_with_tag<ostringstream, Ptr_tag::output_port>();
  if(!p || !*p){
    throw_zs_error({}, "failed at opening file");
  }
  
  return {p};
}

Lisp_ptr port_get_output_string(ZsArgs args){
  check_type(Ptr_tag::output_port, args[0]);

  auto oss = dynamic_cast<ostringstream*>(args[0].get<OutputPort*>());
  if(!oss){
    throw_zs_error(args[0], "not a string port");
  }
  
  return zs_new<String>(oss->str());
}

} // namespace builtin
} // namespace zs
