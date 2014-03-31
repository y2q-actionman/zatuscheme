#include <fstream>
#include <iostream>

#include "builtin_extra.hh"
#include "cons_util.hh"
#include "env.hh"
#include "eval.hh"
#include "lisp_ptr.hh"
#include "printer.hh"
#include "procedure.hh"
#include "reader.hh"
#include "s_closure.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

#ifdef _POSIX_C_SOURCE
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#endif

using namespace std;
using namespace proc_flag;

namespace builtin {

// used for interacting between 'exit' and 'hard-repl'
static bool hard_repl_continue = true;

Lisp_ptr transcript_on(ZsArgs){
  dump_mode = true;
  return Lisp_ptr{true};
}

Lisp_ptr transcript_off(ZsArgs){
  dump_mode = false;
  return Lisp_ptr{true};
}

Lisp_ptr traditional_transformer(ZsArgs args){
  check_type(Ptr_tag::i_procedure, args[0]);

  auto iproc = args[0].get<IProcedure*>();
  auto info = *iproc->info();
  info.passing = Passing::quote;
  info.returning = Returning::code;

  return zs_new<IProcedure>(iproc->get(), info,
                            iproc->arg_list(), iproc->closure());
}

Lisp_ptr gensym(ZsArgs){
  static const string gensym_symname = {"(gensym)"};
  return {zs_new<Symbol>(&gensym_symname)};
}

Lisp_ptr sc_macro_transformer(ZsArgs args){
  check_type(Ptr_tag::i_procedure, args[0]);

  auto iproc = args[0].get<IProcedure*>();
  auto info = *iproc->info();

  info.passing = Passing::whole;
  info.returning = Returning::code;
  info.leaving = Leaving::after_returning_op;

  return zs_new<IProcedure>(iproc->get(), info,
                            iproc->arg_list(),
                            iproc->closure()->fork());
}

Lisp_ptr make_syntactic_closure(ZsArgs args){
  check_type(Ptr_tag::env, args[0]);
  check_type(Ptr_tag::cons, args[1]);

  return zs_new<SyntacticClosure>(args[0].get<Env*>(), args[1], args[2]);
}

Lisp_ptr internal_current_environment(ZsArgs){
  // assumes that native funcions don't introduce a new frame.
  return vm.frame;
}

Lisp_ptr identifierp(ZsArgs args){
  return Lisp_ptr{identifierp(args[0])};
}

Lisp_ptr identifier_eq(ZsArgs args){
  check_type(Ptr_tag::env, args[0]);
  check_identifier_type(args[1]);
  check_type(Ptr_tag::env, args[2]);
  check_identifier_type(args[3]);

  return 
    Lisp_ptr{identifier_eq(args[0].get<Env*>(), args[1],
                           args[2].get<Env*>(), args[3])};
}

Lisp_ptr internal_make_empty_environment(ZsArgs){
  return zs_new<Env>(nullptr);
}

Lisp_ptr internal_push_exception_handler(ZsArgs args){
  check_procedure_type(args[0]);

  vm.exception_handler.push_back(args[0]);
  return {};
}

Lisp_ptr internal_pop_exception_handler(ZsArgs){
  if(!vm.exception_handler.empty()){
    vm.exception_handler.pop_back();
  }
  return {};
}

Lisp_ptr raise(ZsArgs args){
  vm.code.push_back(vm_op_raise);
  return args[0];
}

Lisp_ptr exit(ZsArgs args){
  auto ret = (args.size() > 0) ? args[0] : Lisp_ptr{true};
  args.cleanup();
  vm.stack.clear();
  vm.code.clear();
  hard_repl_continue = false;
  return ret;
}

Lisp_ptr hard_repl(ZsArgs args){
  args.cleanup();
  vm.stack.clear();
  vm.code.clear();

  while(hard_repl_continue){
    cout << ">> " << flush;
    vm.code.push_back(read(cin));
    start_evaluation();
    for(auto& p : vm.return_value){
      print(cout, p);
      cout << endl;
    }
  }

  return {};
}

Lisp_ptr tmp_file(ZsArgs){
#ifdef _POSIX_C_SOURCE

#ifdef P_tmpdir
# define ZS_TMPDIR P_tmpdir
#else
# define ZS_TMPDIR "/tmp"
#endif

#define ZS_TMPFILE ZS_TMPDIR"/zs_temp_file_XXXXXX"

  struct FdHolder{
    int fd;
    char name[sizeof(ZS_TMPFILE)];

    FdHolder() : fd(-1), name(){
      strcpy(name, ZS_TMPFILE);

      fd = mkstemp(name);
      if(fd == -1){
        auto eno = errno;
        throw_zs_error({}, "mkstemp(3) error: %s", strerror(eno));
      }
    }

    ~FdHolder(){
      if(close(fd) == -1){
        auto eno = errno;
        print_zs_warning("close(2) error: %s", strerror(eno));
      }

      if(unlink(name) == -1){
        auto eno = errno;
        print_zs_warning("unlink(2) error: %s", strerror(eno));
      }
    }
  };

  FdHolder fdh;

  InputPort* i_port = zs_new_with_tag<ifstream, Ptr_tag::input_port>(fdh.name);
  if(!i_port || !*i_port){
    throw_zs_error({}, "failed at opening file for input");
  }
  
  OutputPort* o_port = zs_new_with_tag<ofstream, Ptr_tag::output_port>(fdh.name);
  if(!o_port || !*o_port){
    throw_zs_error({}, "failed at opening file for output");
  }
  
  return make_cons_list({i_port, o_port});
#else
  throw_zs_error({}, "tmp-file is not supported");
#endif
}

} // namespace builtin
