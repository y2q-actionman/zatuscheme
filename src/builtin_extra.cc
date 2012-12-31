#include "builtin_extra.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

Lisp_ptr traditional_transformer(){
  ZsArgs args{1};

  if(auto iproc = args[0].get<IProcedure*>()){
    auto info = *iproc->info();
    info.passing = Passing::quote;
    info.returning = Returning::code;

    return new IProcedure(iproc->get(), info,
                          iproc->arg_list(), iproc->closure());
  }else if(auto nproc = args[0].get<const NProcedure*>()){
    auto info = *nproc->info();
    info.passing = Passing::quote;
    info.returning = Returning::code;

    return static_cast<const NProcedure*>(new NProcedure(nproc->get(), info));
  }else{
    throw zs_error("traditional_transformer: error: called with wrong type (%s)\n",
                   stringify(args[0].tag()));
  }
}

Lisp_ptr gensym(){
  static const string gensym_symname = {"(gensym)"};
  ZsArgs args{0};
  return {new Symbol(&gensym_symname)};
}

Lisp_ptr exit_func(){
  ZsArgs args{0};
  // cerr << "exiting..\n";
  vm.stack.clear();
  vm.code.clear();
  return {};
}
