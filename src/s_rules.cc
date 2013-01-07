#include "s_rules.hh"
#include "s_closure.hh"
#include "cons_util.hh"

using namespace std;

namespace Procedure{

constexpr ProcInfo SyntaxRules::sr_procinfo;

SyntaxRules::SyntaxRules(Env* e, Lisp_ptr lits, Lisp_ptr rules)
  : env_(e), literals_(), rules_(){
  for(auto i : lits){
    literals_.push_back(identifier_symbol(i));
  }
  literals_.shrink_to_fit();

  for(auto i : rules){
    bind_cons_list_strict
      (i,
       [&](Lisp_ptr pat, Lisp_ptr tmpl){
        rules_.push_back({pat, tmpl});
      });
  }
  rules_.shrink_to_fit();
}

SyntaxRules::~SyntaxRules() = default;

} // Procedure
