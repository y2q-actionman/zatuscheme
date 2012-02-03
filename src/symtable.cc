#include "symtable.hh"

#include <utility>
#include <cassert>

#include "symbol.hh"

using namespace std;

Symbol* SymTable::intern(const string& s){
  auto i = table_.find(s);

  if(i != table_.end()){
    return &(i->second);
  }else{
    // 1. Construct Key string in map.
    // 2. Let the symbol point the Key string.

    // use 'emplace' when implemented.
    auto ins_ret = table_.insert(make_pair(s, Symbol{}));
    assert(ins_ret.second);

    auto ins_iter = ins_ret.first;
    ins_iter->second.rebind(&ins_iter->first);
    return &ins_iter->second;
  }
}

void SymTable::unintern(Symbol* s){
  table_.erase(s->name());
}
