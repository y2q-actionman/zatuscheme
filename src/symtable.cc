#include <utility>
#include <cassert>

#include "symtable.hh"
#include "symbol.hh"

using namespace std;

Symbol* intern(SymTable& table, const string& s){
  auto i = table.find(s);

  if(i != table.end()){
    return &(i->second);
  }else{
    // 1. Construct Key string in map.
    // 2. Let the symbol point the Key string.

    // use 'emplace' when implemented.
    auto ins_ret = table.insert(make_pair(s, Symbol{}));
    assert(ins_ret.second);

    auto ins_iter = ins_ret.first;
    ins_iter->second.rebind(&ins_iter->first);
    return &ins_iter->second;
  }
}

void unintern(SymTable& table, Symbol* s){
  table.erase(s->name());
}
