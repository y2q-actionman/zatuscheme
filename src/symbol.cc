#include <cassert>
#include <ostream>
#include <utility>

#include "symbol.hh"

using namespace std;

namespace zs {

constexpr inline
Symbol::Symbol()
  : name_(){} // assumes this is immediately set by intern()

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
    ins_iter->second.name_ = &ins_iter->first;
    return &ins_iter->second;
  }
}

void unintern(SymTable& table, Symbol* s){
  table.erase(s->name());
  s->name_ = nullptr;
}

std::ostream& operator<<(std::ostream& f, const SymTable& st){
  for(auto s : st){
    f << s.first.c_str() << ' ' << reinterpret_cast<void*>(&s.second) << '\n';
  }
  return f;
}

} // namespace zs
