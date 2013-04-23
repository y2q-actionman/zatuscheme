#include <unordered_set>
#include <array>
#include <algorithm>
#include "zs_memory.hh"

// Assumes 'delete' is not called frequently.

using namespace std;

namespace {

typedef unordered_set<void*> ArenaType;

static array<ArenaType, static_cast<size_t>(Ptr_tag::PTR_TAG_MAX)>
arena_table;

}

void* zs_m_in(void* p, Ptr_tag tag){
  arena_table[static_cast<int>(tag)].insert(p);
  return p;
}

void zs_m_out(void* p, Ptr_tag tag){
  auto arena = arena_table[static_cast<int>(tag)];
  auto i = find(begin(arena), end(arena), p);
  if(i != end(arena)){
    arena.erase(i);
  }
}
