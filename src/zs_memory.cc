#include <deque>
#include <array>
#include <algorithm>
#include "zs_memory.hh"

// Assumes 'operator delete' is not called frequently,

using namespace std;

namespace {

class zs_allocator {
public:
  typedef deque<void*> ArenaType;

  ArenaType& arena(Ptr_tag tag)
  { return arena_table_[static_cast<int>(tag)]; }

private:
  array<ArenaType, static_cast<size_t>(Ptr_tag::PTR_TAG_MAX)> arena_table_;
};

static zs_allocator alloc;

}

void* operator new(size_t size, Ptr_tag tag){
  auto p = operator new(size);
  alloc.arena(tag).push_front(p);
  return p;
}

void operator delete(void* p, Ptr_tag tag){
  auto arena = alloc.arena(tag);
  auto i = find(begin(arena), end(arena), p);
  if(i != end(arena)){
    arena.erase(i);
  }
}
