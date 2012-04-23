#ifndef STACK_HH
#define STACK_HH

#include <vector>
#include <utility>

#include "lisp_ptr.hh"

class Stack {
public:
  Stack() = default;

  Stack(const Stack&) = default;
  Stack(Stack&&) = default;

  ~Stack() = default;

  Stack& operator=(const Stack&) = default;
  Stack& operator=(Stack&&) = default;


  Lisp_ptr find(Symbol*) const;
  Lisp_ptr set(Symbol*, Lisp_ptr);
  void push(Symbol*, Lisp_ptr);
  void pop(int);

  Lisp_ptr at(int) const;
  int size() const
  { return stack_.size(); }

private:
  typedef std::pair<Symbol*, Lisp_ptr> Entry;
  std::vector<Entry> stack_;
};

#endif 
