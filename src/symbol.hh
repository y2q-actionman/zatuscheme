#ifndef SYMBOL_HH
#define SYMBOL_HH

#include <string>

enum class Keyword;

class Symbol{
public:
  friend class SymTable;

  const std::string& name() const
  { return name_ ; }

  Keyword keyword() const;

private:
  Symbol() = delete;
  explicit Symbol(const std::string& s)
    : name_(s){}
  Symbol(const Symbol&) = default;
  Symbol(Symbol&&) = default;

  ~Symbol() = default;

  Symbol& operator=(const Symbol&) = default;
  Symbol& operator=(Symbol&&) = default;

private:
  const std::string& name_;
  // TODO: add pointer to symbol table
};

#endif // SYMBOL_HH
