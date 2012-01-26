#ifndef SYMBOL_HH
#define SYMBOL_HH

#include <string>

class Symbol{
public:
  enum class Keyword{
    not_keyword = 0,

    // syntactic
      else_, r_arrow, define, unquote, unquote_splicing,

    // expression
      quote, lambda, if_, set_, begin,
      cond, and_, or_, case_, let,
      let_star, letrec, do_, delay, quasiquote,

      MAX
      };

  Symbol() = delete;
  explicit Symbol(const std::string&);
  explicit Symbol(const char*);
  explicit Symbol(Keyword);
  Symbol(const Symbol&) = default;
  Symbol(Symbol&&) = default;

  ~Symbol() = default;

  Symbol& operator=(const Symbol&) = default;
  Symbol& operator=(Symbol&&) = default;

  const char* name() const
  { return name_ ; }
  Keyword keyword() const
  { return k_; }

private:
  const char* const name_;
  Keyword k_;
};

Symbol::Keyword to_keyword(const char*);
const char* stringify(Symbol::Keyword);

#endif // SYMBOL_HH
