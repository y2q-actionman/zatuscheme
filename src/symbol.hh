#ifndef SYMBOL_HH
#define SYMBOL_HH

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
  explicit Symbol(const char* s)
    : name_(s){}
  explicit Symbol(Keyword);
  Symbol(const Symbol&) = default;
  Symbol(Symbol&&) = default;

  ~Symbol() = default;

  Symbol& operator=(const Symbol&) = default;
  Symbol& operator=(Symbol&&) = default;

  const char* name() const
  { return name_ ; }

  Keyword keyword() const;

private:
  const char* const name_;
  // TODO: add pointer to symbol table
};

#endif // SYMBOL_HH
