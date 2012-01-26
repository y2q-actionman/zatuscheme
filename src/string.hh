#ifndef STRING_HH
#define STRING_HH

#include <string>
#include <utility>

class String {
public:
  String() = default;
  explicit String(const std::string& s)
    : str_(s){}
  explicit String(std::string&& s)
    : str_(std::move(s)){}
  String(const String&) = default;
  String(String&&) = default;
  
  ~String() = default;

  String& operator=(const String&) = default;
  String& operator=(String&&) = default;

  const std::string& str() const
  { return str_; }

private:
  const std::string str_;
};

#endif
