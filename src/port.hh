#ifndef PORT_HH
#define PORT_HH

/*
#include <cstdio>
#include <string>
#include "lisp_ptr.hh"

class Port {
public:
  enum class open_output_memstream {t};

  Port(FILE*, const char* mode); // opened stream
  Port(const char* name, const char* mode); // file open
  Port(void*, size_t); // buffer open (read)
  Port(open_output_memstream); // buffer open (write)

  Port(const Port&) = delete;
  Port(Port&&);

  ~Port();

  Port& operator=(const Port&) = delete;
  Port& operator=(Port&&);


  explicit operator bool(){ return f_ != nullptr; }

  FILE* stream() const{ return f_; }

  bool readable() const;
  bool writable() const;

  int add_line(unsigned i){ return (line_ += i); }

  int close();

  std::string get_string_output();

  
private:
  void clear();

  FILE* f_;
  const char* mode_;
  unsigned line_;
  char* string_buf_;
  size_t string_buf_len_;
};
*/

#endif // PORT_HH
