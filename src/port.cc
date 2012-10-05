#include <cstring>
#include "port.hh"
#include "util.hh"

Port::Port(FILE* f, const char* m)
  : f_(f), mode_(m),
    eno_(0), line_(0),
    string_buf_(nullptr), string_buf_len_(0){}

Port::Port(const char* name, const char* m)
  : f_(fopen(name, m)), mode_(m),
    eno_(0), line_(0),
    string_buf_(nullptr), string_buf_len_(0)
{
  if(!f_){
    eno_ = errno;
  }
}

Port::Port(void* buffer, size_t len)
  : f_(fmemopen(buffer, len, "r")), mode_("r"),
    eno_(0), line_(0),
    string_buf_(nullptr), string_buf_len_(0)
{
  if(!f_){
    eno_ = errno;
  }
}

Port::Port(open_output_memstream)
  : f_(nullptr), mode_("w"),
    eno_(0), line_(0),
    string_buf_(nullptr), string_buf_len_(0)
{
  f_ = open_memstream(&string_buf_, &string_buf_len_);
  if(!f_){
    eno_ = errno;
  }
}

Port::Port(Port&& other)
  : f_(other.f_), mode_(other.mode_),
    eno_(other.eno_), line_(other.line_),
    string_buf_(other.string_buf_), string_buf_len_(other.string_buf_len_)
{
  other.f_ = nullptr;
  other.mode_ = nullptr;
  other.eno_ = 0;
  other.line_ = 0;
  other.string_buf_ = nullptr;
  other.string_buf_len_ = 0;
}


Port::~Port(){
  if(close() < 0){
    print_last_error(zs::err);
  }
}


Port& Port::operator=(Port&& other){
  this->close();

  this->f_ = other.f_;
  this->mode_ = other.mode_;
  this->eno_ = other.eno_;
  this->line_ = other.line_;
  this->string_buf_ = other.string_buf_;
  this->string_buf_len_ = other.string_buf_len_;
  
  other.f_ = nullptr;
  other.mode_ = nullptr;
  other.eno_ = 0;
  other.line_ = 0;
  other.string_buf_ = nullptr;
  other.string_buf_len_ = 0;

  return *this;
}

int Port::print_last_error(FILE* out){
  char estr[128];
  strerror_r(eno_, estr, sizeof(estr));

  return fprintf(out, "port error: %s\n", estr);
}

bool Port::readable() const{
  return strspn(mode_, "r+") != 0;
}

bool Port::writable() const{
  return strspn(mode_, "wa+") != 0;
}  

int Port::close(){
  int ret = 0;

  if(f_){
    if(fclose(f_) == EOF){
      eno_ = errno;
      ret = -1;
    }
  }

  if(string_buf_){
    free(string_buf_);
  }

  return ret;
}

std::string Port::get_string_output(){
  if(!string_buf_) return {};

  return {string_buf_, string_buf_len_};
}
  


