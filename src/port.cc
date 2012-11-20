#include <cstring>
#include "port.hh"
#include "util.hh"

/*
static void print_port_error(int eno){
  char estr[128];
  strerror_r(eno, estr, sizeof(estr));
  fprintf(zs::err, "port error: %s\n", estr);
}

Port::Port(FILE* f, const char* m)
  : f_(f), mode_(m),
    line_(0),
    string_buf_(nullptr), string_buf_len_(0){}

Port::Port(const char* name, const char* m)
  : f_(fopen(name, m)), mode_(m),
    line_(0),
    string_buf_(nullptr), string_buf_len_(0)
{
  if(!f_){
    print_port_error(errno);
  }
}

Port::Port(void* buffer, size_t len)
  : f_(fmemopen(buffer, len, "r")), mode_("r"),
    line_(0),
    string_buf_(nullptr), string_buf_len_(0)
{
  if(!f_){
    print_port_error(errno);
  }
}

Port::Port(open_output_memstream)
  : f_(nullptr), mode_("w"),
    line_(0),
    string_buf_(nullptr), string_buf_len_(0)
{
  f_ = open_memstream(&string_buf_, &string_buf_len_);
  if(!f_){
    print_port_error(errno);
  }
}

Port::Port(Port&& other)
  : f_(other.f_), mode_(other.mode_),
    line_(other.line_),
    string_buf_(other.string_buf_), string_buf_len_(other.string_buf_len_)
{
  other.clear();
}


Port::~Port(){
  close();
}


Port& Port::operator=(Port&& other){
  this->close();

  this->f_ = other.f_;
  this->mode_ = other.mode_;
  this->line_ = other.line_;
  this->string_buf_ = other.string_buf_;
  this->string_buf_len_ = other.string_buf_len_;
  
  other.clear();

  return *this;
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
      print_port_error(errno);
      ret = -1;
    }
  }

  if(string_buf_){
    free(string_buf_);
  }

  clear();
  return ret;
}

std::string Port::get_string_output(){
  if(!string_buf_) return {};

  return {string_buf_, string_buf_len_};
}

void Port::clear(){
  f_ = nullptr;
  mode_ = nullptr;
  line_ = 0;
  string_buf_ = nullptr;
  string_buf_len_ = 0;
}
*/
