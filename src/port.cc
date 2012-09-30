#include <cstring>
#include "port.hh"
#include "util.hh"

Port* make_string_input_stream(const char* str, size_t len){
  FILE* f = fmemopen((void*)str, len, "r");
  if(!f){
    auto eno = errno;
    char estr[128];
    strerror_r(eno, estr, sizeof(estr));

    fprintf(zs::err, "port error: opening string stream failed: %s\n",
            estr);
    return nullptr;
  }

  return f;
}
