#include "function.hh"
#include "util.hh"

const char* stringify(Function::Type t){
  switch(t){
  case Function::Type::interpreted:
    return "interpreted";
  case Function::Type::native:
    return "native";
  default:
    return "(unknown function type)";
  }
}
