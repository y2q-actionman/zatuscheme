#ifndef UTIL_HH
#define UTIL_HH

#define EXPAND_STRINGIFY(...) STRINGIFY(__VA_ARGS__)
#define STRINGIFY(...) #__VA_ARGS__

template<typename T, typename U>
inline
T c_cast(U u){
  return (T)u;
}

#endif // UTIL_HH
