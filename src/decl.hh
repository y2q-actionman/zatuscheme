#ifndef DECL_HH
#define DECL_HH

// defines generic facilities.

// Type mapping
template<typename T>
struct to_type;
// usage
//   to_type<Number::Type>::get<Number::Type::integer_type>::type

template<typename enum_type, typename Arg>
enum_type to_tag();
// usage
//   to_tag<Token::Type, std::string>();


// provided by overload
//   template<typename T>
//   void describe(FILE*, T);

#endif // DECL_HH
