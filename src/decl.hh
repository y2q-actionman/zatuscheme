#ifndef DECL_HH
#define DECL_HH

// declares generic facilities.

// Type mapping - provided by overload
template<typename EnumType, EnumType value>
struct to_type;
// usage
//   typedef typename to_type<Number::Type, Number::Type::integer>::type HogeT;


template<typename enum_type, typename Arg>
enum_type to_tag();
// usage
//   to_tag<Token::Type, std::string>();


// provided by overload
//   template<typename Enum_type>
//   const char* stringify(Ennum_type);

#endif // DECL_HH
