#ifndef DECL_HH
#define DECL_HH

// declares generic facilities.

// Type mapping - provided by overload
//   template<EnumType i, typename T>
//   T to_type() = delete;
// usage
//   decltype(to_type<Number::Type::integer_type>())


template<typename enum_type, typename Arg>
enum_type to_tag();
// usage
//   to_tag<Token::Type, std::string>();


// provided by overload
//   template<typename T>
//   void describe(FILE*, T);

#endif // DECL_HH
