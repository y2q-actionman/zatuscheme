struct S{
  constexpr S() : i_(0){}
  constexpr S(int i) : i_(i){}

  union {
    int i_;
  };
};

void cause_ICE_1(){
  S s{};
}

void cause_ICE_2(){
  S s2{0};
}
