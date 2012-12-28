#include "zs.hh"
#include "test_util.hh"

void check(const char* input, const char* expect){
  result &= read_eval_print_test(input, expect);
}

void check_undef(const char* input){
  result &= !eval_text(input);
}

int main(){
  zs_init();

  check("(pair? '(a . b))", "#t");
  check("(pair? '(a b c))", "#t");
  check("(pair? '())", "#f");
  check("(pair? #(a b))", "#f");

  check("(cons 'a '())", "(a)");
  check("(cons '(a) '(b c d))", "((a) b c d)");
  check("(cons \"a\" '(b c))", "(\"a\" b c)");
  check("(cons 'a 3)", "(a . 3)");
  check("(cons '(a  b) 'c)", "((a b) . c)");
  
  check("(car '(a b c))", "a");
  check("(car '((a) b c d))", "(a)");
  check("(car '(1 . 2))", "1");
  with_expect_error([]() -> void {
      check_undef("(car '())");
    });
  
  check("(cdr '((a) b c d))", "(b c d)");
  check("(cdr '(1 . 2))", "2");
  with_expect_error([]() -> void {
      check_undef("(cdr '())");
    });

  eval_text("(define tmp (cons 'a 'b))");
  check("tmp", "(a . b)");
  eval_text("(set-car! tmp 1)");
  check("tmp", "(1 . b)");
  eval_text("(set-cdr! tmp 2)");
  check("tmp", "(1 . 2)");

  
  check("(list? '(a b c))", "#t");
  check("(list? '())", "#t");
  check("(list? '(a . b))", "#f");
  check("(let ((x (list 'a))) (set-cdr! x x) (list? x))", "#f");


  check("(list 1)", "(1)");
  check("(list 1 2)", "(1 2)");
  check("(list 1 2 3)", "(1 2 3)");
  check("(list 'a (+ 4 3) 'c)", "(a 7 c)");
  check("(list)", "()");

  check("(list* 1)", "1");
  check("(list* 1 2)", "(1 . 2)");
  check("(list* 1 2 3)", "(1 2 . 3)");


  check("(length '(a b c))", "3");
  check("(length '(a (b) (c d e)))", "3");
  check("(length '())", "0");

  check("(append '(1 2 3))", "(1 2 3)");
  check("(append '(1 2 3) '(4 5 6))", "(1 2 3 4 5 6)");
  check("(append '(1 2 3) '())", "(1 2 3)");
  check("(append '() '(1 2 3))", "(1 2 3)");
  check("(append '(1 2 3) 'a)", "(1 2 3 . a)");
  check("(append '(x) '(y))", "(x y)");
  check("(append '(a) '(b c d))", "(a b c d)");
  check("(append '(a (b)) '((c)))", "(a (b) (c))");
  check("(append '(a b) '(c . d))", "(a b c . d)");
  check("(append '() 'a)", "a");
  check("(append '() '() '() '() 'a)", "a");

  check("(reverse '(a b c))", "(c b a)");
  check("(reverse '(a (b c) d (e (f))))", "((e (f)) d (b c) a)");

  check("(list-tail '(a b c d) 2)", "(c d)");

  check("(list-ref '(a b c d) 2)", "c");


  eval_text("(define c2r-test '((a . b) . (c . d)))");
  check("(caar c2r-test)", "a");
  check("(cdar c2r-test)", "b");
  check("(cadr c2r-test)", "c");
  check("(cddr c2r-test)", "d");

  eval_text("(define c3r-test '(((a . b) . (c . d)) . ((e . f) . (g . h))))");
  check("(caaar c3r-test)", "a");
  check("(cdaar c3r-test)", "b");
  check("(cadar c3r-test)", "c");
  check("(cddar c3r-test)", "d");
  check("(caadr c3r-test)", "e");
  check("(cdadr c3r-test)", "f");
  check("(caddr c3r-test)", "g");
  check("(cdddr c3r-test)", "h");

  eval_text("(define c4r-test"
            "'((((aa . ad) . (ba . bd)) . ((ca . cd) . (da . dd)))"
            ". (((ea . ed) . (fa . fd)) . ((ga . gd) . (ha . hd)))))");
  check("(caaaar c4r-test)", "aa");
  check("(cdaaar c4r-test)", "ad");
  check("(cadaar c4r-test)", "ba");
  check("(cddaar c4r-test)", "bd");
  check("(caadar c4r-test)", "ca");
  check("(cdadar c4r-test)", "cd");
  check("(caddar c4r-test)", "da");
  check("(cdddar c4r-test)", "dd");
  check("(caaadr c4r-test)", "ea");
  check("(cdaadr c4r-test)", "ed");
  check("(cadadr c4r-test)", "fa");
  check("(cddadr c4r-test)", "fd");
  check("(caaddr c4r-test)", "ga");
  check("(cdaddr c4r-test)", "gd");
  check("(cadddr c4r-test)", "ha");
  check("(cddddr c4r-test)", "hd");


  check("(memq 'a '(a b c))", "(a b c)");
  check("(memq 'b '(a b c))", "(b c)");
  check("(memq 'a '(b c d))", "#f");
  check("(memq (list 'a) '(b (a) c))", "#f");
  check("(member (list 'a) '(b (a) c))", "((a) c)");
  check("(memq 101 '(100 101 102))", "#f"); // unspecified
  check("(memv 101 '(100 101 102))", "(101 102)");


  eval_text("(define e '((a 1) (b 2) (c 3)))");
  check("(assq 'a e)", "(a 1)");
  check("(assq 'b e)", "(b 2)");
  check("(assq 'd e)", "#f");
  check("(assq (list 'a) '(((a)) ((b)) ((c))))", "#f");
  check("(assoc (list 'a) '(((a)) ((b)) ((c))))", "((a))");
  check("(assq 5 '((2 3) (5 7) (11 13)))", "#f"); // unspecified
  check("(assv 5 '((2 3) (5 7) (11 13)))", "(5 7)");

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
